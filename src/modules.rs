use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use rand::random;

use crate::ast::{AST, ImportName, ModulePath, Node};
use crate::parser::Parser;
use crate::transpilers::python_transpiler::Transpiler;

#[derive(Debug, Clone)]
pub struct ModuleError {
    pub message: String,
}

impl ModuleError {
    fn new<T: Into<String>>(message: T) -> Self {
        ModuleError { message: message.into() }
    }
}

impl std::fmt::Display for ModuleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for ModuleError {}

#[derive(Debug, Clone)]
pub struct CompiledProject {
    pub output_root: PathBuf,
    pub entry_module_name: String,
}

#[derive(Debug, Clone)]
struct ModuleRecord {
    id: String,
    python_name: String,
    output_path: PathBuf,
    is_package: bool,
    package_segments: Vec<String>,
    ast: AST,
    exports: BTreeMap<String, ExportKind>,
}

#[derive(Debug, Clone)]
enum ExportKind {
    Local,
    Module,
    ReExport,
}

#[derive(Debug, Clone)]
enum UseTarget {
    Item { module_id: String, export_name: String },
}

#[derive(Debug, Clone)]
struct ResolvedUse {
    local_name: String,
    target: UseTarget,
}

#[derive(Debug, Clone)]
struct ImportBinding {
    exporter_alias: String,
    export_name: String,
}

#[derive(Debug, Clone, Default)]
struct ResolvedNodeEffect {
    import_line: Option<String>,
    extra_lines: Vec<String>,
    bindings: Vec<(String, ImportBinding)>,
}

#[derive(Debug, Clone)]
struct ResolvedModulePath {
    module_segments: Vec<String>,
    tail: Vec<String>,
}

struct ProjectBuilder {
    root: PathBuf,
    explicit_root: bool,
    modules: BTreeMap<String, ModuleRecord>,
    module_order: Vec<String>,
    loading_stack: Vec<String>,
}

pub fn build_project(
    entry_file: PathBuf,
    project_root: Option<PathBuf>,
    output_root: &Path,
) -> Result<CompiledProject, ModuleError> {
    let entry_file = canonicalize_file(&entry_file)?;
    let explicit_root = project_root.is_some();
    let root = match project_root {
        Some(root) => canonicalize_dir(&root)?,
        None => canonicalize_dir(entry_file.parent().unwrap_or_else(|| Path::new(".")))?,
    };

    if !entry_file.starts_with(&root) {
        return Err(ModuleError::new(format!(
            "Entry file '{}' is not inside project root '{}'.",
            entry_file.display(),
            root.display()
        )));
    }

    let mut builder = ProjectBuilder {
        root,
        explicit_root,
        modules: BTreeMap::new(),
        module_order: Vec::new(),
        loading_stack: Vec::new(),
    };

    let entry_module_id = builder.load_module_file(&entry_file)?;
    builder.finalize_exports()?;
    builder.emit_project(output_root)?;

    let entry_module = builder
        .modules
        .get(&entry_module_id)
        .ok_or_else(|| ModuleError::new("Entry module disappeared during build."))?;

    if entry_module.python_name.is_empty() {
        return Err(ModuleError::new("Project root package cannot be used as an executable entrypoint."));
    }

    Ok(CompiledProject {
        output_root: output_root.to_path_buf(),
        entry_module_name: entry_module.python_name.clone(),
    })
}

pub fn run_project(entry_file: PathBuf, project_root: Option<PathBuf>) -> Result<std::process::Output, ModuleError> {
    let output_root = std::env::temp_dir().join(format!("lang_project_{}", random::<u64>()));
    fs::create_dir_all(&output_root).map_err(|error| {
        ModuleError::new(format!("Failed to create project output directory: {error}"))
    })?;

    let compiled = build_project(entry_file, project_root, &output_root)?;
    let output_root_str = output_root.to_string_lossy().replace('\\', "/");
    let command = format!(
        "import sys; sys.path.insert(0, r'{}'); import {}",
        output_root_str,
        compiled.entry_module_name
    );

    let output = Command::new("py")
        .arg("-c")
        .arg(&command)
        .output()
        .or_else(|_| Command::new("python").arg("-c").arg(&command).output())
        .map_err(|error| ModuleError::new(format!("Failed to run generated Python project: {error}")))?;

    Ok(output)
}

impl ProjectBuilder {
    fn load_module_file(&mut self, source_path: &Path) -> Result<String, ModuleError> {
        let source_path = canonicalize_file(source_path)?;
        let module_id = module_id_from_path(&self.root, &source_path)?;

        if self.modules.contains_key(&module_id) {
            return Ok(module_id);
        }

        if let Some(index) = self.loading_stack.iter().position(|candidate| candidate == &module_id) {
            let mut chain = self.loading_stack[index..].to_vec();
            chain.push(module_id.clone());
            return Err(ModuleError::new(format!(
                "Import cycle detected: {}",
                chain.join(" -> ")
            )));
        }

        self.loading_stack.push(module_id.clone());

        let source = fs::read_to_string(&source_path).map_err(|error| {
            ModuleError::new(format!("Failed to read module '{}': {error}", source_path.display()))
        })?;
        let mut parser = Parser::new(source);
        parser.parse();
        let ast = parser.ast;

        let module_path = module_segments_from_id(&module_id);
        let is_package = source_path
            .file_name()
            .and_then(|name| name.to_str())
            .map(|name| name == "mod.lang")
            .unwrap_or(false);
        let package_segments = if is_package {
            module_path.clone()
        } else if module_path.is_empty() {
            Vec::new()
        } else {
            module_path[..module_path.len() - 1].to_vec()
        };

        let output_path = output_path_for_module(&module_path, is_package);
        let python_name = module_id.clone();

        let record = ModuleRecord {
            id: module_id.clone(),
            python_name,
            output_path,
            is_package,
            package_segments,
            ast,
            exports: BTreeMap::new(),
        };

        let nodes = record.ast.get_scope();

        for node in nodes.iter() {
            match node {
                Node::ModuleDecl { name, .. } => {
                    let child_path = self.resolve_child_module_path(&record, name)?;
                    self.load_module_file(&child_path)?;
                }
                Node::ImportStmt { path, .. } | Node::FromImport { path, .. } => {
                    let target = self.resolve_module_path(&record, path)?;
                    let target_path = self.locate_module_file(&target)?;
                    self.load_module_file(&target_path)?;
                }
                Node::UseDecl { path, .. } => {
                    let target = self.resolve_use_target_path(&record, path)?;
                    let target_path = self.locate_module_file(&target.module_segments)?;
                    self.load_module_file(&target_path)?;
                }
                _ => {}
            }
        }

        self.loading_stack.pop();
        self.module_order.push(module_id.clone());
        self.modules.insert(module_id.clone(), record);
        Ok(module_id)
    }

    fn finalize_exports(&mut self) -> Result<(), ModuleError> {
        let module_ids = self.module_order.clone();

        for module_id in module_ids.iter() {
            let base_exports = {
                let module = self.modules.get(module_id).unwrap();
                self.collect_base_exports(module)?
            };
            self.modules.get_mut(module_id).unwrap().exports = base_exports;
        }

        for module_id in module_ids.iter() {
            let use_exports = {
                let module = self.modules.get(module_id).unwrap();
                self.collect_public_use_exports(module)?
            };

            let exports = &mut self.modules.get_mut(module_id).unwrap().exports;
            for (name, export) in use_exports {
                if exports.contains_key(&name) {
                    return Err(ModuleError::new(format!(
                        "Module '{}' exports '{}' more than once.",
                        module_id,
                        name
                    )));
                }
                exports.insert(name, export);
            }
        }

        Ok(())
    }

    fn emit_project(&self, output_root: &Path) -> Result<(), ModuleError> {
        fs::create_dir_all(output_root).map_err(|error| {
            ModuleError::new(format!("Failed to create output root '{}': {error}", output_root.display()))
        })?;

        for module_id in self.module_order.iter() {
            let module = self.modules.get(module_id).unwrap();
            let transformed_ast = self.transform_module_ast(module)?;
            let output_path = output_root.join(&module.output_path);

            if let Some(parent) = output_path.parent() {
                fs::create_dir_all(parent).map_err(|error| {
                    ModuleError::new(format!(
                        "Failed to create output directory '{}': {error}",
                        parent.display()
                    ))
                })?;
            }

            let mut transpiler = Transpiler::new();
            let code = transpiler.transpile(&transformed_ast);
            fs::write(&output_path, code).map_err(|error| {
                ModuleError::new(format!(
                    "Failed to write generated Python file '{}': {error}",
                    output_path.display()
                ))
            })?;

            self.ensure_parent_packages(output_root, &module.output_path)?;
        }

        Ok(())
    }

    fn ensure_parent_packages(&self, output_root: &Path, output_path: &Path) -> Result<(), ModuleError> {
        let mut current = output_root.join(output_path);
        while let Some(parent) = current.parent() {
            if parent == output_root {
                break;
            }

            let init_path = parent.join("__init__.py");
            if !init_path.exists() {
                fs::write(&init_path, "").map_err(|error| {
                    ModuleError::new(format!(
                        "Failed to write generated package file '{}': {error}",
                        init_path.display()
                    ))
                })?;
            }

            current = parent.to_path_buf();
        }

        Ok(())
    }

    fn collect_base_exports(&self, module: &ModuleRecord) -> Result<BTreeMap<String, ExportKind>, ModuleError> {
        let mut exports = BTreeMap::new();

        for node in module.ast.get_scope() {
            match node {
                Node::FunctionDef { name, public, .. }
                | Node::StructDef { name, public, .. }
                | Node::TraitDef { name, public, .. }
                | Node::BindingDef { name, public, .. } => {
                    if public {
                        insert_export(&mut exports, &module.id, &name, ExportKind::Local)?;
                    }
                }
                Node::ModuleDecl { name, public } => {
                    if public {
                        let child_path = self.resolve_child_module_path(module, &name)?;
                        let _child_id = module_id_from_path(&self.root, &child_path)?;
                        insert_export(
                            &mut exports,
                            &module.id,
                            &name,
                            ExportKind::Module,
                        )?;
                    }
                }
                _ => {}
            }
        }

        Ok(exports)
    }

    fn collect_public_use_exports(
        &self,
        module: &ModuleRecord,
    ) -> Result<Vec<(String, ExportKind)>, ModuleError> {
        let mut exports = Vec::new();

        for node in module.ast.get_scope() {
            let Node::UseDecl { path, alias, public } = node else {
                continue;
            };

            if !public {
                continue;
            }

            let resolved = self.resolve_use_target(module, &path)?;
            let UseTarget::Item { module_id: _, export_name: _ } = resolved.target;

            exports.push((
                alias.clone().unwrap_or(resolved.local_name),
                ExportKind::ReExport,
            ));
        }

        Ok(exports)
    }

    fn transform_module_ast(&self, module: &ModuleRecord) -> Result<AST, ModuleError> {
        let nodes = module.ast.get_scope();
        let mut alias_counter = 0usize;
        let mut alias_by_module = BTreeMap::new();
        let mut effects = BTreeMap::new();
        let mut all_bindings = BTreeMap::new();

        for (index, node) in nodes.iter().enumerate() {
            let effect = self.resolve_node_effect(
                module,
                node,
                &mut alias_counter,
                &mut alias_by_module,
                &mut all_bindings,
            )?;

            if effect.import_line.is_some() || !effect.extra_lines.is_empty() || !effect.bindings.is_empty() {
                effects.insert(index, effect);
            }
        }

        let mut emitted_imports = BTreeSet::new();
        let mut active_bindings = BTreeMap::new();
        let mut current_locals = HashSet::new();
        let mut transformed = Vec::new();

        for (index, node) in nodes.iter().enumerate() {
            if let Some(effect) = effects.get(&index) {
                if let Some(import_line) = &effect.import_line {
                    if emitted_imports.insert(import_line.clone()) {
                        transformed.push(Node::Python(import_line.clone()));
                    }
                }

                for line in effect.extra_lines.iter() {
                    transformed.push(Node::Python(line.clone()));
                }

                for (name, binding) in effect.bindings.iter() {
                    active_bindings.insert(name.clone(), binding.clone());
                }

                match node {
                    Node::ImportStmt { alias, path } => {
                        if let Some(name) = alias.clone().or_else(|| path.last_segment().cloned()) {
                            current_locals.insert(name);
                        }
                    }
                    Node::ModuleDecl { name, .. } => {
                        current_locals.insert(name.clone());
                    }
                    _ => {}
                }

                continue;
            }

            transformed.push(self.rewrite_node(node, &active_bindings, &all_bindings, &current_locals));

            match node {
                Node::BindingDef { name, .. } => {
                    if !all_bindings.contains_key(name) {
                        current_locals.insert(name.clone());
                    }
                }
                Node::FunctionDef { name, .. }
                | Node::StructDef { name, .. }
                | Node::TraitDef { name, .. } => {
                    current_locals.insert(name.clone());
                }
                _ => {}
            }
        }

        Ok(AST { scopes: vec![transformed] })
    }

    fn resolve_node_effect(
        &self,
        module: &ModuleRecord,
        node: &Node,
        alias_counter: &mut usize,
        alias_by_module: &mut BTreeMap<String, String>,
        all_bindings: &mut BTreeMap<String, ImportBinding>,
    ) -> Result<ResolvedNodeEffect, ModuleError> {
        match node {
            Node::ImportStmt { path, alias } => {
                let target = self.resolve_module_path(module, path)?;
                let target_id = target.join(".");
                let target_module = self.module(&target_id)?;
                let alias = alias
                    .clone()
                    .or_else(|| path.last_segment().cloned())
                    .ok_or_else(|| ModuleError::new("import requires a target module name."))?;

                Ok(ResolvedNodeEffect {
                    import_line: Some(format!("import {} as {}", target_module.python_name, alias)),
                    ..ResolvedNodeEffect::default()
                })
            }
            Node::ModuleDecl { name, .. } => {
                let child_path = self.resolve_child_module_path(module, name)?;
                let child_id = module_id_from_path(&self.root, &child_path)?;
                let child_module = self.module(&child_id)?;

                Ok(ResolvedNodeEffect {
                    import_line: Some(format!("import {} as {}", child_module.python_name, name)),
                    ..ResolvedNodeEffect::default()
                })
            }
            Node::FromImport { path, names, wildcard } => {
                let target = self.resolve_module_path(module, path)?;
                let target_id = target.join(".");
                let target_module = self.module(&target_id)?;
                let hidden_alias = self.hidden_alias_for(alias_counter, alias_by_module, &target_id);
                let mut bindings = Vec::new();

                if *wildcard {
                    for export_name in target_module.exports.keys() {
                        let binding = ImportBinding {
                            exporter_alias: hidden_alias.clone(),
                            export_name: export_name.clone(),
                        };
                        ensure_unique_binding(all_bindings, module, export_name, &binding)?;
                        bindings.push((export_name.clone(), binding));
                    }
                } else {
                    for ImportName { name, alias } in names.iter() {
                        if !target_module.exports.contains_key(name) {
                            return Err(ModuleError::new(format!(
                                "Module '{}' does not publicly export '{}'.",
                                target_id,
                                name
                            )));
                        }

                        let local_name = alias.clone().unwrap_or_else(|| name.clone());
                        let binding = ImportBinding {
                            exporter_alias: hidden_alias.clone(),
                            export_name: name.clone(),
                        };
                        ensure_unique_binding(all_bindings, module, &local_name, &binding)?;
                        bindings.push((local_name, binding));
                    }
                }

                Ok(ResolvedNodeEffect {
                    import_line: Some(format!("import {} as {}", target_module.python_name, hidden_alias)),
                    bindings,
                    ..ResolvedNodeEffect::default()
                })
            }
            Node::UseDecl { path, alias, public } => {
                let resolved = self.resolve_use_target(module, path)?;
                let UseTarget::Item { module_id, export_name } = resolved.target;
                let target_module = self.module(&module_id)?;
                let hidden_alias = self.hidden_alias_for(alias_counter, alias_by_module, &module_id);
                let local_name = alias.clone().unwrap_or(resolved.local_name);
                let binding = ImportBinding {
                    exporter_alias: hidden_alias.clone(),
                    export_name: export_name.clone(),
                };

                ensure_unique_binding(all_bindings, module, &local_name, &binding)?;

                let mut extra_lines = Vec::new();
                if *public {
                    extra_lines.push(format!("{} = {}.{}", local_name, hidden_alias, export_name));
                }

                Ok(ResolvedNodeEffect {
                    import_line: Some(format!("import {} as {}", target_module.python_name, hidden_alias)),
                    extra_lines,
                    bindings: vec![(local_name, binding)],
                })
            }
            _ => Ok(ResolvedNodeEffect::default()),
        }
    }

    fn resolve_child_module_path(&self, module: &ModuleRecord, child_name: &str) -> Result<PathBuf, ModuleError> {
        if !module.is_package {
            return Err(ModuleError::new(format!(
                "Module '{}' cannot declare child module '{}'; only mod.lang files can use 'mod'.",
                module.id,
                child_name
            )));
        }

        let mut segments = module.package_segments.clone();
        segments.push(child_name.to_string());
        self.locate_module_file(&segments)
    }

    fn resolve_module_path(&self, module: &ModuleRecord, path: &ModulePath) -> Result<Vec<String>, ModuleError> {
        let segments = self.absolute_segments(module, path)?;
        self.locate_module_file(&segments)?;
        Ok(segments)
    }

    fn resolve_use_target_path(&self, module: &ModuleRecord, path: &ModulePath) -> Result<ResolvedModulePath, ModuleError> {
        let segments = self.absolute_segments(module, path)?;

        for split in (1..=segments.len()).rev() {
            let module_segments = segments[..split].to_vec();
            if self.locate_module_file(&module_segments).is_ok() {
                return Ok(ResolvedModulePath {
                    module_segments,
                    tail: segments[split..].to_vec(),
                });
            }
        }

        Err(ModuleError::new(format!(
            "Could not resolve use path '{}' from module '{}'.",
            format_module_path(path),
            module.id
        )))
    }

    fn resolve_use_target(&self, module: &ModuleRecord, path: &ModulePath) -> Result<ResolvedUse, ModuleError> {
        let resolved = self.resolve_use_target_path(module, path)?;

        if resolved.tail.len() != 1 {
            return Err(ModuleError::new(format!(
                "use path '{}' must resolve to a single exported name.",
                format_module_path(path)
            )));
        }

        let export_name = resolved.tail[0].clone();
        let exporter = self.module(&resolved.module_segments.join("."))?;
        if !exporter.exports.contains_key(&export_name) {
            return Err(ModuleError::new(format!(
                "Module '{}' does not publicly export '{}'.",
                exporter.id,
                export_name
            )));
        }

        Ok(ResolvedUse {
            local_name: export_name.clone(),
            target: UseTarget::Item {
                module_id: exporter.id.clone(),
                export_name,
            },
        })
    }

    fn absolute_segments(&self, module: &ModuleRecord, path: &ModulePath) -> Result<Vec<String>, ModuleError> {
        if path.relative_level == 0 {
            if !self.explicit_root {
                return Err(ModuleError::new(format!(
                    "Absolute import '{}' requires an explicit project root.",
                    format_module_path(path)
                )));
            }
            return Ok(path.segments.clone());
        }

        let mut base = module.package_segments.clone();
        for _ in 1..path.relative_level {
            if base.pop().is_none() {
                return Err(ModuleError::new(format!(
                    "Relative import '{}' escapes above the project root.",
                    format_module_path(path)
                )));
            }
        }
        base.extend(path.segments.clone());
        Ok(base)
    }

    fn locate_module_file(&self, segments: &[String]) -> Result<PathBuf, ModuleError> {
        let mut dir_path = self.root.clone();
        for segment in segments.iter() {
            dir_path.push(segment);
        }

        let package_path = dir_path.join("mod.lang");
        let file_path = self.root.join(PathBuf::from_iter(segments.iter())).with_extension("lang");

        let has_package = package_path.exists();
        let has_file = file_path.exists();

        match (has_package, has_file) {
            (true, false) => Ok(package_path),
            (false, true) => Ok(file_path),
            (true, true) => Err(ModuleError::new(format!(
                "Module path '{}' is ambiguous; both '{}' and '{}' exist.",
                segments.join("."),
                file_path.display(),
                package_path.display()
            ))),
            (false, false) => Err(ModuleError::new(format!(
                "Module '{}' does not exist under '{}'.",
                segments.join("."),
                self.root.display()
            ))),
        }
    }

    fn hidden_alias_for(
        &self,
        alias_counter: &mut usize,
        alias_by_module: &mut BTreeMap<String, String>,
        module_id: &str,
    ) -> String {
        if let Some(alias) = alias_by_module.get(module_id) {
            return alias.clone();
        }

        let alias = format!("__lang_import_{}", *alias_counter);
        *alias_counter += 1;
        alias_by_module.insert(module_id.to_string(), alias.clone());
        alias
    }

    fn module(&self, module_id: &str) -> Result<&ModuleRecord, ModuleError> {
        self.modules
            .get(module_id)
            .ok_or_else(|| ModuleError::new(format!("Module '{}' was not loaded.", module_id)))
    }

    fn rewrite_node(
        &self,
        node: &Node,
        active_bindings: &BTreeMap<String, ImportBinding>,
        module_bindings: &BTreeMap<String, ImportBinding>,
        locals: &HashSet<String>,
    ) -> Node {
        match node {
            Node::Identifier(name) => rewrite_identifier(name, active_bindings, locals),
            Node::MemberAccess { object, member } => Node::MemberAccess {
                object: Box::new(self.rewrite_node(object, active_bindings, module_bindings, locals)),
                member: Box::new((**member).clone()),
            },
            Node::Group(expr) => Node::Group(expr.as_ref().map(|expr| {
                Box::new(self.rewrite_node(expr, active_bindings, module_bindings, locals))
            })),
            Node::BinOp { lhs, op, rhs } => Node::BinOp {
                lhs: Box::new(self.rewrite_node(lhs, active_bindings, module_bindings, locals)),
                op: op.clone(),
                rhs: Box::new(self.rewrite_node(rhs, active_bindings, module_bindings, locals)),
            },
            Node::UnaryOp { op, expr } => Node::UnaryOp {
                op: op.clone(),
                expr: Box::new(self.rewrite_node(expr, active_bindings, module_bindings, locals)),
            },
            Node::NamedArg(name, value) => Node::NamedArg(
                name.clone(),
                Box::new(self.rewrite_node(value, active_bindings, module_bindings, locals)),
            ),
            Node::Array(values) => Node::Array(Box::new(self.rewrite_node(
                values,
                active_bindings,
                module_bindings,
                locals,
            ))),
            Node::Index { object, index } => Node::Index {
                object: Box::new(self.rewrite_node(object, active_bindings, module_bindings, locals)),
                index: Box::new(self.rewrite_node(index, active_bindings, module_bindings, locals)),
            },
            Node::Sequence(values) => Node::Sequence(
                values
                    .iter()
                    .map(|value| self.rewrite_node(value, active_bindings, module_bindings, locals))
                    .collect(),
            ),
            Node::WrappedSequence(values) => Node::WrappedSequence(
                values
                    .iter()
                    .map(|value| self.rewrite_node(value, active_bindings, module_bindings, locals))
                    .collect(),
            ),
            Node::Block(nodes) => Node::Block(
                nodes
                    .iter()
                    .map(|value| self.rewrite_node(value, active_bindings, module_bindings, locals))
                    .collect(),
            ),
            Node::FnBody(nodes) => Node::FnBody(
                nodes
                    .iter()
                    .map(|value| self.rewrite_node(value, active_bindings, module_bindings, locals))
                    .collect(),
            ),
            Node::FunctionCall { object, args } => Node::FunctionCall {
                object: Box::new(self.rewrite_node(object, active_bindings, module_bindings, locals)),
                args: Box::new(self.rewrite_node(args, active_bindings, module_bindings, locals)),
            },
            Node::Return(value) => Node::Return(Box::new(self.rewrite_node(
                value,
                active_bindings,
                module_bindings,
                locals,
            ))),
            Node::Dict { keys, values } => Node::Dict {
                keys: keys
                    .iter()
                    .map(|value| self.rewrite_node(value, active_bindings, module_bindings, locals))
                    .collect(),
                values: values
                    .iter()
                    .map(|value| self.rewrite_node(value, active_bindings, module_bindings, locals))
                    .collect(),
            },
            Node::Alias(value) => Node::Alias(Box::new(self.rewrite_node(
                value,
                active_bindings,
                module_bindings,
                locals,
            ))),
            Node::Range { start, end, inclusive } => Node::Range {
                start: Box::new(self.rewrite_node(start, active_bindings, module_bindings, locals)),
                end: Box::new(self.rewrite_node(end, active_bindings, module_bindings, locals)),
                inclusive: *inclusive,
            },
            Node::Len(value) => Node::Len(Box::new(self.rewrite_node(
                value,
                active_bindings,
                module_bindings,
                locals,
            ))),
            Node::Distribution { args, recipients } => Node::Distribution {
                args: args
                    .iter()
                    .map(|value| self.rewrite_node(value, active_bindings, module_bindings, locals))
                    .collect(),
                recipients: recipients
                    .iter()
                    .map(|value| self.rewrite_node(value, active_bindings, module_bindings, locals))
                    .collect(),
            },
            Node::IterDistribution { args, recipients } => Node::IterDistribution {
                args: args
                    .iter()
                    .map(|value| self.rewrite_node(value, active_bindings, module_bindings, locals))
                    .collect(),
                recipients: recipients
                    .iter()
                    .map(|value| self.rewrite_node(value, active_bindings, module_bindings, locals))
                    .collect(),
            },
            Node::Lambda { args, body } => {
                let lambda_locals = collect_lambda_locals(args, body);
                let mut combined_locals = locals.clone();
                combined_locals.extend(lambda_locals);
                Node::Lambda {
                    args: Box::new((**args).clone()),
                    body: Box::new(self.rewrite_node(body, module_bindings, module_bindings, &combined_locals)),
                }
            }
            Node::Conditional { condition, body, elifs, else_body } => Node::Conditional {
                condition: Box::new(self.rewrite_node(
                    condition,
                    active_bindings,
                    module_bindings,
                    locals,
                )),
                body: Box::new(self.rewrite_node(body, active_bindings, module_bindings, locals)),
                elifs: elifs
                    .iter()
                    .map(|(condition, body)| {
                        (
                            self.rewrite_node(condition, active_bindings, module_bindings, locals),
                            self.rewrite_node(body, active_bindings, module_bindings, locals),
                        )
                    })
                    .collect(),
                else_body: else_body.as_ref().map(|body| {
                    Box::new(self.rewrite_node(body, active_bindings, module_bindings, locals))
                }),
            },
            Node::BindingDef { name, value, public } => {
                if let Some(binding) = active_bindings.get(name) {
                    Node::Assign {
                        identifiers: vec![Node::MemberAccess {
                            object: Box::new(Node::Identifier(binding.exporter_alias.clone())),
                            member: Box::new(Node::Identifier(binding.export_name.clone())),
                        }],
                        values: vec![self.rewrite_node(value, active_bindings, module_bindings, locals)],
                        op: "=".to_string(),
                    }
                } else {
                    Node::BindingDef {
                        name: name.clone(),
                        value: Box::new(self.rewrite_node(value, active_bindings, module_bindings, locals)),
                        public: *public,
                    }
                }
            }
            Node::Assign { identifiers, values, op } => Node::Assign {
                identifiers: identifiers
                    .iter()
                    .map(|identifier| rewrite_assignment_target(identifier, active_bindings, locals))
                    .collect(),
                values: values
                    .iter()
                    .map(|value| self.rewrite_node(value, active_bindings, module_bindings, locals))
                    .collect(),
                op: op.clone(),
            },
            Node::SignalDef { name, value, dependencies } => Node::SignalDef {
                name: name.clone(),
                value: Box::new(self.rewrite_node(value, active_bindings, module_bindings, locals)),
                dependencies: dependencies.clone(),
            },
            Node::SignalUpdate { name, value, dependencies } => Node::SignalUpdate {
                name: name.clone(),
                value: Box::new(self.rewrite_node(value, active_bindings, module_bindings, locals)),
                dependencies: dependencies.clone(),
            },
            Node::Deconstruction { identifiers, value, default_values } => Node::Deconstruction {
                identifiers: identifiers
                    .iter()
                    .map(|identifier| rewrite_assignment_target(identifier, active_bindings, locals))
                    .collect(),
                value: Box::new(self.rewrite_node(value, active_bindings, module_bindings, locals)),
                default_values: default_values
                    .iter()
                    .map(|value| self.rewrite_node(value, active_bindings, module_bindings, locals))
                    .collect(),
            },
            _ => rewrite_passthrough_node(node, active_bindings, module_bindings, locals, self),
        }
    }
}

fn rewrite_passthrough_node(
    node: &Node,
    active_bindings: &BTreeMap<String, ImportBinding>,
    module_bindings: &BTreeMap<String, ImportBinding>,
    locals: &HashSet<String>,
    builder: &ProjectBuilder,
) -> Node {
    match node {
        Node::FunctionDef { name, args, body, public } => {
            let function_locals = collect_function_locals(name, args, body);
            Node::FunctionDef {
                name: name.clone(),
                args: Box::new((**args).clone()),
                body: Box::new(builder.rewrite_node(body, module_bindings, module_bindings, &function_locals)),
                public: *public,
            }
        }
        Node::StructDef { name, generics, fields, public } => Node::StructDef {
            name: name.clone(),
            generics: generics.clone(),
            fields: fields.clone(),
            public: *public,
        },
        Node::TraitDef { name, generics, methods, public } => Node::TraitDef {
            name: name.clone(),
            generics: generics.clone(),
            methods: methods.clone(),
            public: *public,
        },
        Node::ImplBlock { generics, trait_ref, target, methods } => Node::ImplBlock {
            generics: generics.clone(),
            trait_ref: trait_ref.clone(),
            target: target.clone(),
            methods: methods
                .iter()
                .map(|method| crate::ast::ImplMethod {
                    signature: method.signature.clone(),
                    body: Box::new(builder.rewrite_node(&method.body, module_bindings, module_bindings, locals)),
                })
                .collect(),
        },
        Node::StructInit { name, fields } => Node::StructInit {
            name: rewrite_type_ref(name, active_bindings, locals),
            fields: fields
                .iter()
                .map(|(field, value)| {
                    (
                        field.clone(),
                        builder.rewrite_node(value, active_bindings, module_bindings, locals),
                    )
                })
                .collect(),
        },
        Node::ReactiveStmt { block, dependencies } => Node::ReactiveStmt {
            block: block
                .iter()
                .map(|node| builder.rewrite_node(node, active_bindings, module_bindings, locals))
                .collect(),
            dependencies: dependencies.clone(),
        },
        Node::Loop(body) => Node::Loop(Box::new(builder.rewrite_node(
            body,
            active_bindings,
            module_bindings,
            locals,
        ))),
        Node::ForLoop { item, iterable, body } => Node::ForLoop {
            item: Box::new(rewrite_assignment_target(item, active_bindings, locals)),
            iterable: Box::new(builder.rewrite_node(
                iterable,
                active_bindings,
                module_bindings,
                locals,
            )),
            body: Box::new(builder.rewrite_node(body, active_bindings, module_bindings, locals)),
        },
        Node::WhileLoop { condition, body } => Node::WhileLoop {
            condition: Box::new(builder.rewrite_node(
                condition,
                active_bindings,
                module_bindings,
                locals,
            )),
            body: Box::new(builder.rewrite_node(body, active_bindings, module_bindings, locals)),
        },
        _ => node.clone(),
    }
}

fn rewrite_type_ref(
    type_ref: &crate::ast::TypeRef,
    bindings: &BTreeMap<String, ImportBinding>,
    locals: &HashSet<String>,
) -> crate::ast::TypeRef {
    if locals.contains(&type_ref.name) {
        return type_ref.clone();
    }

    if let Some(binding) = bindings.get(&type_ref.name) {
        return crate::ast::TypeRef::new(
            format!("{}.{}", binding.exporter_alias, binding.export_name),
            type_ref.generics.clone(),
        );
    }

    type_ref.clone()
}

fn rewrite_identifier(
    name: &str,
    bindings: &BTreeMap<String, ImportBinding>,
    locals: &HashSet<String>,
) -> Node {
    if locals.contains(name) {
        return Node::Identifier(name.to_string());
    }

    match bindings.get(name) {
        Some(binding) => Node::MemberAccess {
            object: Box::new(Node::Identifier(binding.exporter_alias.clone())),
            member: Box::new(Node::Identifier(binding.export_name.clone())),
        },
        None => Node::Identifier(name.to_string()),
    }
}

fn rewrite_assignment_target(
    node: &Node,
    bindings: &BTreeMap<String, ImportBinding>,
    locals: &HashSet<String>,
) -> Node {
    match node {
        Node::Identifier(name) if !locals.contains(name) => match bindings.get(name) {
            Some(binding) => Node::MemberAccess {
                object: Box::new(Node::Identifier(binding.exporter_alias.clone())),
                member: Box::new(Node::Identifier(binding.export_name.clone())),
            },
            None => Node::Identifier(name.clone()),
        },
        Node::Sequence(values) => Node::Sequence(
            values
                .iter()
                .map(|value| rewrite_assignment_target(value, bindings, locals))
                .collect(),
        ),
        Node::WrappedSequence(values) => Node::WrappedSequence(
            values
                .iter()
                .map(|value| rewrite_assignment_target(value, bindings, locals))
                .collect(),
        ),
        Node::MemberAccess { object, member } => Node::MemberAccess {
            object: Box::new(rewrite_assignment_target(object, bindings, locals)),
            member: Box::new((**member).clone()),
        },
        other => other.clone(),
    }
}

fn collect_lambda_locals(args: &Node, body: &Node) -> HashSet<String> {
    let mut locals = HashSet::new();
    collect_argument_names(args, &mut locals);
    collect_local_bindings(body, &mut locals);
    locals
}

fn collect_function_locals(name: &str, args: &Node, body: &Node) -> HashSet<String> {
    let mut locals = HashSet::new();
    locals.insert(name.to_string());
    collect_argument_names(args, &mut locals);
    collect_local_bindings(body, &mut locals);
    locals
}

fn collect_argument_names(node: &Node, locals: &mut HashSet<String>) {
    match node {
        Node::WrappedSequence(values) | Node::Sequence(values) => {
            for value in values.iter() {
                collect_argument_names(value, locals);
            }
        }
        Node::Identifier(name) => {
            locals.insert(name.clone());
        }
        Node::NamedArg(name, _) => {
            locals.insert(name.clone());
        }
        _ => {}
    }
}

fn collect_local_bindings(node: &Node, locals: &mut HashSet<String>) {
    match node {
        Node::Assign { identifiers, .. } => {
            for identifier in identifiers.iter() {
                collect_target_identifiers(identifier, locals);
            }
        }
        Node::BindingDef { name, .. } => {
            locals.insert(name.clone());
        }
        Node::Deconstruction { identifiers, .. } => {
            for identifier in identifiers.iter() {
                collect_target_identifiers(identifier, locals);
            }
        }
        Node::ForLoop { item, iterable, body } => {
            collect_target_identifiers(item, locals);
            collect_local_bindings(iterable, locals);
            collect_local_bindings(body, locals);
        }
        Node::Loop(body) | Node::WhileLoop { body, .. } => {
            collect_local_bindings(body, locals);
        }
        Node::Conditional { body, elifs, else_body, .. } => {
            collect_local_bindings(body, locals);
            for (_, body) in elifs.iter() {
                collect_local_bindings(body, locals);
            }
            if let Some(body) = else_body.as_ref() {
                collect_local_bindings(body, locals);
            }
        }
        Node::Block(nodes) | Node::FnBody(nodes) => {
            for node in nodes.iter() {
                match node {
                    Node::FunctionDef { .. } | Node::Lambda { .. } => {}
                    other => collect_local_bindings(other, locals),
                }
            }
        }
        _ => {}
    }
}

fn collect_target_identifiers(node: &Node, locals: &mut HashSet<String>) {
    match node {
        Node::Identifier(name) => {
            locals.insert(name.clone());
        }
        Node::Sequence(values) | Node::WrappedSequence(values) => {
            for value in values.iter() {
                collect_target_identifiers(value, locals);
            }
        }
        _ => {}
    }
}

fn ensure_unique_binding(
    all_bindings: &mut BTreeMap<String, ImportBinding>,
    module: &ModuleRecord,
    local_name: &str,
    binding: &ImportBinding,
) -> Result<(), ModuleError> {
    if all_bindings.contains_key(local_name) {
        return Err(ModuleError::new(format!(
            "Module '{}' imports '{}' more than once.",
            module.id,
            local_name
        )));
    }
    all_bindings.insert(local_name.to_string(), binding.clone());
    Ok(())
}

fn insert_export(
    exports: &mut BTreeMap<String, ExportKind>,
    module_id: &str,
    name: &str,
    export: ExportKind,
) -> Result<(), ModuleError> {
    if exports.contains_key(name) {
        return Err(ModuleError::new(format!(
            "Module '{}' exports '{}' more than once.",
            module_id,
            name
        )));
    }
    exports.insert(name.to_string(), export);
    Ok(())
}

fn module_segments_from_id(module_id: &str) -> Vec<String> {
    if module_id.is_empty() {
        Vec::new()
    } else {
        module_id.split('.').map(|segment| segment.to_string()).collect()
    }
}

fn output_path_for_module(module_segments: &[String], is_package: bool) -> PathBuf {
    let mut path = PathBuf::new();
    for segment in module_segments.iter() {
        path.push(segment);
    }

    if is_package {
        path.push("__init__.py");
    } else {
        path.set_extension("py");
    }

    path
}

fn module_id_from_path(root: &Path, source_path: &Path) -> Result<String, ModuleError> {
    let relative = source_path.strip_prefix(root).map_err(|_| {
        ModuleError::new(format!(
            "Module path '{}' is not inside '{}'.",
            source_path.display(),
            root.display()
        ))
    })?;

    let mut segments = relative
        .components()
        .map(|component| component.as_os_str().to_string_lossy().to_string())
        .collect::<Vec<String>>();

    let last = segments.pop().ok_or_else(|| ModuleError::new("Could not determine module name."))?;
    if last == "mod.lang" {
        return Ok(segments.join("."));
    }

    let stem = Path::new(&last)
        .file_stem()
        .and_then(|stem| stem.to_str())
        .ok_or_else(|| ModuleError::new("Could not determine module filename stem."))?;
    segments.push(stem.to_string());
    Ok(segments.join("."))
}

fn format_module_path(path: &ModulePath) -> String {
    let mut buffer = ".".repeat(path.relative_level);
    if !path.segments.is_empty() {
        buffer.push_str(&path.segments.join("."));
    }
    buffer
}

fn canonicalize_file(path: &Path) -> Result<PathBuf, ModuleError> {
    fs::canonicalize(path).map_err(|error| {
        ModuleError::new(format!("Failed to canonicalize file '{}': {error}", path.display()))
    })
}

fn canonicalize_dir(path: &Path) -> Result<PathBuf, ModuleError> {
    fs::canonicalize(path).map_err(|error| {
        ModuleError::new(format!("Failed to canonicalize directory '{}': {error}", path.display()))
    })
}
