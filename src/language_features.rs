use serde::Deserialize;
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FeatureCoverage {
    pub feature: LanguageFeature,
    pub parser: bool,
    pub transpiler_runtime: bool,
    pub docs: bool,
    pub scenario: bool,
    pub tree_sitter: bool,
    pub textmate: bool,
    pub lsp: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MissingCoverage {
    pub feature: LanguageFeature,
    pub dimension: &'static str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MissingSyntaxCoverage {
    pub feature: LanguageFeature,
    pub tool: &'static str,
}

#[derive(Debug, Deserialize)]
pub struct SyntaxFeatureMetadata {
    #[serde(default)]
    pub tree_sitter: Vec<String>,
    #[serde(default)]
    pub textmate: Vec<String>,
}

macro_rules! language_features {
    (
        $(
            $variant:ident {
                key: $key:literal,
                title: $title:literal,
                parser: $parser:literal,
                transpiler_runtime: $transpiler_runtime:literal,
                docs: $docs:literal,
                scenario: $scenario:literal,
                tree_sitter: $tree_sitter:literal,
                textmate: $textmate:literal,
        lsp: $lsp:literal $(,)?
            }
        ),+ $(,)?
    ) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum LanguageFeature {
            $($variant),+
        }

        impl LanguageFeature {
            pub fn key(self) -> &'static str {
                match self {
                    $(Self::$variant => $key),+
                }
            }

            pub fn title(self) -> &'static str {
                match self {
                    $(Self::$variant => $title),+
                }
            }
        }

        const ALL_FEATURES: &[FeatureCoverage] = &[
            $(
                FeatureCoverage {
                    feature: LanguageFeature::$variant,
                    parser: $parser,
                    transpiler_runtime: $transpiler_runtime,
                    docs: $docs,
                    scenario: $scenario,
                    tree_sitter: $tree_sitter,
                    textmate: $textmate,
            lsp: $lsp,
                }
            ),+
        ];
    };
}

language_features! {
    Comments {
        key: "comments",
        title: "Comments",
        parser: true,
        transpiler_runtime: true,
        docs: true,
        scenario: false,
        tree_sitter: true,
        textmate: true,
        lsp: false,
    },
    Literals {
        key: "literals",
        title: "Literals",
        parser: true,
        transpiler_runtime: true,
        docs: true,
        scenario: true,
        tree_sitter: true,
        textmate: true,
        lsp: false,
    },
    FunctionDefinition {
        key: "function_definition",
        title: "Function definitions",
        parser: true,
        transpiler_runtime: true,
        docs: true,
        scenario: true,
        tree_sitter: true,
        textmate: true,
        lsp: true,
    },
    TypedFunctionSignature {
        key: "typed_function_signature",
        title: "Typed function signatures",
        parser: true,
        transpiler_runtime: true,
        docs: true,
        scenario: true,
        tree_sitter: true,
        textmate: true,
        lsp: true,
    },
    LambdaExpression {
        key: "lambda_expression",
        title: "Lambda expressions",
        parser: true,
        transpiler_runtime: true,
        docs: false,
        scenario: false,
        tree_sitter: true,
        textmate: true,
        lsp: false,
    },
    BlockLambda {
        key: "block_lambda",
        title: "Block-bodied lambdas",
        parser: true,
        transpiler_runtime: true,
        docs: true,
        scenario: true,
        tree_sitter: true,
        textmate: true,
        lsp: false,
    },
    PrimitiveWrappers {
        key: "primitive_wrappers",
        title: "Lang primitive runtime values",
        parser: true,
        transpiler_runtime: true,
        docs: true,
        scenario: true,
        tree_sitter: true,
        textmate: true,
        lsp: true,
    },
    MethodChains {
        key: "method_chains",
        title: "Method chains",
        parser: true,
        transpiler_runtime: true,
        docs: true,
        scenario: true,
        tree_sitter: true,
        textmate: true,
        lsp: true,
    },
    StructDefinition {
        key: "struct_definition",
        title: "Struct definitions",
        parser: true,
        transpiler_runtime: true,
        docs: true,
        scenario: false,
        tree_sitter: true,
        textmate: true,
        lsp: true,
    },
    TraitDefinition {
        key: "trait_definition",
        title: "Trait definitions",
        parser: true,
        transpiler_runtime: true,
        docs: true,
        scenario: false,
        tree_sitter: true,
        textmate: true,
        lsp: true,
    },
    ImplBlock {
        key: "impl_block",
        title: "Impl blocks",
        parser: true,
        transpiler_runtime: true,
        docs: true,
        scenario: false,
        tree_sitter: true,
        textmate: true,
        lsp: true,
    },
    ModuleSystem {
        key: "module_system",
        title: "Modules and imports",
        parser: true,
        transpiler_runtime: true,
        docs: true,
        scenario: true,
        tree_sitter: true,
        textmate: true,
        lsp: true,
    },
    GlobalStd {
        key: "global_std",
        title: "Global std object",
        parser: true,
        transpiler_runtime: true,
        docs: true,
        scenario: true,
        tree_sitter: true,
        textmate: true,
        lsp: true,
    },
    RawPythonBlock {
        key: "raw_python_block",
        title: "Raw Python blocks",
        parser: true,
        transpiler_runtime: true,
        docs: true,
        scenario: false,
        tree_sitter: true,
        textmate: true,
        lsp: false,
    },
    MacroCall {
        key: "macro_call",
        title: "Macro calls",
        parser: true,
        transpiler_runtime: true,
        docs: false,
        scenario: false,
        tree_sitter: true,
        textmate: true,
        lsp: false,
    },
    Signal {
        key: "signal",
        title: "Signals and reactive statements",
        parser: true,
        transpiler_runtime: true,
        docs: true,
        scenario: false,
        tree_sitter: true,
        textmate: true,
        lsp: false,
    },
    Collections {
        key: "collections",
        title: "Arrays and dictionaries",
        parser: true,
        transpiler_runtime: true,
        docs: true,
        scenario: true,
        tree_sitter: true,
        textmate: true,
        lsp: false,
    },
    RangeExpression {
        key: "range_expression",
        title: "Range expressions",
        parser: true,
        transpiler_runtime: true,
        docs: true,
        scenario: true,
        tree_sitter: true,
        textmate: true,
        lsp: false,
    },
    Distribution {
        key: "distribution",
        title: "Distribution pipelines",
        parser: true,
        transpiler_runtime: true,
        docs: false,
        scenario: false,
        tree_sitter: true,
        textmate: true,
        lsp: false,
    },
    ControlFlow {
        key: "control_flow",
        title: "Control flow",
        parser: true,
        transpiler_runtime: true,
        docs: true,
        scenario: true,
        tree_sitter: true,
        textmate: true,
        lsp: false,
    },
    FileIoStd {
        key: "file_io_std",
        title: "File and IO std APIs",
        parser: true,
        transpiler_runtime: true,
        docs: true,
        scenario: true,
        tree_sitter: true,
        textmate: true,
        lsp: true,
    },
}

pub fn all_features() -> &'static [FeatureCoverage] {
    ALL_FEATURES
}

pub fn missing_coverage() -> Vec<MissingCoverage> {
    let mut missing = Vec::new();
    for coverage in all_features() {
        push_missing(&mut missing, coverage, "parser", coverage.parser);
        push_missing(
            &mut missing,
            coverage,
            "transpiler_runtime",
            coverage.transpiler_runtime,
        );
        push_missing(&mut missing, coverage, "docs", coverage.docs);
        push_missing(&mut missing, coverage, "scenario", coverage.scenario);
        push_missing(&mut missing, coverage, "tree_sitter", coverage.tree_sitter);
        push_missing(&mut missing, coverage, "textmate", coverage.textmate);
        push_missing(&mut missing, coverage, "lsp", coverage.lsp);
    }
    missing
}

pub fn syntax_metadata_path(project_root: &Path) -> PathBuf {
    project_root.join("syntax").join("features.json")
}

pub fn load_syntax_feature_metadata(project_root: &Path) -> Result<SyntaxFeatureMetadata, String> {
    let path = syntax_metadata_path(project_root);
    let source = fs::read_to_string(&path)
        .map_err(|error| format!("failed to read '{}': {error}", path.display()))?;
    serde_json::from_str(&source)
        .map_err(|error| format!("failed to parse '{}': {error}", path.display()))
}

pub fn missing_syntax_metadata(metadata: &SyntaxFeatureMetadata) -> Vec<MissingSyntaxCoverage> {
    let tree_sitter = metadata
        .tree_sitter
        .iter()
        .map(String::as_str)
        .collect::<BTreeSet<_>>();
    let textmate = metadata
        .textmate
        .iter()
        .map(String::as_str)
        .collect::<BTreeSet<_>>();
    let mut missing = Vec::new();

    for coverage in all_features().iter().filter(|coverage| coverage.parser) {
        let key = coverage.feature.key();
        if coverage.tree_sitter && !tree_sitter.contains(key) {
            missing.push(MissingSyntaxCoverage {
                feature: coverage.feature,
                tool: "tree_sitter",
            });
        }
        if coverage.textmate && !textmate.contains(key) {
            missing.push(MissingSyntaxCoverage {
                feature: coverage.feature,
                tool: "textmate",
            });
        }
    }

    missing
}

pub fn syntax_coverage_report(project_root: &Path) -> Result<(String, bool), String> {
    let registry_missing = missing_declared_syntax_coverage();
    let metadata = load_syntax_feature_metadata(project_root)?;
    let syntax_missing = missing_syntax_metadata(&metadata);
    let has_missing = !registry_missing.is_empty() || !syntax_missing.is_empty();
    let mut lines = Vec::new();

    if has_missing {
        lines.push("Syntax coverage gaps:".to_string());
        for item in registry_missing {
            lines.push(format!(
                "- {} ({}) missing {}",
                item.feature.title(),
                item.feature.key(),
                item.dimension
            ));
        }
        for item in syntax_missing {
            lines.push(format!(
                "- {} ({}) missing {} metadata",
                item.feature.title(),
                item.feature.key(),
                item.tool
            ));
        }
    } else {
        lines.push(format!(
            "Syntax coverage is complete for {} registered features.",
            all_features().len()
        ));
    }

    Ok((lines.join("\n"), has_missing))
}

fn missing_declared_syntax_coverage() -> Vec<MissingCoverage> {
    let mut missing = Vec::new();
    for coverage in all_features().iter().filter(|coverage| coverage.parser) {
        push_missing(&mut missing, coverage, "tree_sitter", coverage.tree_sitter);
        push_missing(&mut missing, coverage, "textmate", coverage.textmate);
    }
    missing
}

fn push_missing(
    missing: &mut Vec<MissingCoverage>,
    coverage: &FeatureCoverage,
    dimension: &'static str,
    present: bool,
) {
    if !present {
        missing.push(MissingCoverage {
            feature: coverage.feature,
            dimension,
        });
    }
}
