use serde::Deserialize;
use std::fs;
use std::path::{Path, PathBuf};

const MANIFEST_FILE_NAME: &str = "lang.toml";
const DEFAULT_TRANSPILER: &str = "python";

#[derive(Debug, Clone)]
pub struct ConfigError {
    pub message: String,
}

impl ConfigError {
    pub(crate) fn new<T: Into<String>>(message: T) -> Self {
        ConfigError {
            message: message.into(),
        }
    }
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for ConfigError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LangManifest {
    pub manifest_path: PathBuf,
    pub manifest_dir: PathBuf,
    pub project_root: PathBuf,
    pub entry_file: Option<PathBuf>,
    pub transpiler: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedRunConfig {
    pub entry_file: PathBuf,
    pub project_root: Option<PathBuf>,
    pub transpiler: String,
    pub manifest_path: Option<PathBuf>,
}

#[derive(Debug, Deserialize)]
struct ManifestFile {
    project: Option<ProjectSection>,
    run: Option<RunSection>,
}

#[derive(Debug, Deserialize)]
struct ProjectSection {
    root: Option<PathBuf>,
    entry: Option<PathBuf>,
}

#[derive(Debug, Deserialize)]
struct RunSection {
    transpiler: Option<String>,
}

pub fn discover_manifest(start: &Path) -> Option<PathBuf> {
    let mut current = if start.is_dir() {
        start.to_path_buf()
    } else {
        start.parent().unwrap_or(start).to_path_buf()
    };

    loop {
        let candidate = current.join(MANIFEST_FILE_NAME);
        if candidate.exists() {
            return Some(candidate);
        }

        if !current.pop() {
            return None;
        }
    }
}

pub fn load_manifest(path: &Path) -> Result<LangManifest, ConfigError> {
    let manifest_path = path.to_path_buf();
    let manifest_dir = manifest_path
        .parent()
        .ok_or_else(|| {
            ConfigError::new(format!(
                "Manifest '{}' has no parent directory.",
                manifest_path.display()
            ))
        })?
        .to_path_buf();

    let contents = fs::read_to_string(&manifest_path).map_err(|error| {
        ConfigError::new(format!(
            "Failed to read manifest '{}': {error}",
            manifest_path.display()
        ))
    })?;

    let file: ManifestFile = toml::from_str(&contents).map_err(|error| {
        ConfigError::new(format!(
            "Failed to parse manifest '{}': {error}",
            manifest_path.display()
        ))
    })?;

    let project = file.project.ok_or_else(|| {
        ConfigError::new(format!(
            "Manifest '{}' is missing the [project] section.",
            manifest_path.display()
        ))
    })?;

    let root = project.root.unwrap_or_else(|| PathBuf::from("."));
    let project_root = manifest_dir.join(root);
    let entry_file = project.entry.map(|entry| project_root.join(entry));

    let transpiler = file.run.and_then(|run| run.transpiler);

    Ok(LangManifest {
        manifest_path,
        manifest_dir: manifest_dir.to_path_buf(),
        project_root,
        entry_file,
        transpiler,
    })
}

pub fn resolve_run_config(
    file: Option<PathBuf>,
    cli_project_root: Option<PathBuf>,
    cli_transpiler: Option<String>,
    cwd: &Path,
) -> Result<ResolvedRunConfig, ConfigError> {
    let file = file.map(|path| absolutize_path(cwd, path));

    let manifest_path = match &file {
        Some(file) => discover_manifest(file),
        None => discover_manifest(cwd),
    };
    let manifest = manifest_path
        .as_ref()
        .map(|path| load_manifest(path))
        .transpose()?;

    let project_root = cli_project_root
        .map(|path| absolutize_path(cwd, path))
        .or_else(|| {
            manifest
                .as_ref()
                .map(|manifest| manifest.project_root.clone())
        });

    if let Some(project_root) = project_root.as_ref() {
        if !project_root.is_dir() {
            return Err(ConfigError::new(format!(
                "Project root '{}' is not a directory.",
                project_root.display()
            )));
        }
    }

    let transpiler = cli_transpiler
        .or_else(|| {
            manifest
                .as_ref()
                .and_then(|manifest| manifest.transpiler.clone())
        })
        .unwrap_or_else(|| DEFAULT_TRANSPILER.to_string());
    validate_transpiler(&transpiler)?;

    let manifest_driven = file.is_none() && manifest.is_some();

    let entry_file = match file {
        Some(file) => file,
        None => match manifest.as_ref() {
            Some(manifest) => manifest.entry_file.clone().ok_or_else(|| {
                ConfigError::new("Manifest-driven execution requires project.entry.")
            })?,
            None => {
                return Err(ConfigError::new(
                    "No entry file provided and no lang.toml was found.",
                ))
            }
        },
    };

    if manifest_driven && !entry_file.is_file() {
        return Err(ConfigError::new(format!(
            "Entry file '{}' does not exist.",
            entry_file.display()
        )));
    }

    Ok(ResolvedRunConfig {
        entry_file,
        project_root,
        transpiler,
        manifest_path,
    })
}

fn validate_transpiler(transpiler: &str) -> Result<(), ConfigError> {
    if transpiler != DEFAULT_TRANSPILER {
        return Err(ConfigError::new(format!(
            "Unsupported transpiler '{}'. Only '{}' is supported right now.",
            transpiler, DEFAULT_TRANSPILER
        )));
    }

    Ok(())
}

fn absolutize_path(base: &Path, path: PathBuf) -> PathBuf {
    if path.is_absolute() {
        path
    } else {
        base.join(path)
    }
}
