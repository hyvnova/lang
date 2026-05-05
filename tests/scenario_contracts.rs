mod common;

use common::run_project_with_io;
use rand::random;
use serde::Deserialize;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Deserialize)]
struct Scenario {
    entry: String,
    #[serde(default)]
    stdin: String,
    #[serde(default)]
    args: Vec<String>,
    #[serde(default)]
    expected_stdout: String,
    expected_stderr_contains: Option<String>,
    #[serde(default)]
    expected_files: Vec<ExpectedFile>,
}

#[derive(Debug, Deserialize)]
struct ExpectedFile {
    path: String,
    contents: String,
}

#[test]
fn scenarios_run_from_disk() {
    let scenario_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("scenarios");
    let mut scenarios = fs::read_dir(&scenario_root)
        .expect("failed to read scenarios directory")
        .filter_map(|entry| entry.ok())
        .filter(|entry| entry.file_type().map(|kind| kind.is_dir()).unwrap_or(false))
        .map(|entry| entry.path())
        .collect::<Vec<PathBuf>>();
    scenarios.sort();

    assert!(
        !scenarios.is_empty(),
        "expected at least one scenario fixture"
    );

    for scenario_dir in scenarios {
        run_scenario(&scenario_dir);
    }
}

fn run_scenario(scenario_dir: &Path) {
    let name = scenario_dir
        .file_name()
        .and_then(|name| name.to_str())
        .expect("scenario directory should have a UTF-8 name");
    let manifest_path = scenario_dir.join("scenario.toml");
    let scenario: Scenario = toml::from_str(
        &fs::read_to_string(&manifest_path)
            .unwrap_or_else(|error| panic!("{name}: failed to read scenario.toml: {error}")),
    )
    .unwrap_or_else(|error| panic!("{name}: failed to parse scenario.toml: {error}"));

    let project_root = copy_scenario_project(scenario_dir, name);
    let entry_path = project_root.join(scenario.entry.replace('/', "\\"));
    let args = scenario
        .args
        .iter()
        .map(String::as_str)
        .collect::<Vec<&str>>();
    let output = run_project_with_io(
        &entry_path,
        Some(project_root.clone()),
        &scenario.stdin,
        &args,
    )
    .unwrap_or_else(|error| panic!("{name}: project failed to build: {error}"));

    let stdout = normalize_output(&String::from_utf8_lossy(&output.stdout));
    let stderr = normalize_output(&String::from_utf8_lossy(&output.stderr));
    let expected_stdout = normalize_output(&scenario.expected_stdout);

    if let Some(expected) = scenario.expected_stderr_contains {
        assert!(
            !output.status.success(),
            "{name}: expected runtime failure containing {expected:?}, but project succeeded\nstdout:\n{stdout}\nstderr:\n{stderr}"
        );
        assert!(
            stderr.contains(&expected),
            "{name}: expected stderr containing {expected:?}\nstdout:\n{stdout}\nstderr:\n{stderr}"
        );
        return;
    }

    assert!(
        output.status.success(),
        "{name}: project run failed\nstdout:\n{stdout}\nstderr:\n{stderr}"
    );
    assert!(
        stdout == expected_stdout,
        "{name}: stdout mismatch\nexpected stdout:\n{expected_stdout}\nactual stdout:\n{stdout}\nstderr:\n{stderr}"
    );

    for expected_file in scenario.expected_files {
        let path = project_root.join(expected_file.path.replace('/', "\\"));
        let actual = fs::read_to_string(&path).unwrap_or_else(|error| {
            panic!(
                "{name}: failed to read expected file '{}': {error}",
                path.display()
            )
        });
        assert!(
            actual == expected_file.contents,
            "{name}: generated file '{}' mismatch\n{}",
            expected_file.path,
            diff_context(&expected_file.contents, &actual)
        );
    }
}

fn copy_scenario_project(scenario_dir: &Path, name: &str) -> PathBuf {
    let root = std::env::temp_dir().join(format!("lang_scenario_{name}_{}", random::<u64>()));
    fs::create_dir_all(&root).expect("failed to create scenario temp project");
    copy_dir_contents(scenario_dir, &root);
    root
}

fn copy_dir_contents(from: &Path, to: &Path) {
    for entry in fs::read_dir(from).expect("failed to read scenario directory") {
        let entry = entry.expect("failed to read scenario directory entry");
        let source = entry.path();
        if source.file_name().and_then(|name| name.to_str()) == Some("scenario.toml") {
            continue;
        }

        let target = to.join(entry.file_name());
        if entry
            .file_type()
            .expect("failed to inspect scenario file")
            .is_dir()
        {
            fs::create_dir_all(&target).expect("failed to create scenario subdirectory");
            copy_dir_contents(&source, &target);
        } else {
            fs::copy(&source, &target).expect("failed to copy scenario fixture file");
        }
    }
}

fn normalize_output(value: &str) -> String {
    value.replace("\r\n", "\n")
}

fn diff_context(expected: &str, actual: &str) -> String {
    let expected_lines = expected.lines().collect::<Vec<&str>>();
    let actual_lines = actual.lines().collect::<Vec<&str>>();
    let max_len = expected_lines.len().max(actual_lines.len());
    let first_diff = (0..max_len)
        .find(|index| expected_lines.get(*index) != actual_lines.get(*index))
        .unwrap_or(0);
    let start = first_diff.saturating_sub(2);
    let end = (first_diff + 3).min(max_len);
    let mut lines = Vec::new();

    lines.push(format!("first differing line: {}", first_diff + 1));
    for index in start..end {
        lines.push(format!(
            "- {:>4}: {}",
            index + 1,
            expected_lines.get(index).copied().unwrap_or("<missing>")
        ));
        lines.push(format!(
            "+ {:>4}: {}",
            index + 1,
            actual_lines.get(index).copied().unwrap_or("<missing>")
        ));
    }

    lines.join("\n")
}
