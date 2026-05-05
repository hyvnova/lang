mod common;

use common::{
    assert_project_build_error, assert_project_runs, assert_project_runtime_error,
    build_project_fixture, build_temp_project,
};
use lang::modules::build_project;

#[test]
fn generated_python_runs_nested_modules_with_live_bindings() {
    assert_project_runs(
        &[
            (
                "app/math/mod.lang",
                r#"
pub mod vec
pub mod state
pub value = 1
pub use .vec.Vec2
"#,
            ),
            (
                "app/math/vec.lang",
                r#"
pub struct Vec2 {
    x: int,
    y: int,
}

pub fn magnitude(v) {
    v.x + v.y
}
"#,
            ),
            (
                "app/math/state.lang",
                r#"
print("state init")
pub value = 1
"#,
            ),
            (
                "app/main.lang",
                r#"
from .math import *
from .math.state import value as state_value
from app.math.vec import magnitude

print(value)
state_value = 7
import .math.state as state
print(state.value)
state.value = 8
print(state_value)
point = Vec2 { x: 2, y: 3 }
print(magnitude(point))
"#,
            ),
        ],
        "app/main.lang",
        "state init\n1\n7\n8\n5",
    );
}

#[test]
fn builds_python_package_tree_for_project_modules() {
    let output_root = build_project_fixture(
        &[
            (
                "app/mod.lang",
                r#"
pub mod math
"#,
            ),
            (
                "app/math/mod.lang",
                r#"
pub value = 1
"#,
            ),
            (
                "app/main.lang",
                r#"
from .math import value
print(value)
"#,
            ),
        ],
        "app/main.lang",
    )
    .expect("project should build");

    assert!(output_root.join("app").join("__init__.py").exists());
    assert!(output_root
        .join("app")
        .join("math")
        .join("__init__.py")
        .exists());
    assert!(output_root.join("app").join("main.py").exists());
}

#[test]
fn rejects_private_named_imports() {
    assert_project_build_error(
        &[
            (
                "app/internal.lang",
                r#"
value = 1
"#,
            ),
            (
                "app/main.lang",
                r#"
from .internal import value
"#,
            ),
        ],
        "app/main.lang",
        "does not publicly export 'value'",
    );
}

#[test]
fn rejects_missing_modules() {
    assert_project_build_error(
        &[(
            "app/main.lang",
            r#"
import .missing
"#,
        )],
        "app/main.lang",
        "does not exist",
    );
}

#[test]
fn rejects_duplicate_public_exports() {
    assert_project_build_error(
        &[
            (
                "app/math/state.lang",
                r#"
pub value = 1
"#,
            ),
            (
                "app/mod.lang",
                r#"
pub value = 1
pub use .math.state.value as value
"#,
            ),
            (
                "app/main.lang",
                r#"
from app import value
print(value)
"#,
            ),
        ],
        "app/main.lang",
        "exports 'value' more than once",
    );
}

#[test]
fn rejects_import_cycles_with_full_chain() {
    assert_project_build_error(
        &[
            (
                "app/a.lang",
                r#"
import .b
"#,
            ),
            (
                "app/b.lang",
                r#"
import .a
"#,
            ),
            (
                "app/main.lang",
                r#"
import .a
"#,
            ),
        ],
        "app/main.lang",
        "Import cycle detected: app.a -> app.b -> app.a",
    );
}

#[test]
fn rejects_public_module_declarations_without_a_child_module() {
    assert_project_build_error(
        &[
            (
                "app/mod.lang",
                r#"
pub mod missing
"#,
            ),
            (
                "app/main.lang",
                r#"
from app import missing
"#,
            ),
        ],
        "app/main.lang",
        "does not exist",
    );
}

#[test]
fn absolute_imports_require_an_explicit_project_root() {
    let (root, entry_path) = build_temp_project(
        &[
            (
                "app/math/vec.lang",
                r#"
pub value = 1
"#,
            ),
            (
                "app/main.lang",
                r#"
from app.math.vec import value
"#,
            ),
        ],
        "app/main.lang",
    );

    let error = build_project(entry_path, None, &root.join("__out__"))
        .expect_err("absolute imports should require an explicit root");

    assert!(error.message.contains("requires an explicit project root"));
}

#[test]
fn bare_single_segment_imports_resolve_to_sibling_modules() {
    assert_project_runs(
        &[
            (
                "util.lang",
                r#"
pub value = 1
"#,
            ),
            (
                "main.lang",
                r#"
import util
print(util)
print(util.value)
util.value = 7
print(util)
print(util.value)
"#,
            ),
        ],
        "main.lang",
        "<module util>\n1\n<module util>\n7",
    );
}

#[test]
fn bare_single_segment_imports_prefer_sibling_modules_over_root_modules() {
    assert_project_runs(
        &[
            (
                "util.lang",
                r#"
pub value = 0
"#,
            ),
            (
                "app/util.lang",
                r#"
pub value = 1
"#,
            ),
            (
                "app/main.lang",
                r#"
import util
print(util)
print(util.value)
"#,
            ),
        ],
        "app/main.lang",
        "<module app.util>\n1",
    );
}

#[test]
fn module_display_uses_private_module_string_binding() {
    assert_project_runs(
        &[
            (
                "util.lang",
                r#"
__module_str__ = "utility"
pub value = 1
"#,
            ),
            (
                "main.lang",
                r#"
import util
print(util)
"#,
            ),
        ],
        "main.lang",
        "utility",
    );
}

#[test]
fn module_display_rejects_non_string_override_values() {
    assert_project_runtime_error(
        &[
            (
                "util.lang",
                r#"
__module_str__ = 42
"#,
            ),
            (
                "main.lang",
                r#"
import util
print(util)
"#,
            ),
        ],
        "main.lang",
        "__module_str__ must be str",
    );
}

#[test]
fn rejects_public_module_display_metadata_bindings() {
    assert_project_build_error(
        &[
            (
                "util.lang",
                r#"
pub __module_str__ = "bad"
"#,
            ),
            (
                "main.lang",
                r#"
import util
"#,
            ),
        ],
        "main.lang",
        "reserved for private module display metadata",
    );
}

#[test]
fn rejects_importing_private_module_display_metadata() {
    assert_project_build_error(
        &[
            (
                "util.lang",
                r#"
__module_str__ = "utility"
pub value = 1
"#,
            ),
            (
                "main.lang",
                r#"
from util import __module_str__
"#,
            ),
        ],
        "main.lang",
        "does not publicly export '__module_str__'",
    );
}

#[test]
fn generated_python_runs_std_prelude_without_imports() {
    assert_project_runs(
        &[(
            "main.lang",
            r#"
value = Some("spark")
print(value.unwrap())
print(Ok(3).unwrap())
print(len(range(1, 4)))
"#,
        )],
        "main.lang",
        "spark\n3\n3",
    );
}

#[test]
fn generated_python_runs_explicit_std_imports() {
    assert_project_runs(
        &[(
            "main.lang",
            r#"
from std.text import upper, contains
from std.random import choice
from std.term import color

print(upper("lamp"))
print(contains("lamp", "am"))
print(choice(range(1, 1)).__class__.__name__)
print(color("hot", "red"))
"#,
        )],
        "main.lang",
        "LAMP\nTrue\nErr\n\u{1b}[31mhot\u{1b}[0m",
    );
}

#[test]
fn generated_python_runs_std_module_namespace_import() {
    assert_project_runs(
        &[(
            "main.lang",
            r#"
import std.fs as fs
print(fs.exists("missing-file-for-stdlib-test.txt"))
"#,
        )],
        "main.lang",
        "False",
    );
}

#[test]
fn generated_python_runs_global_std_object_without_imports() {
    assert_project_runs(
        &[(
            "main.lang",
            r#"
print(std.text.upper("lamp"))
print(std.fs.exists("missing-file-for-global-std-test.txt"))
print(std.term.bold("hot"))
"#,
        )],
        "main.lang",
        "LAMP\nFalse\n\u{1b}[1mhot\u{1b}[0m",
    );
}

#[test]
fn generated_project_contains_std_facades_and_native_runtime() {
    let output_root = build_project_fixture(
        &[(
            "main.lang",
            r#"
from std.iter import range
print(len(range(1, 2)))
"#,
        )],
        "main.lang",
    )
    .expect("project should build");

    assert!(output_root.join("std").join("__init__.py").exists());
    assert!(output_root.join("std").join("iter.py").exists());
    assert!(
        output_root.join("lang_std_native.pyd").exists()
            || output_root.join("lang_std_native.so").exists()
    );
}

#[test]
fn rejects_unknown_std_imports() {
    assert_project_build_error(
        &[(
            "main.lang",
            r#"
from std.text import screaming
"#,
        )],
        "main.lang",
        "does not publicly export 'screaming'",
    );
}
