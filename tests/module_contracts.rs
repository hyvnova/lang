mod common;

use common::{
    assert_project_build_error, assert_project_runs, build_project_fixture, build_temp_project,
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
    assert!(output_root.join("app").join("math").join("__init__.py").exists());
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
