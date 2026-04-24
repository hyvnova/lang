mod common;

use common::{assert_python_runs, parse_nodes, transpile};
use lang::ast::Node;
use std::process::Command;

const OBJECT_SOURCE: &str = r#"
struct User<T> {
    name: str,
    age: int,
}

trait Named<T> {
    fn label(self) -> str
}

impl<T> User<T> {
    fn rename(self, name: str) {
        self.name = name
    }
}

impl<T> Named<T> for User<T> {
    fn label(self) -> str {
        self.name
    }
}

user = User { name: "Ada", age: 36 }
"#;

#[test]
fn parses_rust_like_object_model_nodes() {
    let nodes = parse_nodes(OBJECT_SOURCE);

    let Node::StructDef { name, generics, fields, .. } = &nodes[0] else {
        panic!("expected struct definition, got {:#?}", nodes[0]);
    };
    assert_eq!(name, "User");
    assert_eq!(generics, &vec!["T".to_string()]);
    assert_eq!(fields[0].name, "name");
    assert_eq!(fields[0].type_ref.name, "str");
    assert_eq!(fields[1].name, "age");
    assert_eq!(fields[1].type_ref.name, "int");

    let Node::TraitDef { name, generics, methods, .. } = &nodes[1] else {
        panic!("expected trait definition, got {:#?}", nodes[1]);
    };
    assert_eq!(name, "Named");
    assert_eq!(generics, &vec!["T".to_string()]);
    assert_eq!(methods[0].name, "label");
    assert_eq!(methods[0].return_type.as_ref().unwrap().name, "str");

    let Node::ImplBlock { trait_ref, target, methods, .. } = &nodes[2] else {
        panic!("expected inherent impl block, got {:#?}", nodes[2]);
    };
    assert!(trait_ref.is_none());
    assert_eq!(target.name, "User");
    assert_eq!(methods[0].signature.name, "rename");

    let Node::ImplBlock { trait_ref, target, methods, .. } = &nodes[3] else {
        panic!("expected trait impl block, got {:#?}", nodes[3]);
    };
    assert_eq!(trait_ref.as_ref().unwrap().name, "Named");
    assert_eq!(target.name, "User");
    assert_eq!(methods[0].signature.name, "label");

    let Node::BindingDef { value, .. } = &nodes[4] else {
        panic!("expected struct literal assignment, got {:#?}", nodes[4]);
    };
    assert!(matches!(value.as_ref(), Node::StructInit { .. }));
}

#[test]
fn generated_python_runs_struct_and_trait_impl() {
    assert_python_runs(
        r#"
struct User<T> {
    name: str,
    age: int,
}

trait Named<T> {
    fn label(self) -> str
}

impl<T> User<T> {
    fn rename(self, name: str) {
        self.name = name
    }
}

impl<T> Named<T> for User<T> {
    fn label(self) -> str {
        self.name
    }
}

user = User { name: "Ada", age: 36 }
user.rename("Vey")
print(user.label())
"#,
        "Vey",
    );
}

#[test]
fn generated_python_rejects_invalid_struct_field_type() {
    let code = transpile(
        r#"
struct User {
    name: str,
    age: int,
}

user = User { name: "Ada", age: "old" }
"#,
    );

    let output = Command::new("py")
        .arg("-c")
        .arg(&code)
        .output()
        .or_else(|_| Command::new("python").arg("-c").arg(&code).output())
        .expect("failed to run Python");

    assert!(!output.status.success());
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("User.age"),
        "expected field type error, got stderr:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn generated_python_rejects_incomplete_trait_impl() {
    let code = transpile(
        r#"
struct User {}

trait Named {
    fn label(self) -> str
}

impl Named for User {}
"#,
    );

    let output = Command::new("py")
        .arg("-c")
        .arg(&code)
        .output()
        .or_else(|_| Command::new("python").arg("-c").arg(&code).output())
        .expect("failed to run Python");

    assert!(!output.status.success());
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("missing label"),
        "expected trait contract error, got stderr:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
}
