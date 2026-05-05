mod common;

use common::{assert_python_runs, expand_tokens, parse_nodes};
use lang::ast::Node;

#[test]
fn expands_derive_debug_into_regular_object_nodes() {
    let nodes = parse_nodes(
        r#"
#derive(debug)
struct Square {
    w: int,
    h: int,
}
"#,
    );

    assert!(matches!(&nodes[0], Node::TraitDef { name, .. } if name == "Debug"));
    assert!(matches!(&nodes[1], Node::StructDef { name, .. } if name == "Square"));

    let Node::ImplBlock {
        trait_ref,
        target,
        methods,
        ..
    } = &nodes[2]
    else {
        panic!("expected derived Debug impl, got {:#?}", nodes);
    };
    assert_eq!(
        trait_ref.as_ref().map(|value| value.name.as_str()),
        Some("Debug")
    );
    assert_eq!(target.name, "Square");
    assert_eq!(methods[0].signature.name, "debug");

    let Node::ImplBlock {
        trait_ref,
        target,
        methods,
        ..
    } = &nodes[3]
    else {
        panic!("expected inherent impl, got {:#?}", nodes);
    };
    assert!(trait_ref.is_none());
    assert_eq!(target.name, "Square");
    assert_eq!(
        methods
            .iter()
            .map(|method| method.signature.name.as_str())
            .collect::<Vec<&str>>(),
        vec!["__str__", "__repr__"]
    );
}

#[test]
fn derive_debug_runtime_uses_stable_field_order_for_display_and_repr() {
    assert_python_runs(
        r#"
#derive(debug)
struct Square {
    w: int,
    h: int,
}

square = Square { w: 3, h: 4 }
print(square.debug())
print(square)
print(square.__repr__())
"#,
        "Square { w: 3, h: 4 }\nSquare { w: 3, h: 4 }\nSquare { w: 3, h: 4 }",
    );
}

#[test]
fn expands_debug_prelude_only_once_per_module() {
    let tokens = expand_tokens(
        r#"
#derive(debug)
struct Square { w: int }

#derive(debug)
struct Circle { r: int }
"#,
    )
    .expect("macro expansion should succeed");

    let debug_traits = tokens
        .iter()
        .filter(|(kind, value)| kind == "TRAIT" && value == "trait")
        .count();

    assert_eq!(debug_traits, 1);
}

#[test]
fn rejects_unknown_attribute_macro_name() {
    let error = expand_tokens(
        r#"
#unknown(debug)
struct Square { w: int }
"#,
    )
    .expect_err("unknown macro should fail");

    assert!(error.message.contains("Unknown attribute macro"));
}

#[test]
fn rejects_unknown_derive_name() {
    let error = expand_tokens(
        r#"
#derive(display)
struct Square { w: int }
"#,
    )
    .expect_err("unknown derive should fail");

    assert!(error.message.contains("Unknown derive"));
}

#[test]
fn rejects_malformed_derive_argument_list() {
    let error = expand_tokens(
        r#"
#derive(,)
struct Square { w: int }
"#,
    )
    .expect_err("malformed derive should fail");

    assert!(error.message.contains("Expected a derive name"));
}

#[test]
fn rejects_derive_debug_on_trait_targets() {
    let error = expand_tokens(
        r#"
#derive(debug)
trait Debuggable {
    fn debug(self) -> str
}
"#,
    )
    .expect_err("derive(debug) on a trait should fail");

    assert!(error.message.contains("only supports structs"));
}

#[test]
fn rejects_attributes_without_supported_attached_items() {
    let error = expand_tokens("#derive(debug)\n")
        .expect_err("attribute without a following item should fail");

    assert!(error.message.contains("Expected a struct or trait"));
}
