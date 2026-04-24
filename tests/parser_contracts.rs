mod common;

use common::parse_nodes;
use lang::ast::Node;

#[test]
fn parses_member_assignment_as_member_access_lhs() {
    let nodes = parse_nodes("fn set(self, name) { self.name = name }");

    let Node::FunctionDef { body, .. } = &nodes[0] else {
        panic!("expected a function definition, got {:#?}", nodes);
    };

    let Node::FnBody(body_nodes) = body.as_ref() else {
        panic!("expected function body, got {:#?}", body);
    };

    let Node::Assign { identifiers, values, .. } = &body_nodes[0] else {
        panic!("expected assignment in function body, got {:#?}", body_nodes);
    };

    assert!(matches!(identifiers[0], Node::MemberAccess { .. }));
    assert!(matches!(values[0], Node::Identifier(_)));
}

#[test]
fn parses_member_access_expression_shape() {
    let nodes = parse_nodes("counter.value");

    let Node::MemberAccess { object, member } = &nodes[0] else {
        panic!("expected member access, got {:#?}", nodes);
    };

    assert!(matches!(object.as_ref(), Node::Identifier(name) if name == "counter"));
    assert!(matches!(member.as_ref(), Node::Identifier(name) if name == "value"));
}

#[test]
fn parses_range_distribution_with_range_as_distribution_arg() {
    let nodes = parse_nodes("1..10 -> print");

    let Node::Distribution { args, recipients } = &nodes[0] else {
        panic!("expected distribution, got {:#?}", nodes);
    };

    assert!(matches!(args[0], Node::Range { .. }));
    assert!(matches!(recipients[0], Node::Identifier(_)));
}

#[test]
fn parses_operator_precedence_into_ast_shape() {
    let nodes = parse_nodes("1 * 2 + 3");

    let Node::BinOp { lhs, op, rhs } = &nodes[0] else {
        panic!("expected binary operation, got {:#?}", nodes);
    };

    assert_eq!(op, "+");
    assert!(matches!(lhs.as_ref(), Node::BinOp { op, .. } if op == "*"));
    assert!(matches!(rhs.as_ref(), Node::Number(value) if value == "3"));
}

#[test]
fn parses_module_statements_and_public_bindings() {
    let nodes = parse_nodes(
        r#"
pub mod math
import .math.vec as vec
from app.math import value, Vec2
pub use app.math.vec.Vec2
pub answer = 42
"#,
    );

    assert!(matches!(&nodes[0], Node::ModuleDecl { name, public } if name == "math" && *public));
    assert!(matches!(&nodes[1], Node::ImportStmt { alias, .. } if alias.as_deref() == Some("vec")));
    assert!(matches!(&nodes[2], Node::FromImport { names, wildcard, .. } if !*wildcard && names.len() == 2));
    assert!(matches!(&nodes[3], Node::UseDecl { public, .. } if *public));
    assert!(matches!(&nodes[4], Node::BindingDef { name, public, .. } if name == "answer" && *public));
}
