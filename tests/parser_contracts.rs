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
