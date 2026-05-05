use lang::primitive_catalog::PRIMITIVE_TYPES;
use lang::std_catalog::{std_export, std_module, STD_MODULES};

#[test]
fn std_catalog_exports_have_rich_docs() {
    for module in STD_MODULES {
        assert!(
            !module.docs.summary.is_empty(),
            "std.{} is missing a summary",
            module.name
        );
        for export in module.exports {
            assert!(
                !export.docs.summary.is_empty(),
                "std.{}.{} is missing a summary",
                module.name,
                export.name
            );
            assert!(
                !export.docs.signature.is_empty(),
                "std.{}.{} is missing a signature",
                module.name,
                export.name
            );
            assert!(
                !export.docs.returns.is_empty(),
                "std.{}.{} is missing return docs",
                module.name,
                export.name
            );
            assert!(
                !export.docs.examples.is_empty(),
                "std.{}.{} is missing an example",
                module.name,
                export.name
            );
        }
    }
}

#[test]
fn primitive_catalog_members_have_receiver_docs() {
    for primitive in PRIMITIVE_TYPES {
        assert!(
            !primitive.docs.summary.is_empty(),
            "{} is missing type docs",
            primitive.name
        );
        for member in primitive.methods.iter().chain(primitive.properties.iter()) {
            assert!(
                !member.docs.signature.is_empty(),
                "{}.{} is missing a signature",
                primitive.name,
                member.name
            );
            assert_eq!(
                member.receiver, primitive.name,
                "{}.{} has the wrong receiver",
                primitive.name, member.name
            );
        }
    }
}

#[test]
fn read_text_docs_render_markdown_with_result_example() {
    let export = std_export("fs", "read_text").expect("read_text docs should exist");
    let markdown = export.docs.markdown();

    assert!(markdown.contains("```lang"));
    assert!(markdown.contains("std.fs.read_text(path: Str) -> Result<Str, Err>"));
    assert!(markdown.contains("Returns"));
    assert!(markdown.contains("Errors"));
    assert!(markdown.contains(".expect("));
}

#[test]
fn std_module_docs_describe_global_object() {
    let module = std_module("fs").expect("std.fs docs should exist");
    let markdown = module.docs.markdown();

    assert!(markdown.contains("std.fs"));
    assert!(markdown.contains("global `std` object"));
}
