//! Signature help is resolved from catalogs or local symbol details.

use crate::analysis::DocumentAnalysis;
use crate::lsp::convert::{markdown_docs, parameters_from_signature};
use crate::primitive_catalog::primitive_member;
use crate::std_catalog::std_export;
use tower_lsp::lsp_types::{SignatureHelp, SignatureInformation};

pub fn signature_help_at(analysis: &DocumentAnalysis, context: &str) -> Option<SignatureHelp> {
    let (callee, args) = context.rsplit_once('(')?;
    let callee = callee.trim();
    let callee_name = callee.rsplit('.').next().unwrap_or(callee);
    let active_parameter = args.chars().filter(|ch| *ch == ',').count() as u32;

    if let Some((module, export)) = std_signature_target(callee) {
        if let Some(export) = std_export(module, export) {
            return Some(signature_help_from_doc(&export.docs, active_parameter));
        }
    }

    if callee.contains('.') {
        if let Some(member) = primitive_member(callee_name) {
            let signature = member.signature();
            return Some(SignatureHelp {
                signatures: vec![SignatureInformation {
                    label: signature.clone(),
                    documentation: Some(markdown_docs(member.markdown())),
                    parameters: parameters_from_signature(&signature),
                    active_parameter: Some(active_parameter),
                }],
                active_signature: Some(0),
                active_parameter: Some(active_parameter),
            });
        }
    }

    analysis
        .index
        .symbols
        .iter()
        .find(|symbol| symbol.name == callee_name)
        .and_then(|symbol| symbol.detail.as_deref())
        .map(|detail| SignatureHelp {
            signatures: vec![SignatureInformation {
                label: detail.to_string(),
                documentation: Some(markdown_docs(format!("```lang\n{detail}\n```"))),
                parameters: parameters_from_signature(detail),
                active_parameter: Some(active_parameter),
            }],
            active_signature: Some(0),
            active_parameter: Some(active_parameter),
        })
}

fn signature_help_from_doc(
    docs: &crate::api_docs::DocBlock,
    active_parameter: u32,
) -> SignatureHelp {
    SignatureHelp {
        signatures: vec![SignatureInformation {
            label: docs.signature.to_string(),
            documentation: Some(markdown_docs(docs.markdown())),
            parameters: parameters_from_signature(docs.signature),
            active_parameter: Some(active_parameter),
        }],
        active_signature: Some(0),
        active_parameter: Some(active_parameter),
    }
}

fn std_signature_target(callee: &str) -> Option<(&str, &str)> {
    let mut parts = callee.rsplit('.');
    let export = parts.next()?;
    let module = parts.next()?;
    let root = parts.next()?;
    (root == "std").then_some((module, export))
}
