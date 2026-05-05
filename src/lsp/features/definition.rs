//! Go-to-definition reads the definition index built during analysis.

use crate::analysis::{DefinitionTarget, DocumentAnalysis};

pub fn definition_at(analysis: &DocumentAnalysis, word: &str) -> Option<DefinitionTarget> {
    analysis
        .index
        .definitions
        .iter()
        .find(|definition| definition.name == word)
        .cloned()
}
