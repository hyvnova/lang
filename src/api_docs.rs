#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DocBlock {
    pub summary: &'static str,
    pub signature: &'static str,
    pub params: &'static [DocParam],
    pub returns: &'static str,
    pub errors: &'static str,
    pub examples: &'static [DocExample],
    pub notes: &'static str,
}

impl DocBlock {
    pub const fn brief(summary: &'static str) -> Self {
        Self {
            summary,
            signature: "Lang API item",
            params: &[],
            returns: "A Lang runtime value.",
            errors: "",
            examples: &[DocExample {
                title: "Example",
                code: "value = std.core.debug(\"Lang\")",
            }],
            notes: "",
        }
    }

    pub const fn module(signature: &'static str, summary: &'static str) -> Self {
        Self {
            summary,
            signature,
            params: &[],
            returns: "A module namespace available from the global `std` object.",
            errors: "",
            examples: &[DocExample {
                title: "Example",
                code: "value = std.fs.exists(\"input.txt\")",
            }],
            notes: "Use `std.<module>.<name>` without importing `std`; Lang exposes it globally.",
        }
    }

    pub const fn full(
        summary: &'static str,
        signature: &'static str,
        params: &'static [DocParam],
        returns: &'static str,
        errors: &'static str,
        examples: &'static [DocExample],
        notes: &'static str,
    ) -> Self {
        Self {
            summary,
            signature,
            params,
            returns,
            errors,
            examples,
            notes,
        }
    }

    pub fn markdown(&self) -> String {
        self.markdown_with_signature(self.signature)
    }

    pub fn markdown_with_signature(&self, signature: &str) -> String {
        let mut sections = Vec::new();
        sections.push(self.summary.to_string());
        sections.push(format!("```lang\n{}\n```", signature));

        if !self.params.is_empty() {
            let params = self
                .params
                .iter()
                .map(|param| format!("- `{}`: {}", param.name, param.description))
                .collect::<Vec<_>>()
                .join("\n");
            sections.push(format!("**Parameters**\n\n{params}"));
        }

        if !self.returns.is_empty() {
            sections.push(format!("**Returns**\n\n{}", self.returns));
        }

        if !self.errors.is_empty() {
            sections.push(format!("**Errors**\n\n{}", self.errors));
        }

        if !self.examples.is_empty() {
            let examples = self
                .examples
                .iter()
                .map(|example| format!("**{}**\n\n```lang\n{}\n```", example.title, example.code))
                .collect::<Vec<_>>()
                .join("\n\n");
            sections.push(format!("**Examples**\n\n{examples}"));
        }

        if !self.notes.is_empty() {
            sections.push(format!("**Notes**\n\n{}", self.notes));
        }

        sections.join("\n\n")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DocParam {
    pub name: &'static str,
    pub description: &'static str,
}

impl DocParam {
    pub const fn new(name: &'static str, description: &'static str) -> Self {
        Self { name, description }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DocExample {
    pub title: &'static str,
    pub code: &'static str,
}

impl DocExample {
    pub const fn new(title: &'static str, code: &'static str) -> Self {
        Self { title, code }
    }
}
