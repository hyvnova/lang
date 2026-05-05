use crate::api_docs::{DocBlock, DocExample};

pub const PRIMITIVE_TYPES: &[PrimitiveType] = &[
    PrimitiveType::new(
        "Str",
        DocBlock::full(
            "Lang-owned string value with Python-like behavior and method-first helpers.",
            "Str",
            &[],
            "A text value that behaves like a string and adds Lang methods.",
            "",
            &[DocExample::new(
                "Method-first text",
                "words = text.strip().words()\nprint(words.len)",
            )],
            "Prefer methods like `.strip()` and `.words()` over `std.text.*` in new code.",
        ),
        &[
            PrimitiveMethod::new("Str", "strip", "Trim leading and trailing whitespace"),
            PrimitiveMethod::new("Str", "split", "Split text by a separator"),
            PrimitiveMethod::new("Str", "lines", "Split text into lines"),
            PrimitiveMethod::new("Str", "words", "Split text into words"),
            PrimitiveMethod::new("Str", "upper", "Uppercase text"),
            PrimitiveMethod::new("Str", "lower", "Lowercase text"),
            PrimitiveMethod::new("Str", "title", "Title-case text"),
            PrimitiveMethod::new("Str", "replace", "Replace text"),
            PrimitiveMethod::new("Str", "contains", "Check whether text contains a value"),
            PrimitiveMethod::new("Str", "starts_with", "Check text prefix"),
            PrimitiveMethod::new("Str", "ends_with", "Check text suffix"),
            PrimitiveMethod::new("Str", "tap", "Run a side-effect callback and return self"),
            PrimitiveMethod::new("Str", "pipe", "Run a callback and return its result"),
        ],
        &[
            PrimitiveMethod::new("Str", "len", "Text length"),
            PrimitiveMethod::new("Str", "length", "Text length alias"),
            PrimitiveMethod::new("Str", "raw", "Underlying Python value"),
        ],
    ),
    PrimitiveType::new(
        "Vec",
        DocBlock::brief("Lang vector value with chainable collection helpers."),
        &[
            PrimitiveMethod::new("Vec", "map", "Map values"),
            PrimitiveMethod::new("Vec", "filter", "Filter values"),
            PrimitiveMethod::new("Vec", "fold", "Fold values into an accumulator"),
            PrimitiveMethod::new("Vec", "push", "Append a value"),
            PrimitiveMethod::new("Vec", "join", "Join values as text"),
            PrimitiveMethod::new("Vec", "contains", "Check whether a value exists"),
            PrimitiveMethod::new("Vec", "tap", "Run a side-effect callback and return self"),
            PrimitiveMethod::new("Vec", "pipe", "Run a callback and return its result"),
        ],
        &[
            PrimitiveMethod::new("Vec", "len", "Vector length"),
            PrimitiveMethod::new("Vec", "length", "Vector length alias"),
            PrimitiveMethod::new("Vec", "raw", "Underlying Python value"),
        ],
    ),
    PrimitiveType::new(
        "Map",
        DocBlock::brief("Lang map value with key/value helpers."),
        &[
            PrimitiveMethod::new("Map", "get", "Get a value"),
            PrimitiveMethod::new("Map", "set", "Set a value"),
            PrimitiveMethod::new("Map", "keys", "Map keys"),
            PrimitiveMethod::new("Map", "values", "Map values"),
            PrimitiveMethod::new("Map", "items", "Map item pairs"),
            PrimitiveMethod::new("Map", "contains", "Check whether a key exists"),
            PrimitiveMethod::new("Map", "tap", "Run a side-effect callback and return self"),
            PrimitiveMethod::new("Map", "pipe", "Run a callback and return its result"),
        ],
        &[
            PrimitiveMethod::new("Map", "len", "Map length"),
            PrimitiveMethod::new("Map", "length", "Map length alias"),
            PrimitiveMethod::new("Map", "raw", "Underlying Python value"),
        ],
    ),
    PrimitiveType::new(
        "Set",
        DocBlock::brief("Lang set value with membership and set-operation helpers."),
        &[
            PrimitiveMethod::new("Set", "add", "Add a value"),
            PrimitiveMethod::new("Set", "contains", "Check whether a value exists"),
            PrimitiveMethod::new("Set", "union", "Set union"),
            PrimitiveMethod::new("Set", "intersection", "Set intersection"),
            PrimitiveMethod::new("Set", "tap", "Run a side-effect callback and return self"),
            PrimitiveMethod::new("Set", "pipe", "Run a callback and return its result"),
        ],
        &[
            PrimitiveMethod::new("Set", "len", "Set length"),
            PrimitiveMethod::new("Set", "length", "Set length alias"),
            PrimitiveMethod::new("Set", "raw", "Underlying Python value"),
        ],
    ),
    PrimitiveType::new(
        "Num",
        DocBlock::brief("Lang number value with numeric helpers."),
        &[
            PrimitiveMethod::new("Num", "abs", "Absolute value"),
            PrimitiveMethod::new("Num", "round", "Round the number"),
            PrimitiveMethod::new("Num", "floor", "Floor the number"),
            PrimitiveMethod::new("Num", "ceil", "Ceiling of the number"),
            PrimitiveMethod::new("Num", "sqrt", "Square root"),
            PrimitiveMethod::new("Num", "clamp", "Clamp to a range"),
            PrimitiveMethod::new("Num", "tap", "Run a side-effect callback and return self"),
            PrimitiveMethod::new("Num", "pipe", "Run a callback and return its result"),
        ],
        &[PrimitiveMethod::new(
            "Num",
            "raw",
            "Underlying Python value",
        )],
    ),
    PrimitiveType::new(
        "Bool",
        DocBlock::brief("Lang boolean value."),
        &[
            PrimitiveMethod::new("Bool", "tap", "Run a side-effect callback and return self"),
            PrimitiveMethod::new("Bool", "pipe", "Run a callback and return its result"),
        ],
        &[PrimitiveMethod::new(
            "Bool",
            "raw",
            "Underlying Python value",
        )],
    ),
    PrimitiveType::new(
        "Result",
        DocBlock::brief("Fallible operation result: `Ok(value)` or `Err(error)`."),
        &[
            PrimitiveMethod::new("Result", "unwrap", "Return success value or fail"),
            PrimitiveMethod::new(
                "Result",
                "expect",
                "Return success value or fail with a message",
            ),
            PrimitiveMethod::new("Result", "unwrap_or", "Return success value or fallback"),
            PrimitiveMethod::new("Result", "is_ok", "Return whether this is Ok"),
            PrimitiveMethod::new("Result", "is_err", "Return whether this is Err"),
            PrimitiveMethod::new("Result", "map", "Map success value"),
            PrimitiveMethod::new("Result", "map_err", "Map error value"),
            PrimitiveMethod::new(
                "Result",
                "tap",
                "Run a side-effect callback and return self",
            ),
            PrimitiveMethod::new("Result", "pipe", "Run a callback and return its result"),
        ],
        &[PrimitiveMethod::new(
            "Result",
            "raw",
            "Underlying Python value",
        )],
    ),
    PrimitiveType::new(
        "Option",
        DocBlock::brief("Optional value: `Some(value)` or `Nothing`."),
        &[
            PrimitiveMethod::new("Option", "unwrap", "Return contained value or fail"),
            PrimitiveMethod::new(
                "Option",
                "expect",
                "Return contained value or fail with a message",
            ),
            PrimitiveMethod::new("Option", "unwrap_or", "Return contained value or fallback"),
            PrimitiveMethod::new("Option", "is_some", "Return whether this is Some"),
            PrimitiveMethod::new("Option", "is_nothing", "Return whether this is Nothing"),
            PrimitiveMethod::new("Option", "map", "Map contained value"),
            PrimitiveMethod::new(
                "Option",
                "tap",
                "Run a side-effect callback and return self",
            ),
            PrimitiveMethod::new("Option", "pipe", "Run a callback and return its result"),
        ],
        &[PrimitiveMethod::new(
            "Option",
            "raw",
            "Underlying Python value",
        )],
    ),
];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PrimitiveType {
    pub name: &'static str,
    pub docs: DocBlock,
    pub methods: &'static [PrimitiveMethod],
    pub properties: &'static [PrimitiveMethod],
}

impl PrimitiveType {
    pub const fn new(
        name: &'static str,
        docs: DocBlock,
        methods: &'static [PrimitiveMethod],
        properties: &'static [PrimitiveMethod],
    ) -> Self {
        Self {
            name,
            docs,
            methods,
            properties,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PrimitiveMethod {
    pub receiver: &'static str,
    pub name: &'static str,
    pub docs: DocBlock,
}

impl PrimitiveMethod {
    pub const fn new(receiver: &'static str, name: &'static str, summary: &'static str) -> Self {
        Self {
            receiver,
            name,
            docs: DocBlock::brief(summary),
        }
    }

    pub fn signature(&self) -> String {
        match (self.receiver, self.name) {
            ("Str", "replace") => "Str.replace(old: Str, new: Str) -> Str".to_string(),
            ("Str", "split") => "Str.split(sep: Str?) -> Vec<Str>".to_string(),
            ("Str", "words") => "Str.words() -> Vec<Str>".to_string(),
            ("Str", "lines") => "Str.lines() -> Vec<Str>".to_string(),
            (_, "tap") => format!("{}.tap(callback: Fn(Self) -> Any) -> Self", self.receiver),
            (_, "pipe") => format!("{}.pipe(callback: Fn(Self) -> T) -> T", self.receiver),
            (_, "len") | (_, "length") => format!("{}.{} -> Num", self.receiver, self.name),
            (_, "raw") => format!("{}.raw -> PythonValue", self.receiver),
            _ => format!("{}.{}(...) -> LangValue", self.receiver, self.name),
        }
    }

    pub fn markdown(&self) -> String {
        self.docs.markdown_with_signature(&self.signature())
    }
}

pub fn all_primitive_members() -> impl Iterator<Item = &'static PrimitiveMethod> {
    PRIMITIVE_TYPES
        .iter()
        .flat_map(|primitive| primitive.methods.iter().chain(primitive.properties.iter()))
}

pub fn primitive_member(name: &str) -> Option<&'static PrimitiveMethod> {
    all_primitive_members().find(|member| member.name == name)
}
