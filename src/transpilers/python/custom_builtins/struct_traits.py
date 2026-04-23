"""
Runtime helpers for Rust-like structs and traits emitted by the Lang Python
transpiler.
"""

from typing import Dict, Iterable, Type


__all__ = [
    "__lang_register_trait__",
    "__lang_register_impl__",
    "__lang_check_struct_fields__",
    "__lang_check_args__",
    "__lang_check_value__",
]


__lang_traits__: Dict[str, set[str]] = {}


def __lang_register_trait__(name: str, generics: list[str], methods: Iterable[str]) -> None:
    __lang_traits__[name] = set(methods)


def __lang_register_impl__(trait_name: str, target: Type, methods: Iterable[str]) -> None:
    required = __lang_traits__.get(trait_name)
    if required is None:
        raise TypeError(f"Trait {trait_name} is not defined")

    provided = set(methods)
    missing = sorted(required - provided)
    if missing:
        missing_text = ", ".join(missing)
        raise TypeError(f"{target.__name__} does not implement {trait_name}: missing {missing_text}")

    for method in required:
        if not callable(getattr(target, method, None)):
            raise TypeError(f"{target.__name__}.{method} must be callable for trait {trait_name}")


def __lang_check_struct_fields__(struct_name: str, field_types: dict[str, object], values: dict[str, object]) -> None:
    for field_name, expected in field_types.items():
        __lang_check_value__(f"{struct_name}.{field_name}", values.get(field_name), expected)


def __lang_check_args__(function_name: str, arg_types: dict[str, object], values: dict[str, object]) -> None:
    for arg_name, expected in arg_types.items():
        __lang_check_value__(f"{function_name}.{arg_name}", values.get(arg_name), expected)


def __lang_check_value__(label: str, value: object, expected: object) -> None:
    if expected is None:
        return

    if not isinstance(value, expected):
        expected_name = getattr(expected, "__name__", str(expected))
        actual_name = type(value).__name__
        raise TypeError(f"{label} expected {expected_name}, got {actual_name}")
