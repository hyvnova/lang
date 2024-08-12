from typing import Any
from option import Some, Nothing, Option


def cast_to(value: Any, type_name: str) -> Option:
    """
    Casts a value to a type. Returns a Option (Some or None) depending on if the casting can fail.
    """
    try:
        return Some(type_name(value))
    except ValueError:
        return Nothing