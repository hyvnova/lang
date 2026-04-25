"""Runtime helpers for Lang module namespace objects."""

from types import ModuleType


__all__ = ["__lang_wrap_module__"]


class __LangModuleProxy__:
    def __init__(self, module: ModuleType, module_name: str) -> None:
        object.__setattr__(self, "_module", module)
        object.__setattr__(self, "_module_name", module_name)

    def __getattr__(self, name: str) -> object:
        return getattr(self._module, name)

    def __setattr__(self, name: str, value: object) -> None:
        if name in {"_module", "_module_name"}:
            object.__setattr__(self, name, value)
            return

        setattr(self._module, name, value)

    def __str__(self) -> str:
        return __lang_module_display__(self._module, self._module_name)

    def __repr__(self) -> str:
        return __lang_module_display__(self._module, self._module_name)


def __lang_wrap_module__(module: object, module_name: str) -> object:
    if isinstance(module, __LangModuleProxy__):
        return module

    return __LangModuleProxy__(module, module_name)


def __lang_module_display__(module: ModuleType, module_name: str) -> str:
    value = getattr(module, "__module_str__", None)
    if value is None:
        return f"<module {module_name}>"

    if isinstance(value, str):
        return value

    actual_name = type(value).__name__
    raise TypeError(f"Module {module_name} __module_str__ must be str, got {actual_name}")
