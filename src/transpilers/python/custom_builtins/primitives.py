import math


def _wrap(value):
    if isinstance(value, (Str, Num, Bool, Vec, Map, Set)):
        return value
    if isinstance(value, str):
        return Str(value)
    if isinstance(value, bool):
        return Bool(value)
    if isinstance(value, (int, float)):
        return Num(value)
    if isinstance(value, list):
        return Vec(value)
    if isinstance(value, dict):
        return Map(value)
    if isinstance(value, set):
        return Set(value)
    if isinstance(value, tuple):
        return Vec(value)
    return value


def _unwrap(value):
    if isinstance(value, Str):
        return str(value)
    if isinstance(value, Num):
        return value.raw
    if isinstance(value, Bool):
        return bool(value)
    if isinstance(value, Vec):
        return [_unwrap(item) for item in value]
    if isinstance(value, Map):
        return {_unwrap(key): _unwrap(item) for key, item in value.items()}
    if isinstance(value, Set):
        return {_unwrap(item) for item in value}
    return value


def wrap(value):
    return _wrap(value)


def unwrap(value):
    return _unwrap(value)


class PrimitiveMixin:
    def tap(self, func):
        func(self)
        return self

    def pipe(self, func):
        return _wrap(func(self))

    @property
    def raw(self):
        return _unwrap(self)


class Str(str, PrimitiveMixin):
    def __new__(cls, value=""):
        return super().__new__(cls, str(_unwrap(value)))

    @property
    def len(self):
        return Num(len(self))

    @property
    def length(self):
        return self.len

    def strip(self):
        return Str(super().strip())

    def split(self, sep=None):
        if sep is None:
            return Vec(Str(item) for item in super().split())
        return Vec(Str(item) for item in super().split(str(_unwrap(sep))))

    def lines(self):
        return Vec(Str(item) for item in super().splitlines())

    def words(self):
        return self.split()

    def upper(self):
        return Str(super().upper())

    def lower(self):
        return Str(super().lower())

    def title(self):
        return Str(super().title())

    def replace(self, old, new):
        return Str(super().replace(str(_unwrap(old)), str(_unwrap(new))))

    def contains(self, needle):
        return Bool(str(_unwrap(needle)) in self)

    def starts_with(self, prefix):
        return Bool(super().startswith(str(_unwrap(prefix))))

    def ends_with(self, suffix):
        return Bool(super().endswith(str(_unwrap(suffix))))

    def __add__(self, other):
        return Str(str(self) + str(_unwrap(other)))

    def __radd__(self, other):
        return Str(str(_unwrap(other)) + str(self))


class Num(PrimitiveMixin):
    def __init__(self, value=0):
        value = _unwrap(value)
        self.raw = float(value) if isinstance(value, float) else int(value)

    @property
    def raw(self):
        return self._raw

    @raw.setter
    def raw(self, value):
        self._raw = value

    def _other(self, other):
        return _unwrap(other)

    def _binary(self, other, op):
        return Num(op(self.raw, self._other(other)))

    def __add__(self, other):
        return self._binary(other, lambda left, right: left + right)

    def __radd__(self, other):
        return Num(self._other(other) + self.raw)

    def __sub__(self, other):
        return self._binary(other, lambda left, right: left - right)

    def __rsub__(self, other):
        return Num(self._other(other) - self.raw)

    def __mul__(self, other):
        return self._binary(other, lambda left, right: left * right)

    def __rmul__(self, other):
        return Num(self._other(other) * self.raw)

    def __truediv__(self, other):
        return self._binary(other, lambda left, right: left / right)

    def __rtruediv__(self, other):
        return Num(self._other(other) / self.raw)

    def __mod__(self, other):
        return self._binary(other, lambda left, right: left % right)

    def __pow__(self, other):
        return self._binary(other, lambda left, right: left ** right)

    def __neg__(self):
        return Num(-self.raw)

    def __pos__(self):
        return Num(+self.raw)

    def __lt__(self, other):
        return Bool(self.raw < self._other(other))

    def __le__(self, other):
        return Bool(self.raw <= self._other(other))

    def __gt__(self, other):
        return Bool(self.raw > self._other(other))

    def __ge__(self, other):
        return Bool(self.raw >= self._other(other))

    def __eq__(self, other):
        return self.raw == self._other(other)

    def __ne__(self, other):
        return self.raw != self._other(other)

    def __hash__(self):
        return hash(self.raw)

    def __bool__(self):
        return bool(self.raw)

    def __int__(self):
        return int(self.raw)

    def __float__(self):
        return float(self.raw)

    def __index__(self):
        return int(self.raw)

    def __str__(self):
        if isinstance(self.raw, float) and self.raw.is_integer():
            return str(int(self.raw))
        return str(self.raw)

    def __repr__(self):
        return str(self)

    def abs(self):
        return Num(abs(self.raw))

    def round(self):
        return Num(round(self.raw))

    def floor(self):
        return Num(math.floor(self.raw))

    def ceil(self):
        return Num(math.ceil(self.raw))

    def sqrt(self):
        return Num(math.sqrt(self.raw))

    def clamp(self, low, high):
        return Num(min(max(self.raw, self._other(low)), self._other(high)))


class Bool(PrimitiveMixin):
    def __init__(self, value=False):
        self.value = bool(_unwrap(value))

    @property
    def raw(self):
        return self.value

    def __bool__(self):
        return self.value

    def __str__(self):
        return "True" if self.value else "False"

    def __repr__(self):
        return str(self)

    def __eq__(self, other):
        return self.value == bool(_unwrap(other))

    def __hash__(self):
        return hash(self.value)


class Vec(list, PrimitiveMixin):
    def __init__(self, values=None):
        super().__init__(_wrap(item) for item in (values or []))

    @property
    def len(self):
        return Num(len(self))

    @property
    def length(self):
        return self.len

    @property
    def raw(self):
        return [_unwrap(item) for item in self]

    def __getitem__(self, index):
        return _wrap(super().__getitem__(int(_unwrap(index))))

    def map(self, func):
        return Vec(func(item) for item in self)

    def filter(self, func):
        return Vec(item for item in self if func(item))

    def fold(self, initial, func):
        acc = initial
        for item in self:
            acc = func(acc, item)
        return _wrap(acc)

    def push(self, value):
        self.append(_wrap(value))
        return self

    def join(self, sep=""):
        return Str(str(_unwrap(sep)).join(str(_unwrap(item)) for item in self))

    def contains(self, value):
        return Bool(_wrap(value) in self)


class Map(dict, PrimitiveMixin):
    def __init__(self, values=None):
        super().__init__()
        for key, value in (values or {}).items():
            super().__setitem__(_wrap(key), _wrap(value))

    @property
    def len(self):
        return Num(len(self))

    @property
    def length(self):
        return self.len

    @property
    def raw(self):
        return {_unwrap(key): _unwrap(value) for key, value in self.items()}

    def get(self, key, default=None):
        return _wrap(super().get(_wrap(key), default))

    def set(self, key, value):
        super().__setitem__(_wrap(key), _wrap(value))
        return self

    def keys(self):
        return Vec(super().keys())

    def values(self):
        return Vec(super().values())

    def items(self):
        return Vec(Vec([key, value]) for key, value in super().items())

    def contains(self, key):
        return Bool(_wrap(key) in self)


class Set(set, PrimitiveMixin):
    def __init__(self, values=None):
        super().__init__(_wrap(item) for item in (values or []))

    @property
    def len(self):
        return Num(len(self))

    @property
    def length(self):
        return self.len

    @property
    def raw(self):
        return {_unwrap(item) for item in self}

    def add(self, value):
        super().add(_wrap(value))
        return self

    def contains(self, value):
        return Bool(_wrap(value) in self)

    def union(self, other):
        return Set(super().union(Set(other)))

    def intersection(self, other):
        return Set(super().intersection(Set(other)))
