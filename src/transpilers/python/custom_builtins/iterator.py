"""
Provides a Iterator class with methods that mimic the built-in functions filter, map, and reduce.

Syntax Changes:
    From: filter(function, iterable)
    To: iterable.filter(function) 
"""

from typing import Iterable, Callable, Self, Type, Union


def reduce(
    func: Callable[[object, object], object],
    iterable: Iterable[object],
    initializer: Union[object, None] = None,
) -> Union[object, None]:
    """
    Apply a function to pairs of elements in an iterable to reduce the iterable to a single value.

    Args:
    iterable: An iterable object to reduce to a single value.
    func: A function that accepts two arguments and returns a value.
    initializer: An optional initial value for the reduction.

    Returns:
    The final value after reducing the iterable with the function.

    """
    iterator = iter(iterable)
    if initializer is None:
        try:
            initializer = next(iterator)
        except StopIteration:
            raise TypeError("reduce() of empty sequence with no initial value")
    accumulator = initializer
    for element in iterator:
        accumulator = func(accumulator, element)
    return accumulator


class Iterator:
    """
    A class that provides methods that mimic the built-in functions filter, map, and reduce.

    Examples:
    ```
    i = Iterator([1, 2, 3, 4, 5])
    i.filter(lambda x: x % 2 == 0) # [2, 4]
    i.map(lambda x: x ** 2) # [1, 4, 9, 16, 25]
    i.reduce(lambda x, y: x + y) # 15
    i.any(lambda x: x % 2 == 0) # True
    i.all(lambda x: x % 2 == 0) # False
    ```
    """

    def __init__(self, iterable: Iterable) -> None:
        self._iterable = iterable

    def convert_to(self, t: Type) -> Type:
        """
        #### Returns the iterator as the specified type.
        Example:
        ```
        i = Iterator([1, 2, 3, 4, 5])
        i.to(set)
        ```
        """
        return t(self._iterable)

    def to_set(self) -> set:
        """
        #### Returns the iterator as a set.
        """
        return set(self._iterable)

    def to_list(self) -> list:
        """
        #### Returns the iterator as a list.
        """
        return list(self._iterable)

    def to_tuple(self) -> tuple:
        """
        #### Returns the iterator as a tuple.
        """
        return tuple(self._iterable)

    def __iter__(self) -> Iterable:
        return iter(self._iterable)

    def __next__(self) -> object:
        return next(self._iterable)

    def __getitem__(self, key: Union[int, slice]) -> object:
        return self._iterable[key]
    
    def __setitem__(self, key: Union[int, slice], value: object) -> None:
        self._iterable[key] = value

    def __delitem__(self, key: Union[int, slice]) -> None:
        del self._iterable[key]

    def __len__(self) -> int:
        return len(self._iterable)

    def __contains__(self, item: object) -> bool:
        return item in self._iterable

    def __reversed__(self) -> Iterable:
        return reversed(self._iterable)

    def __str__(self) -> str:
        return str(self._iterable)

    def __repr__(self) -> str:
        return repr(self._iterable)

    def __eq__(self, other: Iterable) -> bool:
        return self._iterable == other

    def __ne__(self, other: Iterable) -> bool:
        return self._iterable != other

    def __lt__(self, other: Iterable) -> bool:
        return self._iterable < other

    def __le__(self, other: Iterable) -> bool:
        return self._iterable <= other

    def __gt__(self, other: Iterable) -> bool:
        return self._iterable > other

    def __ge__(self, other: Iterable) -> bool:
        return self._iterable >= other

    def __add__(self, other: Iterable) -> Iterable:
        return tuple(map(lambda x: x + other, self._iterable))

    def __sub__(self, other: Iterable) -> Iterable:
        return tuple(map(lambda x: x - other, self._iterable))
    
    def __isub__(self, other: Iterable) -> Iterable:
        self._iterable -= other

    def __iadd__(self, other: Iterable) -> Iterable:
        self._iterable += other

    def __mul__(self, other: int) -> Iterable:
        return tuple(map(lambda x: x * other, self._iterable))

    def __imul__(self, other: int) -> Iterable:
        self._iterable *= other

    def __rmul__(self, other: int) -> Iterable:
        return tuple(map(lambda x: other * x, self._iterable))
    
    def __floordiv__(self, other: int) -> Iterable:
        return tuple(map(lambda x: x // other, self._iterable))

    def __ifloordiv__(self, other: int) -> Iterable:
        self._iterable //= other
    
    def __truediv__(self, other: int) -> Iterable:
        return tuple(map(lambda x: x / other, self._iterable))
    
    def __itruediv__(self, other: int) -> Iterable:
        self._iterable /= other

    def filter(self, func: Callable[[object], bool]) -> Type[Self]:
        """
        Filter elements from an iterable based on a function.

        Args:
        iterable: An iterable object to filter elements from.
        func: A function that accepts one argument and returns a Boolean value.

        Returns:
        A list of elements from the iterable that satisfy the function.

        """
        return Iterator(list(filter(func, self._iterable)))

    def map(self, func: Callable[[object], object]) -> Type[Self]:
        """
        Apply a function to each element in an iterable.

        Args:
        iterable: An iterable object to apply the function to.
        func: A function that accepts one argument and returns a value.

        Returns:
        A list of values returned by the function for each element in the iterable.

        """
        return Iterator(list(map(func, self._iterable)))

    def reduce(
        self,
        func: Callable[[object, object], object],
        initializer: Union[object, None] = None,
    ) -> Union[object, None]:
        """
        Apply a function to pairs of elements in an iterable to reduce the iterable to a single value.

        Args:
        iterable: An iterable object to reduce to a single value.
        func: A function that accepts two arguments and returns a value.
        initializer: An optional initial value for the reduction.

        Returns:
        The final value after reducing the iterable with the function.

        """
        return reduce(func, self._iterable, initializer)

    def any(self, func: Callable[[object], bool] = lambda x: x) -> bool:
        """
        Check if at least one element in an iterable satisfies a function.

        Args:
        iterable: An iterable object to check for any satisfying elements.
        func: A function that accepts one argument and returns a Boolean value.

        Returns:
        True if at least one element satisfies the function, False otherwise.

        """
        return any(map(func, self._iterable))

    def all(self, func: Callable[[object], bool] = lambda x: x) -> bool:
        """
        Check if all elements in an iterable satisfy a function.

        Args:
        iterable: An iterable object to check for all satisfying elements.
        func: A function that accepts one argument and returns a Boolean value.

        Returns:
        True if all elements satisfy the function, False otherwise.

        """
        return all(map(func, self._iterable))
