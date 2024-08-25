"""
Signals

Signals can listen to other signals.
When a signal which is being listened to changes, the listener signal is updated.

Example
```
Lang            | Python
----------------|------------------
$a = 1          | a = Signal(lambda a: 1)
$b = $a + 1     | b = Signal(lambda b: a.value + 1).listen_to(a)
$a = 2          | a.update(lambda a: 2)
print(a, b)     | print(a, b)

Output: 2, 3

"""


from typing import Iterable, List, Self, Set


class Signal:
    def __init__(self, callback, *deps):
        self.listeners: Set[Signal] = set()
        self.update(callback, *deps)

    def listen_to(self, deps: Iterable[Self]):
        """
        Listens to other signals
        """
        for signal in deps:
            signal.listeners.add(self)

    def update(self, callback, *deps):
        """
        Updates the callback of the signal
        """
        self.callback = callback
        self.listen_to(deps)
        self()


    def __call__(self):
        """
        Call when a signal being listened to changes
        """
        self.value = self.callback(self)
        for listener in self.listeners:
            listener()

    def __str__(self):
        return str(self.value)
    


"""
Reactive Statement
When a dependency changes, the code is re-executed
"""
class ReactiveStmt:
    def __init__(self, callback, *deps):
        for signal in deps:
            signal.listeners.add(self) 

        self.callback = callback
        self()

    def __call__(self):
        self.value = self.callback()
        return self.value

"""
Function Signals
- Relation between functions that depend on signals
- Relation between signals that depend on functions

Example
```
Lang            | Python
----------------|------------------
$a = 1          | a = Signal(lambda a: 1)
(n) -> $a + n   | f = lambda n: a.value + n
"""