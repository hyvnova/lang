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

class Signal:
    def __init__(self, callback):
        self.listeners: List[Signal] = []
        self.callback = callback
        self.value = callback(self)

    def listen_to(self, *signals):
        """
        Makes this signal listen to another signal
        """
        for signal in signals:
            signal.listeners.append(self)
        
        return self

    def update(self, callback):
        """
        Updates the callback of the signal
        """
        self.callback = callback
        self()
        return self
    
    def __call__(self):
        """
        Call when a signal being listened to changes
        """
        self.value = self.callback(self)
        for listener in self.listeners:
            listener()

    def __str__(self):
        return str(self.value)