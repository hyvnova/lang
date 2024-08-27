# Custom builtins
import sys
sys.path.append('./src/transpilers/python/custom_builtins/')
from class_behaviors import * 
from iterator import * 
from option import * 
from signals import * 
from typecasting import * 
# End of custom builtins

y = Signal(lambda y: 1) 


def f(x):
	return y.value + x

def _lang_reactive_stmt():
	print(f(2))
ReactiveStmt(_lang_reactive_stmt, )

y.update(lambda y: y.value + n) 

(Iterator(tuple(range(0, 10)))).map(lambda n: )
