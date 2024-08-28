# Custom builtins
import sys
sys.path.append('./src/transpilers/python/custom_builtins/')
from class_behaviors import * 
from iterator import * 
from option import * 
from signals import * 
from typecasting import * 
# End of custom builtins

#  Fibonacci


a = Signal(lambda a: 1) 


b = Signal(lambda b: 1) 


def _lang_reactive_stmt():
	a.update(lambda a: a.value + b.value, b)
	
	print(a.value)
	b.update(lambda b: b.value + a.value, a)
	
ReactiveStmt(_lang_reactive_stmt, a, b)

