# Custom builtins
import sys
sys.path.append('./src/transpilers/python/custom_builtins/')
from class_behaviors import * 
from iterator import * 
from option import * 
from signals import * 
from typecasting import * 
# End of custom builtins

#  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10

x = Signal(lambda x: Iterator(tuple(range(0, 10)))) 


#  0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20

y = Signal(lambda y: x.value.map(lambda n: n * 2)
, x) 


def _lang_reactive_stmt():
	print()
	print(x.value, y.value, sep = "\n")

ReactiveStmt(_lang_reactive_stmt, y)

x.update(lambda x: Iterator(tuple(range(2, 20)))) 


