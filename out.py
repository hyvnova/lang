# Custom builtins
import sys
sys.path.append('./src/transpilers/python/custom_builtins/')
from class_behaviors import * 
from iterator import * 
from option import * 
from signals import * 
from typecasting import * 
# End of custom builtins

x = Signal(lambda x: Iterator(tuple(range(0, 10)))) 


y = Signal(lambda y: x.value.map(lambda n: n * 2)
, x) 


print(x, y, sep = "\n")

