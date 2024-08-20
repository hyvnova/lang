# Custom builtins
import sys
sys.path.append('./src/transpilers/python/custom_builtins/')
from class_behaviors import * 
from iterator import * 
from option import * 
from signals import * 
from typecasting import * 
# End of custom builtins



a = Signal(lambda a: 1) 

b = Signal(lambda b: a.value, a) 


print(a.value, b.value)

a.update(lambda a: b.value, b) 


print(a.value, b.value)