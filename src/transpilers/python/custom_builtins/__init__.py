
from pathlib import Path
from os import listdir

HERE = Path(__file__).parent

def set_builtins(source_file: Path) -> None:

    with open(source_file, 'r+') as f:
        content = f.read()
        f.seek(0, 0)
        
        # add custom_builtins path to sys
        f.write(f"""# [BUILTINS IMPORT]
import sys
sys.path.append(r"{HERE.resolve().__str__()}")
""")
        
        for file in listdir(HERE):
            
            if file.startswith("_"): # skip files that startswith "_"
                continue

            name = file[:-3]

            f.write(f"from {name} import *\n")

        f.write("# [END BUILTINS IMPORT]\n\n" + content)
