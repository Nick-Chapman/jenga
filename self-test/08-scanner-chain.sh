
# Build
jenga build -a

# Run
jenga exec main.exe

# Mod-A (change const value)
rm defs2.h && echo '#define MY_CONST 11' > defs2.h
jenga exec main.exe

# Mod-B (shorten the chain)
rm defs.h && echo '#define MY_CONST 12' > defs.h
jenga exec main.exe

# Mod-C (repoint the chain)
echo '#define MY_CONST 13' > defs3.h
echo '#include "defs3.h"' > defs.h
jenga exec main.exe
