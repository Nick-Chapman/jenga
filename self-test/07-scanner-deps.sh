
# Build
jenga build -a

# Run
jenga exec main.exe

# Inspect the generated deps
jenga cat main.d
jenga cat fib.d

# Make a change; rebuild
rm defs.h && echo '#define MY_CONST 11' > defs.h
jenga build -a

# Rerun
jenga exec -a main.exe
