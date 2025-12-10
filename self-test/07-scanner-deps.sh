
# Build
jenga build -a

# Run
jenga exec main.exe

# Inspect the generated deps
jenga cat main.d
jenga cat fib.d

# Make a change
rm defs.h
echo '#define MY_CONST 13' > defs.h

# rebuild
jenga build -a

# Rerun
jenga exec -a main.exe
