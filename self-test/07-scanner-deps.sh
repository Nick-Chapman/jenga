
# Build
jenga build -a

# Run
jenga exec main.exe

# Inspect the generated deps
jenga build -mq && find ,jenga -name '*.d' | sort | xargs grep ^

# Make a change; rebuild
rm defs.h && echo '#define MY_CONST 11' > defs.h
jenga build -a

# Rerun
jenga exec main.exe
