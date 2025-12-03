
# Initial build
jenga build -a

# Run the executable
jenga exec main.exe

# Inspect the generated deps
jenga build -mq && find ,jenga -name '*.d' | xargs cat

# Make a change; rebuild
rm defs.h && echo '#define MY_CONST 11' > defs.h
jenga build -a

# And rerun
jenga exec main.exe
