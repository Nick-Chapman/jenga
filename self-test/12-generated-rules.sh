
# Build
jenga build -a

# Run
jenga exec hello.exe

# Change & rereun
sed -i 's/10/11/' defs.h
jenga exec hello.exe

# Targets
jenga list-targets

# Rules
jenga list-rules
