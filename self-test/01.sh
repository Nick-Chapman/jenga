
# Script of self-test jenga commands to be executed for example 01

# Initial build...
./jenga build -a

# Exepected zero re-build...
./jenga build -a

# Remove cache and build again
rm -rf .cache
./jenga build -a
