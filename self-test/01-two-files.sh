
# Rejig files into "example" subdirectory
mkdir example
mv build.jenga fib.c main.c example

# Build from clean
jenga build -a

# Run the built artifact
jenga exec -a example/main.exe

# Install the artifact
jenga install -a example/main.exe as.exe

# And run that
./as.exe

# Rebuild after no changes
jenga build -a

# Update main.c "world->UNIVERSE"; two compile actions run
sed -i 's/world/UNIVERSE/g' example/main.c
jenga exec -a example/main.exe

# Reverting to previous state of main.c; no actions run
sed -i 's/UNIVERSE/world/g' example/main.c
jenga exec -a example/main.exe

# Whitespace change to main.c; no link action run (early cutoff)
sed -i 's/int main/int      main/g' example/main.c
jenga build -a

# Update to link under a different name; new link action run
sed -i 's/main.exe/RENAMED.exe/' example/build.jenga
jenga exec -a example/RENAMED.exe

# Relocate the example to a new directory; no actions run
mv example RELOCATED
jenga build -a

# Duplicate example dir; twice #targets; but no actions run
cp -rp RELOCATED ANOTHER
jenga build -a

# Modify one of the example dirs; minimal rebuild as required
sed -i 's/fib(10)/fib(20)/g' RELOCATED/main.c
jenga build -a

# Run the two co-existing executabls
jenga exec RELOCATED/RENAMED.exe
jenga exec ANOTHER/RENAMED.exe

# View the targets
jenga list-targets

# Corrupt one build.jenga; see parse error
chmod +w RELOCATED/build.jenga && echo xxx >> RELOCATED/build.jenga
jenga build -a

# Remove one directory copy; view targets again
rm -rf RELOCATED
jenga list-targets

# Mod some more, try -q
sed -i 's/fib(10)/fib(11)/g' ANOTHER/main.c
jenga build -q
jenga exec -a ANOTHER/RENAMED.exe

# Help
jenga --help
