
# Build
jenga build -a

# Zero build
jenga build -a

# Run (currently jenga exec doesn't like the exit 25)
# jenga exec main.exe
jenga install main.exe main.exe.MAT && ./main.exe.MAT

# Define CFLAGS; rebuilds
echo '-O2' > CFLAGS
jenga build -a

# Change CFLAGS; rebuilds; warning displayed
echo '-Wall' > CFLAGS
jenga build -a

# Try again: no actions; warning redisplayed
jenga build -a

# Change CFLAGS; rebuilds; error displayed; build failed
echo '-Wall -Werror' > CFLAGS
jenga build -a

# Try again: no actions; error redisplayed; build still failed
jenga build -a

# Remove CFLAGS; no actions; back to original build
rm CFLAGS
jenga build -a
