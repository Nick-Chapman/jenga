
# Allow source to be modifed during this test
chmod +w defs.h main.c

# Build from clean (output 55)
jenga build && jenga exec main.exe

# Zero rebuild (no actions; no execution change)
jenga build && jenga exec main.exe

# Change main.c (one action; modified output 55)
sed -i 's/world/UNIVERSE/g' main.c
jenga build && jenga exec main.exe

# Whitespace change to fib.h (two actions; same output 55)
sed -i 's/int fib/int      fib/g' fib.h
jenga build && jenga exec main.exe

#  Change const value in defs.h (two actions; output 89))
echo '#define MY_CONST 11' > defs.h
jenga build && jenga exec main.exe

# Setup ALT defs file (one action to re-glob; same output 89)
echo '#define MY_CONST 12' > defsALT.h
jenga build -a && jenga exec main.exe

# Switch main to use ALT defs (three actions; output 144)
sed -i 's/defs/defsALT/g' main.c
jenga build && jenga exec main.exe

# Modify defs file to original value (no actions; same output 144):
echo '#define MY_CONST 10' > defs.h
jenga build && jenga exec main.exe

# Switch main back to origianl defs file (no actions; output 55)
sed -i 's/defsALT/defs/g' main.c
jenga build && jenga exec main.exe


# Modify compile flags via optionally detected cflags file...

# Compile with -Wall: causes recompile; but no link
echo '-Wall' > cflags
jenga build -a

# Compile with -O2 causes recompile and link
echo '-O2' > cflags
jenga build -a
