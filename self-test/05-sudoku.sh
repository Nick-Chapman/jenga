
# Build
jenga build -a

# Solve a puzzle
jenga exec solver.exe puzzle

# Modify haskell code (careful with hard-link)
cat Sudoku.hs > x
echo xxx >> x
mv x Sudoku.hs

# Try build; expect ghc error
# BUG: the compile error is reported twice!
# We need -j2 to get the bug; and even then it is not guarenteed. perhaps -j3 is better
jenga.exe build --cache=. --rel -j3 -a

# On rebuild we correctly get just a single report
jenga.exe build --cache=. --rel -j3 -a
