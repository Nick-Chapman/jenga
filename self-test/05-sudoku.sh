
# Build
jenga build -a

# Solve a puzzle
jenga exec solver.exe puzzle

# Modify haskell code (careful with hard-link)
cat Sudoku.hs > x
echo xxx >> x
mv x Sudoku.hs

# Try build; expect ghc error
# We used to have a bug where the compile error was somtimes reported twice for -j2 and higher.
jenga.exe build --cache=. --rel -j3 -a

# On rebuild we correctly get just a single report
jenga.exe build --cache=. --rel -j3 -a
