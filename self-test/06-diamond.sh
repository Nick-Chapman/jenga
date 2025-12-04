
# Build
jenga build -a

# Materialize and show top output
jenga install top top.out; cat top.out; echo

# See memoization when building; No target is "Require"d more than once
jenga build -a --debug-demand
