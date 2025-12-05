
# Build
jenga build -a

# Materialize and show top output
jenga cat top; echo

# See memoization when building; No target is "Require"d more than once
jenga build -a --debug-demand
