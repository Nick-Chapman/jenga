
# Build; seeing the actions
jenga build -a

# Build quietly; materializaing; showing top output
jenga build -mq && cat ,jenga/top; echo

# Build with --debug-demand; memoization ensures no target is "Require"d more than once
jenga build -a --debug-demand
