
# Build
jenga build -a

# Change the example to echo a warning before exiting
sed -i 's|exit 0|echo WARNING; exit 0|' build.jenga
jenga build -a

# And zero build (see warning even though no actions were run)
jenga build -a

# Change the example to have a non-zero error code
sed -i 's|exit 0|exit 42|' build.jenga
jenga build -a

# And zero build (see warning and error, again even though no actions were run)
jenga build -a
