
# Initial build is fine
jenga build
echo $?

# Remove the rule for epsilon
grep -v '^epsilon' build.jenga > xx && mv xx build.jenga
jenga build # BAD
echo $?

# Duplicate rules for epsilon
echo 'epsilon : : echo -n E1 > epsilon' >> build.jenga
echo 'epsilon : : echo -n E2 > epsilon' >> build.jenga
jenga build
grep -v '^epsilon' build.jenga > xx && mv xx build.jenga

# Define epsilon as a source
echo E3 > epsilon
jenga build # OK

# Define epsilon as Source AND by a rule
echo 'epsilon : : echo -n E4 > epsilon' >> build.jenga
jenga build
rm epsilon

# Rule has a syntax error
grep -v '^epsilon' build.jenga > xx && mv xx build.jenga
echo 'epsilon' >> build.jenga
jenga build

# Rule has a bad action
grep -v '^epsilon' build.jenga > xx && mv xx build.jenga
echo 'epsilon : : missing-command' >> build.jenga
jenga build

# Rule action fails to create target
grep -v '^epsilon' build.jenga > xx && mv xx build.jenga
echo 'epsilon : :' >> build.jenga
jenga build

# Rule action has bad glob
grep -v '^epsilon' build.jenga > xx && mv xx build.jenga
echo 'epsilon : : echo '\''$glob:nowhere'\'' > epsilon' >> build.jenga
jenga build

# Try run phony (but phony not defined)
grep -v '^epsilon' build.jenga > xx && mv xx build.jenga
echo 'epsilon : : echo E5 > epsilon' >> build.jenga
jenga run run

# Add the phony
echo '*run : alpha : cat alpha' >> build.jenga
jenga run run # OK

# Create a dependency cycle
grep -v '^epsilon' build.jenga > xx && mv xx build.jenga
echo 'epsilon : alpha :' >> build.jenga
jenga build
