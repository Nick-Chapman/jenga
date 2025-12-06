
# Remove the expectation file
rm the.out.expected

# Test; the.out.expected is missing, so failure
jenga test

# Promote
jenga build -a --promote

# Test (now passes)
jenga test -a

# Replace the expectation file in place.
# This would provoke a bug if file materialization were implemented using "ln" instead of "cp".
chmod +w the.out.expected
echo 'But I want to see this!' > the.out.expected

# Rerun test; get failure
jenga test -a
