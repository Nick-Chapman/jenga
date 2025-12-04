
# Remove the expectation file
rm the.out.expected

# Test; the.out.expected is missing, so failure
jenga test

# Promote
jenga build -a --promote

# Test (now passes)
jenga test -a

# Change expectation
rm the.out.expected
echo 'But I want to see this!' > the.out.expected

# Rerun test; get failure
jenga test -a
