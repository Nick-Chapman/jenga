
# self-test

This directory contains a test script file `NAME.sh` for each example directory `../examples/NAME/`

The expected output from each test script is found in `expected/NAME.expected`

These tests build and test `../src/jenga.exe`

But the test are orchestrated using the installed `jenga`. (This is what is meant by self-test)

Run tests:
```
jenga test
```

To accept changes:
```
jenga test --promote
```
