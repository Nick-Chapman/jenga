#!/bin/bash

for name in "$@"; do
cat << EOF

${name} : @self-test.deps @${name}.files ${name}.sh
  PATH=\$\$PWD:\$\$PATH bash -v ${name}.sh > .output 2>&1; mv .output \$out
EOF
done

for name in "$@"; do
cat << EOF

${name}.files :
  echo '\$glob:../examples/${name}' | sed 's|^|../examples/${name}/|' > \$out
EOF
done
