#!/bin/bash

for name in "$@"; do
cat << EOF

${name} : @jenga.deps @${name}.files ${name}.sh
  PATH=\$PWD:\$PATH bash -v ${name}.sh > .output 2>&1; mv .output \$@
EOF
done

for name in "$@"; do
cat << EOF

${name}.files :
  echo '\$glob:../examples/${name}' | sed 's|^|../examples/${name}/|' > \$@
EOF
done
