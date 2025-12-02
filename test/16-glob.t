
  $ (cd $TESTDIR/..; jenga build src -q)
  $ echo exec $TESTDIR/../src/jenga.exe '"$@"' --rel --cache=$PWD > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

Get example: tests globbing & use jenga actions (phony targets):

  $ cp -rp $TESTDIR/../examples/16-glob example
  $ cd example

Run the view action:

  $ jenga run view
  checked 4 targets
  (directory) .
  (rule) *view : report
  (command) $ cat report
  --------------------------------------------------
  my.all [ file1 file3 yyy file2 build.jenga ]
  new_my.all [ :. ]
  TAR...
  build.jenga
  file1
  file2
  file3
  yyy
  --------------------------------------------------
  ran 10 commands
