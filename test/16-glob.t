
  $ here=$PWD
  $ (cd $TESTDIR/../src; jenga install jenga.exe $here/jenga.exe)
  $ echo exec $PWD/jenga.exe '"$@"' --rel --cache=$PWD > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

Get example: tests globbing & use jenga actions (phony targets):

  $ cp -rp $TESTDIR/../examples/16-glob example
  $ cd example

Run the view action:

  $ jenga run view -a
  A: echo 'build.jenga\nfile1\nfile2\nfile3\nyyy' > my.all
  A: tar -cf ball.tar $(cat my.all)
  A: echo -------------------- >> report
  A: tar -tf ball.tar | sort >> report
  A: echo -------------------- >> report
  checked 3 targets
  A: cat report
  (directory) .
  (rule) *view : report
  (command) $ cat report
  --------------------
  build.jenga
  file1
  file2
  file3
  yyy
  --------------------
  ran 6 commands
