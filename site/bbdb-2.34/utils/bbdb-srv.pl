#!/usr/local/bin/perl

# This script reads a block of message headers on stdin, and converts them
# to an emacs-lisp string (quoting  all dangerous characters) and then 
# uses the `gnudoit' program to cause a running Emacs process to invoke
# the `bbdb-srv' function with that string.
#
# This has the effect of causing the running Emacs to display the BBDB
# record corresponding to these headers.
#
# See the Emacs side of things in bbdb-srv.el for more info.
#
# A trivial application of this is the shell command:
#
#    echo 'From: Jamie Zawinski <jwz@netscape.com>' | bbdb-srv.perl
#
# which will cause the corresponding record to be displayed.
# A more interesting application of this is:
#
#    setenv NS_MSG_DISPLAY_HOOK bbdb-srv.perl
#
# which will hook BBDB up to Mozilla (Unix Netscape Mail and Netscape News
# versions 3.0b2 and later only.)
#
#	-- Jamie Zawinski <jwz@netscape.com>, 25-apr-96

# spawn in the background and return to the caller immediately.
if (fork == 0) { exit 0; }

$str="(bbdb-srv \"";
while(<>)
{
  # quote most shell metacharacters with backslash.
  s/([\\"`$#^!])/\\\1/g;
  # but quote ' as \047
  s/'/\\047/g;
  # and just for kicks, turn newlines into \n
#  s/\n/\\n/g;

  $str = $str.$_;
}
$str=$str."\")";

exec "gnudoit", "-q", $str;
exit 0;
