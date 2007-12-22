#!/usr/local/bin/perl5 -w
#
# Caller-ID-logger, by jwz (19-Jan-97)
# Modified: 24-Apr-97
#
# Opens the modem and waits for it to print caller-ID data.  When it does,
# it logs it to a file, parses it, and pops up a window using "xmessage".
# If the number is present in your .bbdb file, it shows the name (or company)
# associated with it.  
#
# Todo:
#  My caller ID service (in San Francisco) only ever sends numbers, not names,
#  so I've never seen a "name" line come in; I assume that it would send both
#  a name and a number, so it would be nice to present both (with error 
#  checking against BBDB) but the code as currently structured only handles
#  one-line-per-call.  It should realize that consecutive lines with the same
#  timestamp are the same call.
#
#  Modems other than ZyXELs have different caller-ID formats, and this doesn't
#  deal with those.

##############################################################################
#
# Some variables you might want to set...


# Set this to the device that your modem is attached to.
#
$modem_device = "/dev/ttyd1";

# This is your .bbdb file.  (Set it to null if you don't want to do BBDB
# lookups at all, but why would you want to go and do a thing like that?)
#
$bbdb_file    = "$ENV{HOME}/.bbdb";

# A shell command to use to cause emacs to pop up the BBDB buffer
# (bbdb-srv.pl is a good choice, so it defaults to the value of the
# shell environment variable $NS_MSG_DISPLAY_HOOK.)
#
$bbdb_cmd     = $ENV{NS_MSG_DISPLAY_HOOK};

# If you want the $bbdb_cmd to be run on a different host, set it here.
#
$bbdb_host    = "gimp";

# If you want each call to be logged to a file as well, name it here.
#
$logfile      = "/usr/spool/fax/log/cid-log";

# For verbosity...
$debug        = 0;

# How to pop up a dialog box.
#
$xmessage_cmd	= "xmessage";
@xmessage_args	= ("-display",	":0",
		   "-name",	"Caller ID",
		   # roughly centered on my screen; YMMV.
		   "-geometry",	"+400+400",
		   "-xrm",	"*Font: -*-new cent*-bold-r-normal-*-240-*",
		   "-xrm",	"*Foreground: black",
		   "-xrm",	"*Background: lightgreen",
		   # no buttons on the window: dismiss it by clicking in it.
		   "-button",	"",
		   "-xrm", "*form.Translations: #override <BtnDown>: exit(0)",
		   "-xrm", "*Command.Font: -*-new cent*-bold-r-normal-*-120-*",
		   "-xrm", "*Command.horizDistance: 130"
		   );

# Uh, let's turn off the screensaver before popping up the window.
#
$pre_dialog_cmd = "xscreensaver-command -deactivate";


# commands (and their expected responses) used to initialize the modem.
#
@modem_init   = ( "AT",		"OK",		# ping
		  "ATZ",	"OK",		# reset
		  "ATE0",	"OK",		# don't echo commands
		  "ATM0",	"OK",		# turn off speaker
		  "ATN0",	"OK",		# turn off ringer
		  "ATS40.2=1",	"OK",		# turn on caller ID
	        );


# for diagnostics: if the modem ever asynchronously prints something that
# doesn't match this, we issue a warning.
#
$expected_responses = "^CALLER NUMBER"		. "|" .
    		      "^REASON FOR NO CALLER "	. "|" .
    		      "^RING"			. "|" .
    		      "^TIME: [-0-9: ]+\$";


##############################################################################
#
# Talking to the serial port...
#
#

if ( $ debug ) {
    use diagnostics;
}


sub open_modem {
    use IPC::Open2;

    # Close the terminal streams before forking `cu', because otherwise
    # it fucks around with the stty settings.
    #
    open(SAVEIN,  "<&STDIN")  || die("can't dup stdin");
    open(SAVEOUT, ">&STDOUT") || die("can't dup stdout");
    open(SAVEERR, ">&STDERR") || die("can't dup stderr");
    close(STDIN);
    close(STDOUT);
    close(STDERR);

    my $cu_pid = open2( \*MODEM_IN, \*MODEM_OUT,
		       "cu -l$modem_device -s2400 2>&1");

    # Now that cu has been launched, we can restore them.
    #
    open(STDIN,  "<&SAVEIN")  || die("can't restore stdin");
    open(STDOUT, ">&SAVEOUT") || die("can't restore stdout");
    open(STDERR, ">&SAVEERR") || die("can't restore stderr");
    close(SAVEIN);
    close(SAVEOUT);
    close(SAVEERR);

    # The following doesn't seem to work and I don't know why...
    #
    # Set up a signal handler to try and kill off the cu process
    # when we exit, instead of waiting ~30 seconds for it to notice
    # that the pipe is gone...
    #
#    $SIG{INT} = sub { my $signame = shift;
#		      if ( $debug) { 
#			  print STDERR "sending $signame to $cu_pid\n";
#		      }
#		      print MODEM_OUT "\r\n~.\r\n";
#		      close MODEM_OUT;
#		      close MODEM_IN;
#		      kill ($signame, $cu_pid);
#		      exit (1);
#		    };

    $_ = <MODEM_IN>;
    chop;
    if ( !m/^Connected/ ) {
	print STDERR "$0: cu printed `$_' instead of `Connected'\n";
    }
}

sub read_line {
    $_ = <MODEM_IN>;
    $_ || die("got eof on modem");
    s/[\r\n]+$//;
    if ( $_ eq "" ) {
	$_ = <MODEM_IN>;
	$_ || die("got eof on modem");
	s/[\r\n]+$//;
    }
    return $_;
}

sub command {
    my ( $command, $expected_response) = @_;

    if ( $debug ) {
	print STDERR "sending: $command\n";
    }

    print MODEM_OUT "$command\r\n";
    my $line = read_line();

    if ( $line eq $command ) {
	if ( $debug ) {
	    print STDERR "    got echo: reading next line too...\n";
	}
	$line = read_line();
    }

    if ( $line ne $expected_response ) {
	print STDERR "    got: $line ; expected: $expected_response\n";
    } elsif ( $debug ) {
	print STDERR "    got: $line\n";
    }
}

sub init_modem {
    open_modem;

    my $len = $#modem_init + 1;
    my $i;
    for ($i = 0; $i < $len; $i += 2) {
	command($modem_init[$i], $modem_init[$i+1]);
    }
}

sub handle_async_line {
    local ( $_ ) = @_;

    if (!m/$expected_responses/) {
	print STDERR "modem turd:   $_\n";

    } elsif (m/CALLER/) {
	if ( $debug ) {
	    print STDERR "caller: $_\n";
	}
	handle_cid_line($_);

    } elsif ( $debug ) {
	if ( $_ eq '' ) {
	    print STDERR "ignored: blank line\n";
	} else {
	    print STDERR "ignored: $_\n";
	}
    }
}


##############################################################################
#
# Parsing BBDB and CID data...
#

sub find_bbdb_record {
    my ( $area, $exchange, $suffix ) = @_;

    if ( ! $bbdb_file ) {
	return undef;
    }

    # strip off leading 0's, to match the way it's stored in .bbdb.
    $area     =~ s/^0+(.)/$1/;
    $exchange =~ s/^0+(.)/$1/;
    $suffix   =~ s/^0+(.)/$1/;

    my $bbdb_rec = undef;
    my $pat = "\\[\"[^\"]+\" $area $exchange $suffix (nil|[0-9]+)\\]";

    open(BBDB, "<$bbdb_file") || die("error opening $bbdb_file: $!\n");

    while (<BBDB>) {
	if ( m/$pat/ ) {
	    $bbdb_rec = $_;
	    last;
	}
    }
    close(BBDB);
    return $bbdb_rec;
}


# note: global (kludge!)
$pretty_number = 0;

sub make_message_string {
    my ( $number, $date, $fn, $ln, $co, $error ) = @_;
    my $msg;

    my $line_prefix = "    ";
    my $line_suffix = "    ";

    # First print the date (reformatted.)
    #
    $_ = $date;
    my ( $dotw, $mon, $day, $hr, $min, $sec, $year ) =
	m/^([^ ]+) +([^ ]+) +([^ ]+) +([^:]+):([^:]+):([^:]+) +([^ ]+) *$/;
    $year =~ s/^..(..)/$1/;
    $day  =~ s/^0//;
    $hr   =~ s/^0//;
    if ($hr < 12) {
	$ampm = "am";
    } else {
	$ampm = "pm";
	if ($hr > 12) { $hr -= 12 };
    }
    $date = "$hr:$min$ampm, $day-$mon-$year ($dotw)";
    $msg = $line_prefix . $date . $line_suffix;

    # Next print the caller name, company, or error message.
    #
    if ( $error ) {
	$msg .= "\n" . $line_prefix . $error . $line_suffix;
    } elsif ( $co && !$fn && !$ln ) {
	$msg .= "\n" . $line_prefix . $co . $line_suffix;
    } elsif ( $fn || $ln ) {
	$msg .= "\n" . $line_prefix . "$fn $ln" . $line_suffix;
    }

    # Next print the phone number (formatted nicely.)
    #
    if ( $number ) {
	my $area = 0;
	my $exchange = 0;
	my $suffix = 0;
	$_ = $number;
	( $area, $exchange, $suffix ) =
	    m/^([0-9][0-9][0-9])([0-9][0-9][0-9])([0-9][0-9][0-9][0-9]+)/;

	# note: global (kludge!)
	$pretty_number = "($area) $exchange-$suffix";
	$msg .= "\n" . $line_prefix . $pretty_number . $line_suffix;
    }

    return $msg;
}

use POSIX;
sub reaper {
    $SIG{CHLD} = \&reaper;  # loathe sysV
    my $signame = shift;
    if ( $debug >= 2 ) {
	printf STDERR "  (got SIG$signame...)\n";
    }
    my $child;
    while ( ( $child = waitpid(-1,WNOHANG) ),
	    $child > 0 ) {
	if ( $debug >= 2 ) {
	    printf STDERR "    (pid $child exited with $?)\n";
	}
    }
}

sub fork_and_exec {
    my @cmd_list = @_;

    $SIG{CHLD} = \&reaper;

    if ( $debug >= 2 ) {
	$_ = $cmd_list[0];
	s/ .*//;
	print STDERR "forking for " . $_ . " at " . (localtime) . ".\n";
    }

    my $pid;
    if ($pid = fork()) {
	# parent
    } elsif (!defined $pid) {
	print STDERR "$0: fork failed: $!\n";
    } else {
	# child

	if ( $debug ) {
	    $_ = $cmd_list[0];
	    s/ .*//;
	    print STDERR "exec'ing " . $_ . " at " . (localtime) .
		" in pid $$.\n";
	}
	close(STDIN);
	close(STDOUT);
	close(STDERR);
	exec @cmd_list;
    }
}


sub fork_and_exec_for_bbdb {
    my @cmd_list = @_;
    my $number = shift @cmd_list;

    $SIG{CHLD} = \&reaper;

    if ( $debug >= 2 ) {
	$_ = $cmd_list[0];
	s/ .*//;
	print STDERR "forking for " . $_ . " at " . (localtime) . ".\n";
    }

    my $pid;
    if ($pid = fork()) {
	# parent
    } elsif (!defined $pid) {
	print STDERR "$0: fork failed: $!\n";
	exit (1);
    } else {
	# child

	if ( $debug ) {
	    $_ = $cmd_list[0];
	    s/ .*//;
	    print STDERR "exec'ing " . $_ . " at " . (localtime) .
		" in pid $$.\n";
	}
	if ( system @cmd_list ) {
	    my $cmd = "gnudoit -q '(bbdb-srv-add-phone \"$pretty_number\")'";
	    if ( $bbdb_host ) {
		$cmd =~ s/([()\"])/\\$1/g;
		$cmd = "rsh $bbdb_host $cmd";
	    }
	    exec $cmd;
	}
	exit (0);
    }
}


sub pop_up_dialog {
    my ( $msg, $buttonp, $number ) = @_;

    fork_and_exec $pre_dialog_cmd;

    if ( ! $buttonp ) {
	fork_and_exec $xmessage_cmd, @xmessage_args, "\n$msg\n\n";
    } else {
	my @args = ( @xmessage_args, "-button", "Add To BBDB" );
	fork_and_exec_for_bbdb $number, $xmessage_cmd, @args, "\n$msg\n\n";
    }
}

sub pop_up_bbdb_buffer {
    my ( $caller ) = @_;
    if ( $bbdb_cmd ) {
	my $cmd = $bbdb_cmd;
	if ( $bbdb_host ) {
	    $cmd = "rsh $bbdb_host $cmd";
	}
	$caller =~ s/\\/\\\\/g;
	$caller =~ s/\"/\\\\\"/g;
	`echo "Path:\nFrom: \\\"$caller\\\" <>" | $cmd >&- 2>&- &`;
    }
}


sub handle_cid_line {
    my($line) = @_;

    my $date = localtime;

    # Log the call...
    #
    if ( $logfile ) {
	if (open(LOG, ">>$logfile")) {
	    print LOG "$date\t$line\r\n";
	    close LOG;
	} else {
	    print STDERR "error opening $logfile: $!\n";
	}
    }

    # Pull the phone number out of the message...
    #
    my $number = "";
    my $error = "";

    $_ = $line;
    if ( m/^CALLER NUMBER/ ) {
	( $number ) = m/^[^:]+: *(.*) *$/;
    } else {
	$error = $line;
    }

    my $caller = undef;

    my $fn = undef;
    my $ln = undef;
    my $co = undef;
    my $buttonp = 0;

    if ( !$number || $number eq "" ) {
       	$error =~ tr#A-Z#a-z#;
	$error =~ s/^REASON FOR NO CALLER (NUMBER|NAME)/Caller unknown/i;

    } else {
	$_ = $number;

	my $area = 0;
	my $exchange = 0;
	my $suffix = 0;
	( $area, $exchange, $suffix ) =
	    m/^([0-9][0-9][0-9])([0-9][0-9][0-9])([0-9][0-9][0-9][0-9]+)/;

	my $bbdb_rec = find_bbdb_record($area, $exchange, $suffix);

	if ( $bbdb_rec ) {
	    my $junk = 0;
	    $_ = $bbdb_rec;
	    # This will lose if names or aliases have double-quotes in them.
	    # No doubt there's some hairier regexp magic that handles that...
	    ( $fn, $ln ) = m/^[\[]\"([^\"]*)\" *\"([^\"]*)\"/;
	    ( $junk, $junk, $junk, $co ) =
      m/^[[](nil|\"[^\"]*\") *(nil|\"[^\"]*\") (nil|[(][^)]*[)]) \"([^\"]*)\"/;

	    if ( $fn || $ln ) {
		$caller = "$fn $ln";
	    }
	} else {
	    $buttonp = 1;
	}
    }

    my $msg = make_message_string($number, $date, $fn, $ln, $co, $error);
    pop_up_dialog($msg, $buttonp, $pretty_number);

    if ( $caller ) {
	pop_up_bbdb_buffer($caller);
    }
}


##############################################################################
#
# hey ho.  let's go.
#
sub main {
    init_modem();
    while (1) {
	handle_async_line(read_line());
    }
    exit (1);
}

main();

