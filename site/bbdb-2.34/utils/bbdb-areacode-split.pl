#!/usr/bin/perl
# 
# Looks for phone numbers in your .bbdb with a particular area code
# and one of a set of exchanges and changes the area code.  The old
# and new area codes are specified on the command line, as is the
# location of a file that contains the exchanges that are being
# changed.  (The format of that file is very loose.  Every three digit
# sequence will be used.)
# 
# Seth Golub <seth@cs.wustl.edu>
# 15 Aug 1997

sub Usage
{
    $0 =~ s@.*/@@;
    die "Usage: \n  $0 <old-code> <new-code> <exchanges-file> [bbdb]\n";
}

$old_area_code = shift || Usage();
$new_area_code = shift || Usage();
$exchange_list_file = shift || Usage();

$bbdb_file = $ENV{'BBDB'} || shift || $ENV{'HOME'} . '/.bbdb';
$bbdb_dir = `dirname $bbdb_file`;  chomp $bbdb_dir;
$tmp_file = "$bbdb_dir/bbdb.new-$$";

open( LIST, "<$exchange_list_file" ) 
    || die "Failed to open $exchange_list_file\n";

while (<LIST>)
{
    while ( /(\d\d\d)/g )
    {
        push( @exchanges, $1 );
    }
}

close( LIST );

$exchanges = join( '|', @exchanges );

open( BBDB_IN, "<$bbdb_file" ) || die "Failed to open $bbdb_file\n";
open( BBDB_OUT, ">$tmp_file" ) || die "Failed to open $tmp_file\n";

while (<BBDB_IN>)
{
    next unless /^\[/;
    s/(\[\".*?\") $old_area_code (($exchanges) \d+ \d+\])/$1 $new_area_code $2/og;
} continue {
    print BBDB_OUT;
}

close( BBDB_IN );
close( BBDB_OUT );

unlink( "$bbdb_file.bak" );
rename( $bbdb_file, "$bbdb_file.bak" );
rename( $tmp_file, $bbdb_file );

print STDERR "Old bbdb moved to $bbdb_file.bak\n";

__END__
