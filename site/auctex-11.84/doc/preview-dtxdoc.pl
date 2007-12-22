#!/usr/bin/perl
# Simpleminded autoconverter from preview.dtx to preview-dtxdoc.texi
# run as 'perl preview-dtxdoc.pl ../latex/preview.dtx preview-dtxdoc.texi'
# Author Jan-Åke Larsson <jalar@mai.liu.se>

die "Usage: perl preview-dtxdoc.pl infile outfile" unless ($#ARGV == 1);
open(STDIN, $ARGV[0]) || die "Can't open $ARGV[0] for reading";
open(STDOUT, "> $ARGV[1]") || die "Can't open $ARGV[1] for writing";


# Eat header 
MUNGE: while (<STDIN>) {
    last MUNGE if /^% *.section/;
}

# Fish out possible CR characters.
/(\r*)$/;
$cr = $1;


# Noindent is used sometimes after \end{quote} (see below) 
$noindent="";
# Quote environments is translated into @example _without_
# @code{..} inside (see below) 
$quote="";
MAIN: while (<STDIN>) {
    s/^%//;
    s/\\%/%/g;

    # Text-substitution macros
    s/\@/\@\@/g;
    s/\\#/#/g;
    s/AUC~?\\TeX[\\ ]?/\@AUCTeX{}/g;
    s/\\LaTeX[\\ ]?/\@LaTeX{}/g;
    s/\\TeX[\\ ]?/\@TeX{}/g;
    s/\\previewlatex[\\ ]?/\@previewlatex{}/g;
    s/EPS/\@acronym{EPS}/g;
    s/DVI/\@acronym{DVI}/g;
    s/~/\@w{ }/g;
    s/^ *//;
    # Environments
    if (s/\\begin\{quote\}/$cr\n\@example/) { 
	$quote="yes" }
    if (/^\w/) { 
	print $noindent } 
    $noindent = "";
    if (s/\\end\{quote\}/\@end example$cr\n/) { 
	$quote=""; 
	$noindent="\@noindent$cr\n"  }
    s/\\begin\{description\}/$cr\n\@table \@w/;
    # Convoluted pattern: handle 
    # \item[|...|], \item[\meta{..}] and \item[{|[]|}]
    s/\\item\[\{?(.+?[\|\}])\}?\] ?/\@item $1$cr\n/;
    s/\\end\{description\}/\@end table$cr\n/;
    s/\\begin\{enumerate\}/$cr\n\@enumerate/;
    s/\\item /\@item /;
    s/\\end\{enumerate\}/\@end enumerate$cr\n/;

    # Formatting (\cmd is special within {quote})
    s/\\texttt/\@option/g;
    s/\\marg\{([^}]+)\}/\@{\@var{$1}\@}/g;
    s/\\meta/\@var/g;
    s/\\emph/\@emph/g;
    s/\\cmd(\\[\(\)\w]+)/|$1|/g;
    s/\\cmd\{(.*?)\}/|$1|/g;
    s/\\oarg\{([^}]+?)\}/\[\@var{$1}\]/g;
    s/\\char.//g;
    s/\\raggedright$cr\n//g;
    s/\\DescribeEnv\{(.*?)\} /\@item \\begin\@{$1\@}\@dots{}\\end\@{$1\@}$cr\n/;
    if (s/\\DescribeMacro\{(.*?)\}( |$cr\n)/\@item $1$cr\n/) {
	# Index entries for two important macros
	if (/(\\Preview(Macro|Environment))( |$cr\n)/) {
	    $_ .= "\@findex $1$cr\n";
	}
    }

    # ||||||| Hell... I hate bars
    # Braces WITHIN bars should be escaped like so: @{ @}
    # and |..| translates to @code{..} or @file{..} depending on content
    # and to .. if in {quote}
    split /\|/;
    $odd=0;
    COMMAND: foreach (@_) {
	if ($odd==0) {
	    $odd=1;
	} else {
	    s/\{/\@\{/g;
	    s/\}/\@\}/g;
	    if (! $quote) {
		if (/[.\/]/) {
		    $_="\@file\{".$_."\}";
		} else {
		    $_="\@code\{".$_."\}";
		}
	    }
	    $odd=0;
	}
    }
    $_=join("",@_);
    # Argh! mixed types occurs in @code{...}@var{..}@file{..}
    # Should be @file{...@var{..}..}
    s/\@code(\S*?)\}(\S*)\@file\{/\@file$1$2/g;

    # Texinfo @node-ification
    if (s/\\section\{(.*)\}/\@subsection $1/) {
	if (s/[Oo]ptions/options/) {
	    $_="\@menu$cr\n" .
"* Package options::$cr\n" .
"* Provided commands::$cr\n" .
"\@end menu$cr\n$cr\n" .
"\@node Package options, Provided commands, The LaTeX style file, The LaTeX style file$cr\n" . $_;
	} elsif (s/[Cc]ommands/commands/) {
        # \Describe... needs @table
	    $_= "\@node Provided commands, ,Package options, The LaTeX style file$cr\n" . 
		$_ . "$cr\n\@table \@code$cr\n";
	}
    }
	
    # Stop here
    # \Describe.... needs @end table
    if (/^.StopEventually/) {
	print "\@end table$cr\n";
	last MAIN;
    }
    print $_;
}
