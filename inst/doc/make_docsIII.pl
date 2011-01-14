use strict;
use warnings;



# (0) SUBROUTINE DEFINITIONS
# **************************

# @_: list of arguments as an array
# result - a list with unique entries
sub uniq {
  my $hits = {};
  grep { ! $hits->{$_}++ } @_;
}


# $a, $b: two dates formated like dd/dd/dddd
# return: -1, 0, 1 - less than, equal, greater than
sub date_cmp {
  # convert left hand side into a triplet of numbers
  $a =~ /(\d?\d)\/(\d?\d)\/(\d{4})/;
  my ($lmon, $lday, $lyear) = ($1, $2, $3);

  # convert right hand side into ... """
  $b =~ /(\d?\d)\/(\d?\d)\/(\d{4})/;
  my ($rmon, $rday, $ryear) = ($1, $2, $3);

  # compare
  return $lyear <=> $ryear unless $lyear == $ryear;
  return $lmon <=> $rmon unless $lmon == $rmon;
  $lday <=> $rday;
}


# $text: a blob of text
# return: a unique list of packages
sub get_packages {
  my $text = shift;
  $text =~ m/(\\usepackage(?:\[.*?\])?{.*?}(?:\[.*?\])?)/gs;
}


# @_: a list of package preamble commands
# return: a reduced list - containing only the earliest dated
#         of each package
sub reduce_packages {
  my @packages;
  my @package_names = uniq map { m/\\usepackage{(.*?)}/ } @_;

  for (@package_names) {
    # generate regex
    my $regex = qr{\\usepackage{$_}\[(.*?)\]};

    # sort list of dates and get earliest
    my @dates = sort { &date_cmp } map { m/$regex/ } @_;

    my $date = shift @dates;

    # construct earliest dated package
    my $pack = q/\usepackage/;
    $pack .= "{$_}";
    $pack .= "[$date]" if defined $date;

    push @packages, $pack;
  }

  @packages;
}


# get body of a blob of text (from a tex file)
sub get_body {
  my ($text) = shift;
  $text =~ m/\\begin{document}(.*?)\\end{document}/s;
  if ($text =~ m/\\begin{document}(.*?)\\end{document}/s) {
    return $1;
  }
  else {
    return $text;
  }
}


# get title of a blob of text (from a tex file)
sub get_title {
  my ($title) = shift;
  $title =~ m/\\title\{(.*?)\}/;
}



# (1) DEFINITIONS OF REGEXes
# **********************
# outline:
#  * initialization code
#  * preamble
#  * merge-able lines
#  * removable lines
#  * command/environment definitions

# initial stuff - before documentclass
my $init = {};


# preamble stuff - between documentclass and begin{document}
my $pre = {
	   classes => qr{(\\documentclass(?:\[.*?\])?{.*?}(?:\[.*?\])?)},
	   styles => qr{(\\documentstyle(?:\[.*?\])?{.*?})},
	   packages => qr{(\\usepackage(?:\[.*?\])?{.*?}(?:\[.*?\])?)}
	  };


# things that just get called only once
my $merge = {
	     listf => qr{(\\listfiles)(?: |\n|\\)},
	     errc => qr{(\\setcounter{.*?}{\\d*})}
	    };


# remove stuff
my $rm = {
	  docbeg => qr{\\begin{document}},
	  docend => qr{\\end{document}}
	 };


# command/environment stuff - between documentclass and begin{document}
# NOTE: full support for commands requires a simple parser
#       whose algorithm is as follows:
#
#   1. greedier regex: \newcommand{(?:.*?)}(?:\[\d\]){1,2}{.*}
#      (note: .* at the end, instead of .*?)
#   2. stroll through definition until the braces match,
#      return the substring as a hit
#   3. chop off the hit
#   4. repeat until we get no results for the regex in step 1
#
# for the time being, we'll warn users of what's going on
# and give them the option to strip commands or use them without
# braces within the definition
#
# also: do not tab these lines more; otherwise > 80 char
my $cmds = {
   ncmd => qr{\\newcommand{.*?}(?:\[\d*\]){1,2}{(?:.|\n)*?}},
   rncmd => qr{\\renewcommand{.*?}(?:\[\d*\]){1,2}{(?:.|\n)*?}},
   nenv => qr{\\newenvironment{.*?}(?:\[\d*\]){1,2}{(?:.|\n)*?}{(?:.|\n)*?}},
   rnenv => qr{\\newenvironment{.*?}(?:\[\d*\]){1,2}{(?:.|\n)*?}{(?:.|\n)*?}},
   prcmd => qr{\\providecommand{.*?}(?:\[\d*\]){1,2}{(?:.|\n)*?}{(?:.|\n)*?}}
  };

# vignette stuff (?)
my $vig = {};




# (2) SYSTEM COMMAND-LINE STUFF
# ******************************


my (@single);
my ($assign) = {};

foreach (@ARGV) {
  my (@splitted) = split "=", $_;


  if (scalar(@splitted) == 1) {
    push @single, $_;
  }

  elsif (scalar(@splitted) == 2) {
    $assign->{$splitted[0]} = $splitted[1];
  }

  else {
    die "ERROR: incorrect number of parameters passed through commandline";
  }
}




# (3) INITIALIZE IMPORTANT VARIABLES and:
# (4) FILE HANDLE STUFF
# ***********************
# outline:
#  * open file handle
#  * set file delimeter
#  * 

# **********************************

# results of matches instances of regexes (since
# most of our regexes are group matched)
my ($matches) = [];
my $packages = [];

# string containing text in body of tex document
my ($body);

my $dest_file = defined $assign->{dest} ? $assign->{dest} : "doc.tex";
my $conf_file = defined $assign->{conf} ? $assign->{conf} : "make_docsIII.conf";
my $dir = defined $assign->{outdir} ? $assign->{outdir} : "texs";


# open destination and conf file handles
open DOC, ">$dest_file" or die("");
open CONF, "<$conf_file" or die("");


# 1
$/ = "\n";
my @file_list = map { chomp; $_ } <CONF>;


# (5) PROGRAM START
# *****************

# collect data about packages, classes, and styles
my @packages;
my @commands;
my @styles;
my @classes;

# block of body text
my $sections = {};

# holds file contents
my $tex = "";
my $postamble = "";

# set file delimeter to EOF
$/ = \0;

# not a real word, I think
# postamble holds the all the tex between \begin{document}...\end{...}
# essentially, this is the text
#
$postamble = "";

#
foreach my $line (@file_list) {
  my $fi;
  my $title;


  # if the line is wrapped in braces
  # then create a new part named from the text in braces
  # and then process next line
  if (($fi) = $line =~ m/^\{(.*?)\}$/s) {
    $postamble .= "% BEGINNING OF PART\n";
    $postamble .= "\\part{$fi}\n";
    next;
  }

  elsif ($line =~ m/^\*\*\*$/) {
    $postamble .= "\n\n\\appendix\n\n";
    next;
  }

  elsif ($line =~ m/^\*\s*(.*)$/) {
    $fi = $1;
    $title = "";
  }

  elsif ($line =~ m/^:(.*)$/s) {
    $fi = "";
    $title = $1;
  }

  elsif ($line =~ m/(.*?):(.*)/s) {
    ($fi, $title) = ($1, $2);
  }

  else {
    $fi = $title = "";
  }

  next unless $title or $fi;

  # split the word around the first colon
  #($fi, $title) = $fi =~ m/(.*?):(.*)$/s if ($fi =~ m/(.*?):(.*)/s);

  # trim whitespace of both sides of the colon (if there is a colon)
  $fi =~ s/^\s+|\s+$//g;
  $title =~ s/^\s+|\s+$//g;

  # add a .tex extension if there is none
  $fi = "$fi.tex" unless $fi =~ m/.*\.tex$/ or !$fi;


  if ($title) {
    $postamble .= "% CHAPTER $title\n";
    $postamble .= "\\chapter[$title]{$title}\n\n";
  }
  # append directory to this
  $fi = "$dir/$fi" if defined $dir and -d $dir and $fi;

  # if doesn't exist, skip and warn the user

  next unless $fi;
  unless (open FI, $fi) {
    print "*** warning: file `$fi` not found\n";
    next;
  }


  # load entire tex file into a string
  # NOTE: $/ = \0 # EOF
  $tex = <FI>;

  # get body from FI, see subroutine for more info
  my ($body) = get_body $tex;

  #$postamble .= "% CHAPTER $title\n";
  #$postamble .= "\\chapter[$title]{$title}\n\n";
  $postamble .= $body;


  # get packages from FI
  push @packages, get_packages $tex;
}


# remove redundant info
@commands = uniq @commands;
@classes = uniq @classes;
@styles = uniq @styles;

# usepackages has a few extra parameters
@packages = reduce_packages @packages;




# (6) BUILD DOCUMENT
# ******************
print DOC "\\documentclass{book}\n\n";

print DOC "\\title{Zelig}\n";
print DOC "\\author{Gary King}\n";


if (@packages) {
  print DOC "\n\n\n% required packages\n";

  # packages used in individual articles
  print DOC "% package taken from TeX files:";
  print DOC join "\n", @packages;

  # required packages for any Zelig doc
  print DOC "\n\n% Zinput (essential for Zelig docs)\n";
  print DOC "\\usepackage{Zinput}\n";
  print DOC "\\usepackage{Sweave}\n";
  #print DOC "\\usepackage{Rd}\n";

  # line spaces for tex-clarity
  print DOC "\n\n";
}


if (@styles) {
  #print DOC "\n\n\n% required styles\n";
  #print DOC join "\n", @styles;
}


if (@commands) {
  #print DOC "\n\n\n% block of defined commands\n";
  #print DOC join "\n", @commands;
  print "\n\n";
  print "command assignment is currently not supported by make_docs.  ";
  print "In order, to define your own commands, one must add them through";
  print "a .sty file.  Please see the Zelig manual for further instructions.";
}

if (@classes) {
  # I don't know what we should do in this situation
}

print DOC "\n\n\n% begin body\n";
print DOC "\\begin{document}\n\n\n";

print DOC "\\tableofcontents\n\n";

print DOC $postamble;

print DOC "\n\n% end body\n";
print DOC "\\end{document}";




# (6) PROGRAM CLOSE
# *****************

close DOC;
close CONF;
close FI;
