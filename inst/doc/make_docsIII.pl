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

# set file delimeter to EOF
$/ = \0;


foreach my $fi (@file_list) {
  my $title = $fi;

  ($fi, $title) = $fi =~ m/(.*?):(.*)$/s if ($fi =~ m/(.*?):(.*)/s);

  $fi =~ s/^ *//g;
  $fi =~ s/ *$//gm;

  $title =~ s/^ *//g;
  $title =~ s/ *$//gm;

  $fi = "$fi.tex" unless $fi =~ m/.*\.tex$/;

  # append directory to this
  $fi = "$dir/$fi" if defined $dir and -d $dir;

  # skip if doesn't exist
  unless (open FI, $fi) {
    print "*** file `$fi` not found\n";
    next;
  }

  $tex = <FI>;

  $tex =~ m/\\begin{document}(.*?)\\end{document}/s;

  $body->{$title} = $1 if defined $1;

  $tex =~ s/\\begin{document}.*\\end{document}//gs;

  #
  push @packages, $tex =~ m/$pre->{packages}/gs;
  push @classes, $tex =~ m/$pre->{classes}/gs;
  push @styles, $tex =~ m/$pre->{styles}/gs;

  #
  for (keys %$cmds) {
    push @commands, $tex =~ m/$cmds->{$_}/gs
  }
}




# (5.5) FILTER REDUNDANT LIST ENTRIES
# ***********************************

if ( grep { m/(\[.*?\])$/s } @packages) {
  map { s/(\[.*?\])$//gm } @packages;
  # print "*** warning: ";
  # print "makde_docs does not currently support working with dated packages";
}


@packages = uniq @packages;
@commands = uniq @commands;
@classes = uniq @classes;
@styles = uniq @styles;



# (6) BUILD DOCUMENT
# ******************
print DOC "\\documentclass{book}";


my ($m, $d, $y);
my $earliest;


if (@packages) {
  print DOC "\n\n\n% required packages\n";
  print DOC join "\n", @packages;
  print DOC "\\usepackage{Zinput}";
}


if (@styles) {
  print DOC "\n\n\n% required styles\n";
  print DOC join "\n", @styles;
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
print DOC "\\begin{document}\n";

for (keys %$body) {
  print DOC "\n\n\\chapter{$_}\n";
  print DOC $body->{$_};
  print DOC "\n\n";
}

print DOC "\n\n% end body\n";
print DOC "\\end{document}";




# (6) PROGRAM CLOSE
# *****************

close DOC;
close CONF;
close FI;
