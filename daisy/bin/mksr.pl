#!/usr/bin/perl 

 #  ====================================================================
 #  Copyright (c) 1998-2000 Astonish Inc.
 #  All rights reserved.
 #
 #  Redistribution and use in source and binary forms, with or without
 #  modification, are permitted provided that the following conditions
 #  are met:
 #
 #  1. Redistributions of source code must retain the above copyright
 #     notice, this list of conditions and the following disclaimer.
 #
 #  2. Redistributions in binary form must reproduce the above copyright
 #     notice, this list of conditions and the following disclaimer in the
 #     documentation and/or other materials provided with the distribution.
 #
 #  3. The name of the author may not be used to endorse or promote products
 #     derived from this software without specific prior written permission.
 #
 #  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 #  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 #  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 #  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 #  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 #  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 #  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 #  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 #  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 #  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 #  ====================================================================
 #

#
#   APP: mksr
#   ORIGIN: August 1999
#
#   VERSION NUMBER
$ver = "ver 0.0.5";

#   Last Modified: November 2000
#   
#   Auth: www.blazonry.com/perl/
#
#   DESC: Search and Replace 
#

## print "\nmksr Search and Replace    $ver\n";
## whatever print "blazonry.com\n\n";

# TODO LIST
#
# - ADD Recursion Option

    ## REQUIRED CPAN MODULES ##
    # File::Recurse Module bundled with File-Tools 2.0
    # download from CPAN site (http://www.perl.com/CPAN//modules/by-module/File/)
    # use File::Recurse;
    ####


my $find = $ARGV[0];
my $replace = $ARGV[1];
my $filename = $ARGV[2];

#@filelist = <*$glob>;

if ( (!$find) || (!$replace) || (!$filename) ) {
    print "Search and replace recursively through the current directory\n";
    print "replacing <find> with <replace> in each file specified.\n";
    print "To use wildcards leave off the * Ex: '.txt' \n\n";
    print "    mksr <find> <replace> <file>\n";
    
    exit(0);
}


# process each file in file list
#oreach $filename (@filelist) {

	print "    P: Replacing text in $filename\n";

	# retrieve complete file
    open (IN, "$filename") || die("Error Reading File: $filename $!");
	{
		undef $/;          
		$infile = <IN>;
	}
    close (IN) || die("Error Closing File: $filename $!");

	$infile =~ s/$find/$replace/g;
	

	# write complete file 
     open (PROD, ">$filename") || die("Error Writing to File: $filename $!");
	 print PROD $infile;
     close (PROD) || die("Error Closing File: $filename $!");

#

   print "\nReplacing text in file $filename finished.\n";


   exit(0);


