#!/usr/bin/perl

# This script extracts all equations and eqnarrays from a .tex file
# The output is a new .tex file containing the equations only 
# To extract the syntax for a specific equation use the 
# daisyGetTexEq.pl command 
# 
# Example
#
# %% Extract all equations from fileIn.tex, and send these to fileOut.tex 
#
# daisyHarvestTexEq fileIn.tex fileOut.tex
#
# (c) Niklas U Andersson, Electronics Systems, Linköping University
#
# Change History
#
# 2010-12-11 First Release



$inFile=$ARGV[0];  # input .tex file
$outFile=$ARGV[1]; # output .tex file 

open(FILE_IN, "$inFile");
open(FILE_OUT,">$outFile");


@tempArray = ();
@fileOutArray = ();


# This file header is used for the output file
@fileHeader = ("
%This is the result of the harvested equations
\\documentclass{article}

\\newcommand{\\s}[1]{\\ensuremath{\\sin \\frac{\\pi}{#1}}}
\\newcommand{\\co}[1]{\\ensuremath{\\cos \\frac{\\pi}{#1}}}
\\usepackage{graphicx}
\\usepackage{multirow}
\\usepackage{cite}
\\usepackage{color}
\\usepackage{subfigure}
\\usepackage{algorithmic}
\\usepackage{mdwmath}
\\usepackage{amsmath} 
\\usepackage{amssymb} 
\\usepackage{tabularx}

\\title{Harvested Equations}
\\author{This file is generated using the Perl script daisyHarvestTexEq.pl}
\\begin{document}
\\maketitle
");



# Running index for the collected equations (\label{eq:$runningIdx}
$runningIdx = 0;

# print(@fileHeader);

# parse the input file row by row
while(<FILE_IN>)
{
    
    # search for equations in the .tex file
    # start collect the rows in the eq. statement
    # continue to do so as long as you are inside the eq. statement
    if (
	(( (m/\\begin{equation}/) || (m/\\begin{eqnarray}/)) && (!(m/\%/))) 
	|| 
	($insideEquation == 1))
    {
	
	# if you find a label statement, create a new lable using the running index
	if (m/\\label/)
	{
	    #print("\\label{eq:",$runningIdx,"}\n");   
	    @tmpLine = "\\label{eq:".$runningIdx."}\n";
	    
            # push the line into a temporary array that will be
	    # written to the output file
	    push(@fileOutArray,@tmpLine);
	    $runningIdx++;
	}
	else
	{
	    #print("$_\n");
	    #print("$_");
	    
            push(@fileOutArray,"$_");
	}
	    
	# keep pushing until you find the \end{equation}
	$insideEquation = 1;	
    }

    # stop collect rows if you find have reached the end of the equation statement
    if ((m/\\end{equation}/) || (m/\\end{eqnarray}/)) 
    {
	$insideEquation = 0;
	push(@fileOutArray,"\n");
	#print("\n");
    }

}

# print("\n");

# print(@fileOutArray);

# Create the output file
# file header, collected equations and finally the .tex \end{document} statement 
print FILE_OUT "@fileHeader";
print FILE_OUT "@fileOutArray";
print FILE_OUT "\\end{document}";

#print("\n");
# Close the files
close(FILE_IN);
close(FILE_OUT);

