#!/usr/bin/perl 

# Make a word frequency count
%seen = ();
while (<>) {
    while ( /(\w['a-zåäö-]*)/g ) {
        $seen{lc $1}++;
    }
}
 
# output hash in a descending numeric sort of its values
foreach $word ( sort { $seen{$b} <=> $seen{$a} } keys %seen) {
    printf "%5d %s\n", $seen{$word}, $word;
}
