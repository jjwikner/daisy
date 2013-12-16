function w = hann1(n)
% function w = hann1(n)
% A Hann1 (squared) window of length n. Does not smear tones located exactly in a bin.
w = .5*(1 - cos(2*pi*(0:n-1)/n) );
