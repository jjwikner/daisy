function hg = histogram(analog_values, NOB);

K = 2^NOB;

[hg, dummy] = hist(analog_values, K-1);

hg = hg/sum(hg);













