function idnls = idealdnl(ftype,nob,N,ph,nos);


K = 2^nob
A = K-1
ampmax = 31;
ampmin = 0;

fvals = A*sin(ph+2*pi*(1:nos)/N);

[digval, tresh, delta] = adconvert(fvals,nos,0,nob,ampmax,ampmin);


idnls = digval(2:nos)-digval(1:(nos-1))-1;

