function f2 = designF2(fp,zetap,phi)
% f2 = designF2(fp,zetap,phi)		Design the F2 sub-filter
% of a Saramaki halfband filter.  This function is called by designHBF.m.

% subfilter design:
%   1 - delta2' < |F2/phi| < 1 	for f in [0 fp];
%  -1 < |F2/phi| < -1 + delta2'	for f in [0.5-fp, 0.5];
%   1-delta2' = (1-delta2)/(1+delta2)

delta2 = (1-zetap)/(1+zetap);
%delta2p = 1 - (1-delta2)/(1+delta2);

% determine the minimum order required by the filter
passband = exp(j*linspace(0,4*pi*fp));
for nsub = 3:2:17
    h2 = remez(nsub,[0 4*fp 1 1], [1 1 0 0]);
    mag = abs( polyval(h2,passband) );
    if max(abs(mag-1)) < delta2;
	break;
    end
end
n2min = (nsub+1)/2;

% Search all n2,nsd pairs, in order of the product n2*(nsd+1)
% allowing fp to be a variable?
success = 0;
nsdmin = 3;	nsdmax = 6;
for product = (nsdmin+1)*n2min:(nsdmax+1)*n2min
    for nsd = nsdmin:nsdmax
    	n2 = product/(nsd+1);
	if floor(n2) ~= n2	% Only take integer n2,nsd pairs
	    break
	end
	nsub = 2*n2-1;
	% Could try a bunch of fp values
	%fprintf(2,'designF2: Trying (n2,nsd2,fp)=(%2d,%2d,%6.4f)\n',n2,nsd,fp);
	h2 = remez(nsub,[0 4*fp 1 1], [1 1 0 0]);
	h2 =  h2/(phi*(1+delta2));		% Adjust the coefficients.
	f2 = bquantize( h2(n2+1:nsub+1), nsd );
	h2 = (1+delta2)*phi*[f2(n2:-1:1).val f2.val];
	mag = abs( polyval(h2,passband) );
	if max(abs(mag-1)) < delta2;
	    success =1;
	    break;
	end
    end
    if success
	break;
    end
end

if ~success
    f2 = []; 
    q2 = [];
end
