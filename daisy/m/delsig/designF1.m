function [f1_saved,zetap,phi] = designF1(delta, fp1)
% [f1 zetap phi] = designF1(delta, fp1)		Design the F1 sub-filter
% of a Saramaki halfband filter. This function is called by designHBF.m.
%
% f1    A structure array containing the F1 filter coefficents and
%       Their CSD representation.
% phi	The scaling factor for the F2 filter (imbedded in the f1 coeffs.)

passband = exp(4*pi*j*linspace(0,fp1));
ok = 0;
for n1 = 1:2:7 	% Odd values only
    if n1 == 1
	h = [0.5 0.5];
    else
	h = remez(2*n1-1,[0 4*fp1 1 1],[1 1 0 0]);
	if ~(abs(sum(h)-1) < 1e-3 )		% remez bug! Use firls instead
	    h = firls(2*n1-1,[0 4*fp1 1-1e-6 1],[1 1 0 0]);
	end
    end
    fresp = abs( polyval(h,passband) );
    if max( abs(fresp-1) ) <= delta
	ok = 1;
	break
    end
end
if ~ok
    zetap = 1;	% Use this as an indication that the function failed.
    return
end

% Transform h(n) to a chebyshev polynomial f1(n)
% Sum(f1(i)*cos(w)^n)|i=1:n1 + Sum(h(n1+i))*cos(n*w))|i=1:n1, n = 2*i-1;
w = pi*rand(1,n1);
cos_w = cos(w);
A = zeros(n1,length(w));
B = zeros(1,n1);
for i = 1:n1
    n = 2*i-1;
    A(i,:) = cos_w .^ n;
    B = B + h(n1+i)* cos(n*w);
end
f1 = B/A;

% Matlab Ver. 5 change:
phivecb = [];

% Optimize the quantized version of f1 to maximize the stopband width 
% ( = acos(zetap) )

zetap = 1;
testPoints = [0 logspace(-2,0,128)] - 1;
for nsd = 3:8
    f1a = f1'; f1b = f1'; 		% First try the unperturbed filter.
    for phia = 1 ./ [1 f1]
	phia = phia / 2^nextpow2(phia); % keep phi in (0.5,1]
	% Try a bunch of coefficients in the current neighborhood,
	% shrinking the neighborhood once 10 successive trial values show no
	% improvement.  If 2 successive shrinkages do no good, try a higher nsd.
	count = 0;
	nohelp = 0;
	neighborhood = .05;
	while neighborhood > 1e-5
	    phivec = phia .^ [1:2:2*n1-1]';
% Matlab Ver. 5 change:
	    if isempty(phivecb); phivecb = phivec; end
	    f1q = bquantize( f1a.*phivec, nsd );
	    F1 = evalF1( [f1q.val], testPoints, phia );
	    fi = find( abs(F1) > delta ); 
	    zeta = -testPoints( max( fi(1)-1, 1 ) );
	    %fprintf(2,'nsd=%d, nbhd= %f, count=%d, zeta = %f, phia=%f\n', ...
	    %  nsd, neighborhood, count, zeta, phia );
	    if zeta < zetap
		count = 0;
		nohelp = 0;
		zetap = zeta;
		f1b = [f1q.val]';
		f1_saved = f1q;
		phi = phia;
		phivecb = phivec;
	    else
		count = count + 1;
	    end
	    if count > 10
		count = 0;
		neighborhood = neighborhood/2;
		nohelp = nohelp +1;
		if nohelp > 2
		    break;
		end
	    end
	    f1a = f1b./phivecb + neighborhood*(rand(size(f1b))-0.5);
	    phia = phia + neighborhood*(rand(1,1)-0.5);
	end
	if zetap < 1	% Found a filter with adequate attn.
	    break;
	end
    end			% for phia ...
    if zetap < 1	% Found a filter with adequate attn.
	break;
    end
end
