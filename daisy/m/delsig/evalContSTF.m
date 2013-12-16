function G=evalContSTF(L0,H,f)
% G=evalContSTF(L0,H,f)
% Compute the signal transfer function at a frequency f.
% L0 and H are cts-time and discrete-time transfer functions, respectively.
% Both L0 and H are TF structs


fprintf(1,'Warning. evalContSTF() called. Use evalTFP() instead.\n');

spoles = L0.poles;	% These are actually designed in already.
zzeros = H.zeros;	% These correspond via z=exp(s) (see LCparam2tf)

G = zeros(size(f));
w = 2*pi*f;	s = j*w;	z=exp(s);
for i=1:length(f)
    wi = w(i);	si = s(i);	zi = z(i);
    if isempty(spoles)
        cancel = 0;
    else
        cancel = abs(si-spoles)<1e-5;
    end
    if ~cancel
    	% wi is far from a pole, so just use the product L0*H
    	G(i) = evalTF(L0,si) * evalTF(H,zi);
    else
    	% cancel pole(s) of L0 with the corresponding zero(s) of H
    	if( norm(zi-zzeros(cancel)) > 1e-4 )
    	 fprintf(1,'%s: error. NTF zeros do not match L0 poles at w=%g\n', ...
    	   mfilename ,wi);
    	    return
	end
	G(i) =	evalRPoly(L0.zeros,si,L0.k) * ...
		zi^sum(cancel) * evalRPoly(zzeros(~cancel),zi,1) / ... 
		(evalRPoly(spoles(~cancel),si,1)*evalRPoly(H.poles,zi,1));
    end
end
