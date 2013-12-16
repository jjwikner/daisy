	function EE = lp_get_ee(Z,NIN,Amax)	%  	% Returns the E*conj(E) given an  attenuation pole 	% vector, the number of attenuation poles at infinity  	% and the passband attenuation Amax.		% Author: 			Per Loewenborg    % Modified by:		LW	% Copyright:		Divison of Electronics Systems	%					Dept. Electrical Engineering, Linkoping University, Sweden	% Version: 			1		% Known bugs:		None	% Report bugs to:	larsw@isy.liu.se		ep_sqred = (10^(0.1*Amax))-1;	% Find F^2 and Q^2	N = length(Z);	if N > 0		F2 = [1 2*Z(1) Z(1)^2];		Q2 = [1 0 -2*(Z(1)^2) 0 Z(1)^4];		for k = 2:N			F2 = conv(F2,[1 2*Z(k) Z(k)^2]);  			Q2 = conv(Q2,[1 0 -2*(Z(k)^2) 0 Z(k)^4]);		end		if NIN > 0			for k = 1:NIN				F2 = conv(F2,[1 1]);				Q2 = conv(Q2,[1 0 -1]);			end    		end		% Get the even part of F2		N = length(F2);		if mod(N,2) 	    % Odd length-start with nulling  the second			for k = 2:2:N				F2(k) = 0;			end		else 		    % Even length start with nulling the first			for k = 1:2:N				F2(k) = 0;			end  			for k = 2:length(F2) % No zero first				F2(k-1) = F2(k);			end			F2 = F2(1:length(F2)-1);		end		F2 = conv(F2,F2);		F2 = ep_sqred*F2;		Q2 = Q2*((-1)^NIN);	else % No attenuation poles  		F2 = [1];		Q2 = [1];		if NIN > 0			for k = 1:NIN				F2 = conv(F2,[1 1]);				Q2 = conv(Q2,[1 0 -1]);			end    		end  		N = length(F2);		if mod(N,2) 	 % Odd length-start with nulling  the second			for k = 2:2:N				F2(k) = 0;			end		else % Even length start with nulling the first			for k = 1:2:N				F2(k) = 0;			end  			for k = 2:length(F2) 		    % No zero first				F2(k-1) = F2(k);			end			F2 = F2(1:length(F2)-1);		end		F2 = conv(F2,F2);		F2 = ep_sqred*F2;		Q2 = Q2*((-1)^NIN);	end	dif = length(Q2) - length(F2);	if dif > 0		z = zeros(1,dif);		F2 = [z F2];	else		z = zeros(1,dif);		Q2 = [z Q2];	end	EE = F2+Q2;