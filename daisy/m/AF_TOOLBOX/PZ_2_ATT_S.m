	function A = PZ_2_ATT_S(G, Z, P, W)	%	% 	Computes the attenuation for an analog filter significantly 	% 	more accurate than the standard MATLAB routine freqs.	%	% 	Replaces the MATLAB routine freqs	%	% 	Toolbox for ANALOG FILTERS USING MATLAB, Springer, 2009	% 	Author: 			Lars Wanhammar, 2007-02-19	% 	Modified by: 			% 	Copyright:		Divison of Electronics Systems	% 					Dept. Electrical Engineering, Linkoping University, Sweden	% 	Version: 		1	% 	Known bugs:		None	% 	Report bugs to:	larsw@isy.liu.se		H = G*ones(1,length(W));		W = i*W;		for n = 1:length(Z)			H = H.*(W - Z(n));    		end		for n = 1:length(P)			H = H./(W - P(n));    		end		A = MAG_2_ATT(H);