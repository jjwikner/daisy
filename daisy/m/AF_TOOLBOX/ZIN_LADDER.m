	function [ZinNum, ZinDen] = ZIN_LADDER(R_ZEROS, Z, P, Rs, RL)	%	% 	Computes the input impedance (ZinNum/ZinDen) to a lowpass LC ladder 	%	filter with finite zeros.	%	% 	Toolbox for ANALOG FILTERS USING MATLAB, Springer, 2009		%	Author: 		Lars Wanhammar 2007-08-26					%	Modified by: 	%	Copyright:		Divison of Electronics Systems	%					Dept. Electrical Engineering, Linkoping University, Sweden	%	Version: 		1	%	Known bugs:	 	None	%	Report bugs to:	larsw@isy.liu.se		ZinNum = Rs*POLYADD(real(ROOTS_2_POLY(P)), real(ROOTS_2_POLY(R_ZEROS)));	ZinDen = POLYSUB(real(ROOTS_2_POLY(P)), real(ROOTS_2_POLY(R_ZEROS)));	ZinDen(abs(ZinDen(1)) < 10*eps) = []; % Normalize to highest power first