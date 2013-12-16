	function [Z0, RL, KI] = RICHARDS_EQ(N, Amax, Rs, Wc, T)	%	% 	Computes the characteristic resistances in a doubly resistively	% 	terminated Richards' structure with equiripple passband with 	%	source resistor Rs. Wc is the cutoff frequency and T is the forwards and	%	back propagation time for the commensurate unit elements.		%	% 	Toolbox for ANALOG FILTERS USING MATLAB, Springer, 2009			% 	Author: 		Lars Wanhammar 2007-07-02	% 	Modified by: 			% 	Copyright:		Divison of Electronics Systems	% 					Dept. Electrical Engineering, Linkoping University, Sweden	% 	Version: 		1	% 	Known bugs:		Works only for small bandwiths	% 	Report bugs to:	larsw@isy.liu.se		epsilon = sqrt(10^(0.1*Amax)-1);	if mod(N, 2) == 1		RL = Rs;	else		RL = Rs*(sqrt(1+epsilon^2)-epsilon)/(sqrt(1+epsilon^2)+epsilon);	end	ata = sinh(asinh(1/epsilon)/N);	A = ones(1, N)/ata;	for m = 2:N		for k = 1:m/2			A(m) = A(m)*(ata^2 + sin((m-2*k)*pi/N)^2)/(ata^2 + sin((m-2*k+1)*pi/N)^2);		end	end	n = 1:N;	alfa = sin(Wc*T);	gama = (ata^2+sin(n*pi/N).^2)./sin((n+0.5)*pi/N)+(ata^2+sin((n-1)*pi/N).^2)./sin((n-1.5)*pi/N);	Z0 = Rs*A.*(2*sin((n-0.5)*pi/N)/alfa - 0.25*alfa*gama);	for k = 2:2:N		Z0(k) = Rs^2/Z0(k);	end	KI = 6*ones(1, N);