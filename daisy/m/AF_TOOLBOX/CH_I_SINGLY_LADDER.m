 	function [L, C, KI] = CH_I_SINGLY_LADDER(Wc, Ws, Amax, Amin, N,  Rs, RL, Ladder)		%	%	Computes the element values for a doubly resistively 	%	terminated ladder of Chebyshev I type	%	%	Ladder = 1 for a T ladder and 0 for a � ladder	%	Rs = source resistor and RL = load resistor	%	KI is a vector used by LADDER_2_H that defines the two-ports.	%	% 	Toolbox for ANALOG FILTERS USING MATLAB, Springer, 2009	%	% 	Author: 			Lars Wanhammar, 1983-08-18	% 	Modified by: 	 	LW 1987-01-19/1987-07-28/2005-11-21/2007-06-26	%	Copyright:			Divison of Electronics Systems	%	 					Dept. Electrical Engineering, Linkoping University, Sweden	% 	Version:			1 	% 	Known bugs:			None	% 	Report bugs to:		larsw@isy.liu.se	%		Print = 1;	% Set Print = 0 for no printing	if (Rs ~= 0 & RL ~= 0 & Rs ~= inf & RL ~= inf)		disp(['Use CH_I_LADDER instead']); return	end	n = 1:N;	A = sin((n-0.5)*pi/N);		Apn = Amax*log(10)/20;	epsilon2 = 2*exp(Apn)*sinh(Apn);	Aterm = 4*RL*Rs/(Rs + RL)^2;	if mod(N,2) == 0 Aterm = Aterm*(epsilon2+1); end	Gama = sinh(asinh(1/sqrt(epsilon2))/N);	B = (Gama*Gama + sin(n*pi/(2*N)).^2).*cos(n*pi/(2*N)).^2;	beta = asinh(1/sqrt(epsilon2));	G(1) = A(1)/Gama;	for indx = 2:N		G(indx) = A(indx)*A(indx-1)/(B(indx-1)*G(indx-1));	end	L = zeros(1, N); C = zeros(1,N); Rnorm = Rs;	if Rs ~= 0 & Rs ~= inf		if  Ladder == 0			if (mod(N,2) == 0 & (RL == inf | RL > tanh(beta/2)^2)) | (mod(N,2) == 1 & RL == 0)				disp(['This case do no exist']); return			end		else			if mod(N,2) == 0 & RL < 1/tanh(beta/2)^2				disp(['This case do no exist']); return			end		end	end	if Rs == inf & RL ~= 0 & RL ~= inf		if Ladder == 0 			G = fliplr(G); Rnorm = RL;		else			disp(['This case do no exist']); return		end	end	if Rs == 0 & RL ~= inf & RL ~= 0		if Ladder == 0				disp(['This case do no exist']); return		else			G = fliplr(G); Rnorm = RL;		end	end	if (Rs == 0 & (RL == 0| RL == inf)) | (Rs == inf & (RL == 0| RL == inf))		disp(['This case do no exist']); return	end 	G = G/Wc;		for indx = 1:2:N-1 % Define the two-ports		if Ladder == 1	% T Ladder 			L(indx) = G(indx); C(indx+1) = G(indx+1);			KI(indx) = 2; KI(indx+1) = 9; 		else 	% � Ladder 			C(indx) = G(indx); L(indx+1) = G(indx+1);			KI(indx) = 9; KI(indx+1) = 2;		end 	end	if mod(N,2) == 1		if Ladder == 1			L(N) = G(N); KI(N) = 2; 		else 			C(N) = G(N); KI(N) = 9; 		end	end	L = Rnorm*L; C = C/Rnorm;	if Print == 1	% Printing		disp(['Rs = ', num2str(Rs),' ohm'])			for indx = 1:2:N-1			if Ladder == 1	% T Ladder 				disp(['L', num2str(indx),' = ', num2str(L(indx)),' H'])					disp(['C', num2str(indx+1),' = ', num2str(C(indx+1)), ' F'])			else 	% � Ladder				disp(['C', num2str(indx),' = ', num2str(C(indx)), ' F'])				disp(['L', num2str(indx+1),' = ', num2str(L(indx+1)), ' H'])			end 		end		if mod(N,2) == 1			if Ladder == 1				disp(['L', num2str(N),' = ', num2str(L(N)),' H'])			else				disp(['C', num2str(N),' = ', num2str(C(N)), ' F'])			end		end 		disp(['RL = ', num2str(RL),' ohm'])	end