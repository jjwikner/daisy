 	function [L, C, K] = CH_I_LADDER(Wc, Ws, Amax, Amin, N,  Rs, RL, Ladder)	%	%	Computes the element values for a doubly resistively 	%	terminated LC ladder of Chebyshev I type.	%	%	Ladder = 1 for a T ladder and 0 for a � ladder	%	Rs = source resistor and RL = load resistor	%	K is a vector used by LADDER_2_H that defines the two-ports.	%	% 	Toolbox for ANALOG FILTERS USING MATLAB, Springer, 2009		% 	Author: 			Lars Wanhammar, 1983-08-18	% 	Modified by: 	 	LW 1987-01-19/1987-07-28/2005-11-21/2007-06-26	%	Copyright:			Divison of Electronics Systems	%	 					Dept. Electrical Engineering, Linkoping University, Sweden	% 	Version:			1 	% 	Known bugs:			None	% 	Report bugs to:		larsw@isy.liu.se	%		Print = 0;	% Set Print = 0 for no printing	if (Rs == 0 | Rs == inf | RL == 0 | RL == inf)		H = CH_I_SINGLY_LADDER(N, Amax, Wc, Rs, RL, Ladder);	end	n = 1:N;	A = sin((n-0.5)*pi/N);		Apn = Amax*log(10)/20;	epsilon2 = 2*exp(Apn)*sinh(Apn);	RLnorm = RL/Rs;	Aterm = 4*RLnorm/(1 + RLnorm)^2;	if mod(N, 2) == 0  Aterm = Aterm*(epsilon2+1); end	Gama = sinh(asinh(1/sqrt(epsilon2))/N);	d = sinh(asinh(sqrt((1 - Aterm)/epsilon2))/N);	B = Gama*Gama + d*d - 2*Gama*d*cos(n*pi/N)+sin(n*pi/N).^2;	G(1) = 2*A(1)/(Gama-d);	for indx = 2:N		G(indx) = 4*A(indx)*A(indx-1)/(B(indx-1)*G(indx-1));	end	L = zeros(1,N); C = zeros(1,N); Rnorm = Rs;	if mod(N,2) == 0 					if (Rs > RL & Ladder == 1) | (Rs < RL & Ladder == 0)			disp(['This case do not exist'])			return;		end 		else		if (Rs > RL & Ladder == 1) | (Rs < RL & Ladder == 0)			G = fliplr(G);	Rnorm = RL;		end 		end	G = G/Wc;		for indx = 1:2:N-1 % Define the two-ports		if Ladder == 1	% T Ladder 			L(indx) = G(indx); C(indx+1) = G(indx+1);			K(indx) = 2; K(indx+1) = 9; 		else 	% � Ladder 			C(indx) = G(indx); L(indx+1) = G(indx+1);			K(indx) = 9; K(indx+1) = 2;		end 	end	if mod(N,2) == 1		if Ladder == 1			L(N) = G(N); K(N) = 2; 		else 			C(N) = G(N); K(N) = 9; 		end	end	L = Rnorm*L; C = C/Rnorm;	if Print == 1	% Printing		disp(['Rs = ', num2str(Rs),' ohm'])			for indx = 1:2:N-1			if Ladder == 1	% T Ladder 				disp(['L', num2str(indx),' = ', num2str(L(indx)),' H'])					disp(['C', num2str(indx+1),' = ', num2str(C(indx+1)), ' F'])			else 	% � Ladder				disp(['C', num2str(indx),' = ', num2str(C(indx)), ' F'])				disp(['L', num2str(indx+1),' = ', num2str(L(indx+1)), ' H'])			end 		end		if mod(N,2) == 1			if Ladder == 1				disp(['L', num2str(N),' = ', num2str(L(N)),' H'])			else				disp(['C', num2str(N),' = ', num2str(C(N)), ' F'])			end		end 		disp(['RL = ', num2str(RL),' ohm'])	end	