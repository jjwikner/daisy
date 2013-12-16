 	function [L, C, K] = BW_LADDER(Wc, Ws, Amax, Amin, N,  Rs, RL, Ladder)	
	%
	%	Computes the element values for a doubly resistively 
	%	terminated LC ladder of Butterworth type
	%
	%	Ladder = 1 for a T ladder and 2 for a ¹ ladder
	%	Rs = source resistor and RL = load resistor
	%	K is a vector used by LADDER_2_H that defines the type
	%	of elements in the ladder.
	%
	% 	Toolbox for ANALOG FILTERS USING MATLAB, Springer, 2009
	
	% 	Author: 			Lars Wanhammar, 1983-08-18
	% 	Modified by: 	 	LW 1987-01-19/1987-07-28/2005-11-21/2007-06-26
	%	Copyright:			Divison of Electronics Systems
	%	 					Dept. Electrical Engineering, Linkoping University, Sweden
	% 	Version:			1 
	% 	Known bugs:			 None
	% 	Report bugs to:		larsw@isy.liu.se
	%
	
	Print = 0;	% Set Print = 0 for no printing
	if (Rs == 0 | Rs == inf | RL == 0 | RL == inf)
		H = BW_SINGLY_LADDER(N, Amax, Wc, Rs, RL, Ladder);
	end
	n = 1:N;
	A = sin((n-0.5)*pi/N);
	d = (1 - 4*Rs*RL/(Rs+RL)^2)^(0.5/N);
	B = 1 + d^2 - 2*d*cos(n*pi/N);
	G(1) = 2*A(1)/(1 - d);
	Rnorm = Rs;
	for indx = 2:N
		G(indx) = 4*A(indx)*A(indx-1)/(B(indx-1)*G(indx-1));
	end
	L = zeros(1,N); C = zeros(1,N);
	if mod(N, 2) == 0 			
		if (Rs > RL & Ladder == 1) | (Rs < RL & Ladder == 2)
			disp(['This case do not exist'])
			return;
		end 	
	else
		if (Rs > RL & Ladder == 1) | (Rs < RL & Ladder == 2)
			G = fliplr(G);	Rnorm = RL;
		end 
	end
	rp0 = Wc*(10^(0.1*Amax)-1)^(-1/(2*N));
	G = G/rp0;	
	for indx = 1:2:N-1  % Define the two-ports
		if Ladder == 1	% T Ladder 
			L(indx) = G(indx); C(indx+1) = G(indx+1);
			K(indx) = 2; K(indx+1) = 9;
		else 	% ¹ Ladder 
			C(indx) = G(indx); L(indx+1) = G(indx+1);
			K(indx) = 9; K(indx+1) = 2;
		end 
	end
	if mod(N,2) == 1
		if Ladder == 1
			L(N) = G(N); K(N) = 2; 
		else 
			C(N) = G(N); K(N) = 9; 
		end
	end
	L = Rnorm*L; C = C/Rnorm;
	if Print == 1	% Printing
		disp(['Rs = ', num2str(Rs),' ohm'])	
		for indx = 1:2:N-1
			if Ladder == 1	% T Ladder 
				disp(['L', num2str(indx),' = ', num2str(L(indx)),' H'])	
				disp(['C', num2str(indx+1),' = ', num2str(C(indx+1)), ' F'])
			else 	% ¹ Ladder
				disp(['C', num2str(indx),' = ', num2str(C(indx)), ' F'])
				disp(['L', num2str(indx+1),' = ', num2str(L(indx+1)), ' H'])
			end 
		end
		if mod(N,2) == 1
			if Ladder == 1
				disp(['L', num2str(N),' = ', num2str(L(N)),' H'])
			else
				disp(['C', num2str(N),' = ', num2str(C(N)), ' F'])
			end
		end 
		disp(['RL = ', num2str(RL),' ohm'])
	end
