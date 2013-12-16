	function [P, Wpas] = EQ_TG_LP_S(W1, W2, G, Z, P, Nap)	%	% 	Determine poles for an allpass filter that corrects the group delay	% 	for an analog lowpass filter in approximately equiripple sence.	%	% 	Toolbox for ANALOG FILTERS USING MATLAB, Springer, 2009		% 	Author: 		Lars Wanhammar, 2007-11-19	% 	Modified by: 		 	% 	Copyright:		Divison of Electronics Systems	% 					Dept. Electrical Engineering, Linkoping University, Sweden	% 	Version: 		1	% 	Known bugs:		None	% 	Report bugs to:	larsw@isy.liu.se	%	global FX LDT Dmin NF NL V Q0 Q1 QA QB QC QD0 QD1 QF1 Tol Tol1 H X	global p TgH  Wpas points ToP	points = 100*Nap; n = [0:points];	Wpas = W1 + (W2-W1)*sin(n*pi/(2*points));	% LP	%	Wpas = W2 - (W2-W1)*cos(n*pi/(2*points));	% HP	%	Wpas = (W2+W1)/2 - (W2-W1)*cos(n*pi/(2*points))/2;	% BP		TgH = PZ_2_TG_S(G, Z, P, Wpas);	if mod(Nap,2) == 1			% Select initial poles of the allpass sections		X(Nap) =  -0.15*W2;	end	for n = 1:2:Nap-1		X(n) = -0.15*W2; X(n+1) = -n*(W2-W1)/Nap;	end	ToP = eye(Nap); ToP(2:2*Nap + 2:Nap^2) = 1;	ToP(Nap + 1:2*Nap + 2:Nap^2) = i; ToP(Nap + 2:2*Nap + 2:Nap^2) = -i;		h = 1;	Tol = 10^-13; prin = 0; Fmin = 0; SCBD = 2; KTM = 1;	ILLC = (1==0);		% illc = false;		for p = 2:2:10		[X, fin] = PRAXIS('EQAPLP', Nap, h, prin, ILLC, SCBD, KTM, Fmin);	end	P = ToP*X';	Taug = PZ_2_TG_S(1, -P, P, Wpas);	Minimax_ERROR = max(Taug+TgH)-min(Taug+TgH)	return