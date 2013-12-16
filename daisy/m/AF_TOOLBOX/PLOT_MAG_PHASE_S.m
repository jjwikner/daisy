	function PLOT_MAG_PHASE_S(Mag, Phase, omega, axis_Amax)	%	%	Plots the magnitude and phase for an analog filter in a single figure	%	% Toolbox for ANALOG FILTERS USING MATLAB, Springer, 2009		% Author: 			Lars Wanhammar 2007-09-13	% Modified by: 	 	 	% Copyright:		Divison of Electronics Systems	% 					Dept. Electrical Engineering, Linkoping University, Sweden	% Version:			1  	% Known bugs:		None	% Report bugs to:	larsw@isy.liu.se	%	%========================================================		% Standard settings	fs = 16; % Font size	lw = 2; % Linewidth	fn = 'times'; % Font	% PowerPoint settings	%	fs = 16; % Font size	%	lw = 2; % Linewidth	%	fn = 'times'; % Font	%======================================================== 	subplot('position', [0.4 0.4 0.88 0.5]);	[AX, H1, H2] = plotyy(omega, Mag, omega, Phase);	set(H1, 'linewidth', lw);	set(H2, 'linewidth', lw);	Phase = Phase/360+0.5;	set(AX(1), 'ylim', [0 axis_Amax+0.1]);	set(AX(2), 'ytick', [0:0.2:1.1]);	grid on;	hold on;	set(gca,'FontName', fn,'FontSize', fs);	ylabel('|{\itH}({\itj\omega})|  [dB]','FontName',fn,'FontSize',fs);		xlabel('{\it\omega}  [rad/s]','FontName', fn,'FontSize',fs);	axes(AX(2))	ylabel('Phase(\it\omega)  [degrees]','FontName', fn,'FontSize',fs);	