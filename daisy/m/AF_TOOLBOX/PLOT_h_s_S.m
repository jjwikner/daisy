	function PLOT_h_s_S(h, h_scale, s_of_t, t_axis, tmax, ymin, ymax)	%	%	Plots the impulse and step responsess for an analog filter in a single figure	%	% Toolbox for ANALOG FILTERS USING MATLAB, Springer, 2009		% Author: 			Lars Wanhammar 2007-09-13	% Modified by: 	 	 	% Copyright:		Divison of Electronics Systems	% 					Dept. Electrical Engineering, Linkoping University, Sweden	% Version:			1	% Known bugs:		None	% Report bugs to:	larsw@isy.liu.se	%	%========================================================		% Standard settings	fs = 16; % Font size	lw = 2; % Linewidth	fn = 'times'; % Font	% PowerPoint settings	%	fs = 16; % Font size	%	lw = 2; % Linewidth	%	fn = 'times'; % Font	%========================================================	figure	subplot('position', [0.08 0.4 0.86 0.5]);	axis([0, tmax, ymin, ymax]);	plot(t_axis, h*h_scale,'linewidth',lw); 	hold on;	plot(t_axis, s_of_t,'linewidth',lw); 	grid on		xlabel('{\itt}   [s]','FontName',fn,'FontSize',fs); 	set(gca,'FontName', fn,'FontSize', fs);	% write text in the figure	text(3*10^-4, 0.9,'{\its}({\itt})','FontName',fn,'FontSize',fs);	text(3*10^-4, 0.3,['{\ith}({\itt})*',num2str(h_scale)],'FontName',fn,'FontSize',fs);