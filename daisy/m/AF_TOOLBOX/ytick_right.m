	function h = ytick(yt, ytl, h)	% YTICK Set yticks of current plot.	%	% YTICK(Y) sets the YTick property of current axis to the values in the	% vector Y.	%	% YTICK(T) sets the YTickLabel property of current axis to the strings in	% the cell array T. The FontName and FontSize properties are the same as in	% the current axis.	%	% YTICK(Y,T) sets the YTick property of current axis to the values in the	% vector Y and the YTickLabel property of current axis to the strings in	% the cell array T.	%	% H = YTICK(Y,T) gives the handle vector to the tick marks.	%	% Example:	%	% >> Y = [-pi 0 pi];	% >> T = {'-\pi';'0';'\pi'};	% >> ytick(Y,T);	%	% See also XTICK, SET, GET, and TEXT	%	% Magnus Borga	% CVL, Link�pings universitet	% 980126		if nargin == 1	    if iscell(yt)	      ytl = yt;	      clear yt	      yt = get(gca,'YTick');	    else	      set(gca,'YTick', yt)	      return	    end	else	    set(gca,'YTick',yt)	end	set(gca,'yticklabel',[' '])	fn = get(gca,'FontName');	fs = get(gca,'FontSize');	xlim = get(gca,'XLim');	pos = xlim(2)+(xlim(2)-xlim(1))/60;	for p = 1:length(yt)	    h = text(pos+ 0.003, yt(p), ytl{p},'FontSize',fs,'FontName',fn,'HorizontalAlignment','left','VerticalAlignment','middle');	end	