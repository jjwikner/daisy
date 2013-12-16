	function A = lp_get_spec(zstep,amin,z)	% 	% Returns the specification in the stopband for a given 	% frequency z, given the stepfrequencies and the 	% piecewise constant stopband requirements.	% Use for lowpass filters.	% A = lp_get_spec(zstep,amin,z)		% Author: 			Per Loewenborg    % Modified by:		LW	% Copyright:		Divison of Electronics Systems	%					Dept. Electrical Engineering, Linkoping University, Sweden	% Version: 			1		% Known bugs:		None	% Report bugs to:	larsw@isy.liu.se		low_index = max(find(zstep <= z));	high_index = min(find(zstep >= z));	if isempty(low_index)		disp('Error-frequency out of range');		A = 0;	else		if isempty(high_index)			A = amin(low_index);		elseif low_index == high_index			if low_index == 1				A = amin(1);			else				A = max(amin(low_index-1),amin(low_index));			end		else			A = amin(low_index);    		end	end