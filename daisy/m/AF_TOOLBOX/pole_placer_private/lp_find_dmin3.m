	function [dmin,zout] = lp_find_dmin3(zstep,zatt,zmin,amin,Amax,NIN)	%  	% Finds the minimum distances to the specification for a stopband filter.	% Also the z-frequencies are found.	% zstep = vector containing the step frequencies	% zatt = vector containing the attenuation poles	% zmin = vector containing the arc minimum frequencies	% amin = vector containing the stopband attenuation requirement	% Amax = passband ripple		% Author: 			Per Loewenborg    % Modified by:		LW	% Copyright:		Divison of Electronics Systems	%					Dept. Electrical Engineering, Linkoping University, Sweden	% Version: 			1		% Known bugs:		None	% Report bugs to:	larsw@isy.liu.se		zout = [];	dmin = [];	for k = 1:length(zatt)	  if k == 1 	    zsub = [zstep(find(zstep <= zatt(1)))];	    asub = lp_sb_atten_mf(zatt,NIN,zsub,Amax);	    temp = 0;	    for l = 1:length(zsub)	      temp(l) = asub(l)-lp_get_spec(zstep,amin,zsub(l));	    end	    [dummy,min_index] = min(temp);	    dmin = [dmin dummy];	    zout = [zout zsub(min_index)];	  else	    zsub = [zstep(find(and(zstep <= zatt(k),zstep > zatt(k-1))))...		    zmin(find(and(zmin <= zatt(k),zmin > zatt(k-1))))];	    asub = lp_sb_atten_mf(zatt,NIN,zsub,Amax); 	    temp = 0;	    for l = 1:length(zsub)	      temp(l) = asub(l)-lp_get_spec(zstep,amin,zsub(l));	    end	    [dummy,min_index] = min(temp);    	    dmin = [dmin dummy];	    zout = [zout zsub(min_index)];    	  end	end 	  zsub = [zstep(find(zstep > zatt(length(zatt))))...		  zmin(find(zmin > zatt(k)))];	  asub = lp_sb_atten_mf(zatt,NIN,zsub,Amax);	  temp = 0;	  for l = 1:length(zsub)	    temp(l) = asub(l)-lp_get_spec(zstep,amin,zsub(l));	  end	  [dummy,min_index] = min(temp);   	  dmin = [dmin dummy];	  zout = [zout zsub(min_index)]; 