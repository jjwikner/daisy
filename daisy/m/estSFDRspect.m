function [SFDR, carr_pos, spur_pos] = ... 
     estSFDRspect(input_spect, exp_carr_pos);

%
% [SFDR, carr_pos, spur_pos] = ... 
%     estSFDRspect(input_spect, exp_carr_pos);
%
% The SFDR is estimated from the signal spectrum, input_spect. 
% Note, unlike the output from FFT this spectrum is half-sided 
% only! Hence, from 0 Hz to Fsample/2. Generate the proper
% spectrum with for example the function spect20.m
% Only giving the input spectrum, the algorithm searches for 
% the peak bin and assigns this position to the carrier. 
% Using the additional exp_carr_pos [0:0.5] value, the 
% algorithm can be told to use another interval to find 
% the signal carrier. The frequency interval 
%      (exp_carr_pos +/- 5%)*(length input_signal)
% is used.
%
% The SFDR and the normalized positions of the carrier and 
% the spurious (0.5 <-> Fsample/2) are returned.
%
% (c) JJWikner, MERC, Ericsson Components AB. April 28, 1999.
% Marbella, Spain. Plaza de Naranjos.
% JJWikner, -.-, modified version January 31, 2000. Not in Spain.
%
% Revision: Some strange oddities to the algorithm. Works,
% but could be made much more attractive...

    
m = floor(length(input_spect));

max_val = max(input_spect); 	    % (Should be zero returned from spect20)
spectrum = input_spect - max_val;   % Correction anyway...
    max_val = 0; 

carr_pos = find(spectrum == max_val);
carr_pos = carr_pos(1);
%deltac = 0.02;
deltac = 0.04;
carr_interval = carr_pos - floor(deltac*m) : carr_pos + floor(deltac*m);
dc_interval = 1:floor(deltac*m/3);
neglect_interval = [dc_interval carr_interval];
SFDR=inf;
allowed_spur = 0;
stop = 0;
threshold = 0;
deltat=10;
while ~stop
  threshold = threshold - deltat;
  larger_pos = find(spectrum > threshold);
  % pause; figure(1); plot(spectrum); length(larger_pos)
  for lpos = 1:length(larger_pos)
    lp_pos = larger_pos(lpos);
    if size(find(neglect_interval == lp_pos),1) 
      % The peak is in the unwanted frequency range.
      spectrum(lp_pos) = -inf;
    else
      SFDR_found = -spectrum(lp_pos);
      if (SFDR_found < SFDR)
	SFDR = SFDR_found;
	spur_pos = lp_pos;
      else
	spectrum(lp_pos) = -inf;
      end;
      allowed_spur = 1;
    end;
  end;
  if (allowed_spur)
    deltat = deltat/2;
    threshold = threshold + 2*deltat;
  end;
  stop = (deltat < 0.5);
  % hold on; plot(1:m, threshold*ones(1,m),'r--'); hold off; threshold
end;

% Use normalized  values:
carr_pos = carr_pos / (2*m);
spur_pos = spur_pos / (2*m);
