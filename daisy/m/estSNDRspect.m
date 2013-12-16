function [SNDR, carr_pos] = ... 
     estSNDRspect(input_spect, os_ratio, exp_carr_pos);

%
% function [SNDR, carr_pos] = ... 
%     estSNDRspect(input_spect, os_ratio, exp_carr_pos);
%
% The SNDR is estimated from the input_spect. Using the
% additional exp_carr_pos [0:0.5] value, the algorithm can
% be told to use another interval to find the signal carrier.
% The following frequency interval is investigated:
%      (exp_carr_pos +/- 5%)*(length input_signal)
% 
% The function returns the SNDR and the normalized position
% of the carrier. (0.5 <-> Fsample/2)
%
% os_ratio determines the interval in which the signal/noise
% should be determined. os_ratio = 1 implies a Nyquist converter.
% The frequency band from 0 to 0.5/os_ratio is used.
% 
% estSNDRspect(input_spect) finds the strongest ac signal component
% and determines the SNDR within the Nyquist band.
%
% (c) JJWikner, MERC, Ericsson Components AB. April 28, 1999.
% Marbella, Spain. Plaza de Naranjos.
% 
% Changes to the code at Linköping university, 990701, JJW
% Changes to the code at Linköping university, 000608, JJW


k = length(input_spect);
m = floor(k);
spectrum = input_spect;

if (nargin == 1)
  os_ratio = 1;
end;

if (nargin == 3)
  search_interval = floor((exp_carr_pos-0.05)*m):ceil((exp_carr_pos+0.05)*m);
  max_val = max(spectrum(search_interval)); % (Should be zero returned from spect20)
  spectrum = spectrum - max_val; max_val = 0;  % Correction anyway.
  carr_pos = find(spectrum == max_val);
  carr_pos = carr_pos(1);
else
  max_val = max(spectrum); 	    % (Should be zero returned from spect20)
  spectrum = spectrum - max_val; max_val = 0; % Correction anyway.
  carr_pos = find(spectrum == max_val);
  carr_pos = carr_pos(1);
end;

%deltac = 0.02 ;
deltac = 0.04 ;

carr_interval = carr_pos - floor(deltac*m) : carr_pos + floor(deltac*m);
dc_interval = 1:floor(deltac*m/3);
neglect_interval = [dc_interval carr_interval];
outband_interval = floor(m/os_ratio):m;
lin_pwr = 10.^(spectrum/10);

signal_power = sum(lin_pwr(carr_interval));
noise_power = sum(lin_pwr) - ...
    sum(lin_pwr(neglect_interval)) - ...
    sum(lin_pwr(outband_interval));

% Use normalized  values:
carr_pos = carr_pos / k;
SNDR = 10*log10(signal_power/noise_power);

