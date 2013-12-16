function [ENOB, carr_pos] = ... 
     estENOB(input_signal, os_ratio, exp_ratio);

%
% function [ENOB, carr_pos] = ... 
%     estENOB(input_signal, exp_carr_pos);
%
% The ENOB is estimated from the input_signal. Using the
% additional exp_carr_pos [0:0.5] value, the algorithm can
% be told to use another interval to find the signal carrier.
% The following frequency interval is investigated:
%      (exp_carr_pos +/- 5%)*(length input_signal)
% 
% The function returns the ENOB and the normalized position
% of the carrier. (0.5 <-> Fsample/2)
%
% os_ratio determines the interval in which the signal/noise
% should be determined. os_ratio = 1 implies a Nyquist converter.
% The frequency band from 0 to 0.5/os_ratio is used.
% 
% estENOB(input_signal) finds the strongest ac signal component
% and determines the ENOB within the Nyquist band.
%
% (c) JJWikner, MERC, Ericsson Components AB. April 28, 1999.
% Marbella, Spain. Plaza de Naranjos.
% 
% Changes to the code at Linköping university, 990701, JJW
%
% Related
%
% estSFDR, estSNDR, estMTPR, estSNDRspect

if (nargin == 1)
  os_ratio = 1;
end;

if (nargin == 3)
  SNDR = 10*log10(estSNDR(input_signal, os_ratio, exp_ratio));
else
  SNDR = 10*log10(estSNDR(input_signal, os_ratio));
end;
  
ENOB = (SNDR-1.76)/6.02;
