function [PAR, peak, rms] = estPAR(input_signal);
 

%
% [PAR, peak, avg] = estPAR(input_signal);
%
% The PAR is the peak-to-average ratio, and is defined as
% the power ratio between the peak AC amplitude and the rms
% amplitude.
%
% The input_signal is in the time domain. The PAR, peak, 
% and rms values are returned.
%
% (c) JJWikner, MERC, Ericsson Microelectronics AB. Feb 17, 2000.
% Linkoping, Sweden, I ett morkt rum.
%
% Comments: The definition of PAR may vary.
%
% Revisions:

input_signal = input_signal-mean(input_signal);
peak = max(abs(input_signal))^2;
rms = std(input_signal);
PAR = peak/(rms^2);
