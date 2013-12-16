function [MTPR, carr_pwr, spur_pwr] = ... 
    estSFDR(input_signal, refBand, distBand);

% [MTPR, carr_pwr, spur_pwr] = ...
%      estSFDR(input_signal, refBand, distBand);
% 
% The multi-tone power ratio (MTPR) of the input_signal 
% is calculated. The reference signal power is expected to 
% be found in the frequency band specified by refBand. 
% The distortion term is to be found in a left-out frequency 
% position given by the frequency band distBand.
%
% The values returned are given in dB.
%
% The frequency bands are normalized and hence 0:0.5 denotes
% the entire frequency band.
%
% (c) JJWikner, MERC, Ericsson Microelectronics AB. Feb. 17, 2000.
% Linkoping, Sweden.
%

spectrum = 20*log10(abs(fft(input_signal)));
L = length(spectrum);

refBand = round(L*refBand)
distBand = round(L*distBand)

carr_pwr = max(spectrum(refBand))
spur_pwr = max(spectrum(distBand))

MTPR = carr_pwr - spur_pwr;


