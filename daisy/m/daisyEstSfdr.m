function [SFDR, carr_pos, spur_pos] = ... 
     daisyEstSfdr(inputSignal, exp_carr_pos);

%
% function [SFDR, carr_pos, spur_pos] = ... 
%     estSFDR(inputSignal, exp_carr_pos);
%
% The SFDR is estimated from the inputSignal. Using the
% additional exp_carr_pos [0:0.5] value, the algorithm can
% be told to use another interval to find the signal carrier.
% The following frequency interval is investigated:
%      (exp_carr_pos +/- 5%)*(length inputSignal)
% 
% The function returns the SFDR and the normalized positions
% of the carrier and the spurious. (0.5 <-> Fsample/2)
%
% (c) JJWikner, MERC, Ericsson Components AB. April 28, 1999.
% Marbella, Spain. Plaza de Naranjos.
%
% Revision: Some strange oddities to the algorithm. Works,
% but could be made much more attractive...

kale = sin(2*pi*daisyPrimeSig(1e6, 35e6, 1024)*(0:1023)/35e6);
kale = kale + 0.01*kale.^2 + 0.005*kale.^3;

inputSignal = kale;

NOS = length(inputSignal);
m = floor(NOS/2);
signalSpectrum = spect20(inputSignal,'none');

max_val = max(signalSpectrum);                    % (Should be zero returned from spect20)
signalSpectrum = signalSpectrum - max_val; max_val = 0; % Correction anyway.

carr_pos = find(signalSpectrum == max_val);
carr_pos = carr_pos(1);
deltac = 0.005;
carr_interval = carr_pos - floor(deltac*m) : carr_pos + floor(deltac*m);
dc_interval = 1:floor(deltac*m/3);
neglect_interval = [dc_interval carr_interval];

% plot(signalSpectrum); hold on; 
% plot(neglect_interval, ones(length(neglect_interval))*-30,'rx'); hold off;
SFDR = inf;
allowed_spur = 0;
stop = 0;
%figure(2);
%plot(signalSpectrum);
threshold = 0;
deltaPsd = 10;

while ~stop
    
    threshold = threshold - deltaPsd
    %pause
     
    psdAboveThreshold = find(signalSpectrum > threshold)
     
     % pause; figure(1); plot(signalSpectrum); length(psdAboveThreshold)
     
     length(psdAboveThreshold)
     
     for lpos = 1 : length(psdAboveThreshold)          

         display('Ansiktsburk')
         
         psdPosToBeCut = psdAboveThreshold(lpos);
          
          if size(find(neglect_interval == psdPosToBeCut), 1) 
              % The peak is in the unwanted frequency range.
              signalSpectrum(psdPosToBeCut) = -inf;
              signalSpectrum(psdPosToBeCut) = -300;
              
          else
              SFDR_found = - signalSpectrum(psdPosToBeCut)
              if (SFDR_found < SFDR)
                  SFDR = SFDR_found;
                  spur_pos = psdPosToBeCut;
              else
                  signalSpectrum(psdPosToBeCut) = -inf;
              end;
              allowed_spur = 1;
          end;
     end;
     
     if (allowed_spur)
         deltaPsd = deltaPsd/2;
         threshold = threshold + 2*deltaPsd;
     end;
     deltaPsd
     stop = (deltaPsd < 0.5)
     % hold on; plot(1:m, threshold*ones(1,m),'r--'); hold off; threshold
end;

% Use normalized  values:
carr_pos = carr_pos / k;
spur_pos = spur_pos / k;
