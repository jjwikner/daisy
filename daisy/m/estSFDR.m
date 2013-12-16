function [SFDR, carr_pos, spur_pos] = ... 
     estSFDR(input_signal, exp_carr_pos);

%
% function [SFDR, carr_pos, spur_pos] = ... 
%     estSFDR(input_signal, exp_carr_pos);
%
% The SFDR is estimated from the input_signal. Using the
% additional exp_carr_pos [0:0.5] value, the algorithm can
% be told to use another interval to find the signal carrier.
% The following frequency interval is investigated:
%      (exp_carr_pos +/- 5%)*(length input_signal)
% 
% The function returns the SFDR and the normalized positions
% of the carrier and the spurious. (0.5 <-> Fsample/2)
%
% (c) JJWikner, MERC, Ericsson Components AB. April 28, 1999.
% Marbella, Spain. Plaza de Naranjos.
%
% Revision: Some strange oddities to the algorithm. Works,
% but could be made much more attractive...


k = length(input_signal);
m = floor(k/2);
spectrum = spect20(input_signal);

max_val = max(spectrum);                    % (Should be zero returned from spect20)
spectrum = spectrum - max_val; max_val = 0; % Correction anyway.

carr_pos = find(spectrum == max_val);
carr_pos = carr_pos(1);
deltac = 0.02;
carr_interval = carr_pos - floor(deltac*m) : carr_pos + floor(deltac*m);
dc_interval = 1:floor(deltac*m/3);
neglect_interval = [dc_interval carr_interval];
% plot(spectrum); hold on; plot(neglect_interval, ones(length(neglect_interval))*-30,'rx'); hold off;
SFDR=inf;
allowed_spur = 0;
stop = 0;
%figure(2);
%plot(spectrum);
threshold = 0;
deltat=10;
while ~stop
     threshold = threshold - deltat;
     larger_pos = find(spectrum > threshold);
     % pause; figure(1); plot(spectrum); length(larger_pos)
     for lpos = 1:length(larger_pos)
          
          lp_pos = larger_pos(lpos);
          if size(find(neglect_interval == lp_pos),1) % The peak is in the unwanted frequency range.
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
carr_pos = carr_pos / k;
spur_pos = spur_pos / k;
