function [resStruct] = daisyAnalyzeFFT(inputSignal)

% function [resStruct] = daisyAnalyzeFFT(inputSignal);
% 
% 
% The function extracts dynamic performance measures from the 
% FFT of the 'inputSignal' given to the function. The output 
% is a struct with the following contents. 
% If you which to add functionality, extend the struct and document 
% your changes carefully.   
% 
% In this function distortion terms are any distortion term found in the
% spectrum while harmonics always are distortion terms found at
% multiples of the signal frequency, folding is taken into account.     
%      fHarmonic(k) = fSignal*k, k=[1,2,3,...]
%
% Therefore the distortion 
%
%
%
% resStruct.spect20 : The power spectrum of the input signal reduced to
%                     half Nyquist band and normalized to 0 dB.
%                    
% resStruct.sfdr    : The spurious free dynamic range [dB], ratio of
%                     fundamental power and largest distortion term 
%                     found in the spectrum.   
%  
% resStruct.snr     : Signal to noise ratio [dB], here the 20 largest 
%                     distortion terms has been removed.
%
% resStruct.sndr    : Signal to noise-and-disortion ratio [dB], ratio of
%                     fundamental power and the sum of noise+distortion.
%    
% resStruct.enob    : Effective number of bits, defined as:
%                     ENOB=(SNDR-1.76)/6.02.
%
% resStruct.distPower: Vector containing the power of the 20 largest
%                      distortion terms in falling order. Fundamental 
%                      tone power is found in resStruct.distPower(1).
%
% resStruct.distIndex: Vector containing the corresponding indexes of 
%                      resStruct.distPower. The indexes relate to the  
%                      the spectrum found in resStruct.spect20.     
%
% resStruct.harmPower: Vector containing the power of the 20 first harmonics.
%                      Fundamental tone power is found in resStruct.harmPower(1).
%        
% resStruct.harmIndex: Vector containing the corresponding indexes of 
%                      resStruct.harmPower. The indexes relate to the  
%                      the spectrum found in resStruct.spect20.     
%    
% resStruct.hd       : Harminic Distortion, vector containing the ratio
%                      of the k-th distortion and signal power [dB].
% 
% resStruct.thd      : Total Harmonic Distortion, TBD, ratio of the sum
%                      of the 20 largest distortion terms and the signal 
%                      power [dB].
%  
%    
% (c) Niklas U. Andersson, Electronis Systems Lab, 
%     Dept. Electrical Engineering, Linköping University. Jan 4th, 2011. 
%
% Revision History:
% 2011-01-04 (NUA): First Draft
% 2011-04-07 (NUA): Bug fix, don't look for harmonics
%                   on vector indexes < 1 and > max 
% 2012-04-14 (NUA): Uses full Nyquist band if complex input signal, 
%                   works for SFDR but how does it affect SNR, THD etc?
%
%
%

%%% CREATE THE POWER SPECTRUM %%%

% First remove the DC component of the input signal
inputSignal = inputSignal - mean(inputSignal);

%inputSignal = inputSignal - sum(inputSignal/length(inputSignal));


% Derive the power spectrum, use only half Nyquist band 
% (full Nyquist if complex input signal)
% Normalize signal power to 0 dB
% Future changes will be to handle oversampling as well
origSpect = 20*log10(abs(fft(inputSignal)));
if isreal(inputSignal)
    origSpect = origSpect(1:floor(end/2));
end
origSpect = origSpect-max(origSpect);

% Assign the power spectrum to the output struct
resStruct.spect20 = origSpect;

% Plot original power spectrum
%figure(1); plot(origSpect(2:end)-max(origSpect)); zoom on;
%figure(2); plot(testSignal); zoom on;


%%% FIND THE 20 LARGEST DISTORTION TERMS %%%

% Always work on a copy of origSpect
workSpect = origSpect;

distPower = zeros(1,20);
distIndex = zeros(1,20);

for i = 1:20

    % Find the largest distortion term
    [psdMaxValue, indMax] = max(workSpect);  
    
    distPower(i) = psdMaxValue; 
    distIndex(i) = indMax;
 
    % Walk on both slopes (up and down in freq) 
    % of the max bin (signal bin) until the derivative 
    % on one of them becomes positive, then remove that 
    % range from the spectrum, the next max gives the SFDR
    
    slopeUp = -1;
    slopeDown = -1;
    indCnt = 0;
    
    while (slopeUp <= 0 ) && (slopeDown <= 0)
        
        indCnt = indCnt + 1;
        
        % If the distortion is close to 0 or max of workSpect, there are some
        % problems...
        if indMax+indCnt <= length(workSpect)
            slopeUp   = workSpect(indMax+indCnt)-workSpect(indMax+indCnt-1);
        end
        if indMax-indCnt >= 1
            slopeDown = workSpect(indMax-indCnt)-workSpect(indMax-indCnt+1);
        end
                
    end
    
    % Remove distortion term from spectrum
    % How many points should be removed?
    % This might be a bit agressive...
    % Again terms close to end-poits are problematic
    % hence to following if statements.

    if indMax-indCnt >= 1
        removeStart = indMax-indCnt;
    else
        removeStart = 1;
    end
    if indMax+indCnt <= length(workSpect)
        removeEnd = indMax+indCnt;
    else
        removeEnd = length(workSpect);
    end
         
    workSpect(removeStart:removeEnd) = -inf; 

    % Save range for fundamental for future use (e.g SNDR calc)
    if i == 1
        fundStart = removeStart;
        fundEnd   = removeEnd;
    end
       
    %figure(2);
    %plot(workSpect); pause;
end


% Assign the distortion powers and indexes to output struct
resStruct.distPower = distPower;
resStruct.distIndex = distIndex;

% Create noise spectrum for SNR calculation
% in workSpect all harmonics are now set to -inf 
noiseSpect = workSpect;
%infIndex = find(noiseSpect == -inf);
%noiseSpect(infIndex) = [];
noiseSpect(noiseSpect == -inf) = [];


%%% FIND THE FIRST 20 HARMONICS %%%

% New copy of spectrum
workSpect = origSpect;

% Find first (signal) harmonic 
[psdMaxValue, indMax] = max(workSpect);


% Delta range might be to small...
deltaRange = 5;
fsamp = length(workSpect)*2;
fsig = indMax; 

% The harmonics are found at the following frequencies/indexes
harmIndex = fsamp/2 - abs(fsamp/2-mod([1:19].*fsig,fsamp));
harmPower = zeros(1,length(harmIndex));

for i = 1:length(harmIndex)
    
    d_from = harmIndex(i)-deltaRange;
    if d_from < 1
         d_from = 1;
    end
    
    d_to   = harmIndex(i)+deltaRange;
    if d_to > length(workSpect)
        d_to = length(workSpect);
    end
    
    harmPower(i) = max(workSpect(d_from:d_to));
    
end

% Assign the harmonics power and index to output struct
resStruct.harmPower = harmPower;
resStruct.harmIndex = harmIndex;


%%% DERIVE THE SNDR %%%

% Convert logarithmic spectrum to a linear power spectrum 
linearSpect = 10.^(origSpect/10); 
linSigPower = sum(linearSpect(fundStart:fundEnd));
linNoiseAndDistPower = sum(linearSpect(1:fundStart-1))+sum(linearSpect(fundEnd+1:end));

SNDR = 10*log10(linSigPower/linNoiseAndDistPower);
resStruct.sndr = SNDR; 

%%% DERIVE SNR %%%
linNoiseSpect = 10.^(noiseSpect/10); 
linNoisePower = sum(linNoiseSpect);

SNR = 10*log10(linSigPower/linNoisePower);
resStruct.snr = SNR; 

%%% DERIVE THE ENOB %%%
ENOB = (SNDR-1.76)/6.02;
resStruct.enob = ENOB;

%%% DERIVE THE SFDR %%%
SFDR = distPower(1)-distPower(2);
resStruct.sfdr = SFDR;

%%% DERIVE THE HD vector %%%
HD = distPower(2:end)-distPower(1);
resStruct.hd = HD;

%%% DERIVE THE THD vector %%%
linDistPower = 10.^(distPower/10);
THD = 10*log10(sum(linDistPower(2:end))/linDistPower(1));
resStruct.thd = THD;







