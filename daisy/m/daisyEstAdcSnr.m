function [] = daisyEstAdcSnr(filepath, wkT)
    
% 'filepath' contains path to file to read data from     
% This data is assumed to be a matrix from the ADC, i.e., 
% digital data. The level is assumed to be between 0 and 1.
%
% wkT = 2*pi*fk/fs is the normalized input sinusoid frequency
%
% If you have used coherent sampling, this becomes very easy...
%
    
    
    yTot = dlmread(filepath,' ');
    
    timeInfo = yTot(:,1);
    data = yTot(:, 2:end);

    % Binaryfy them...
    data(find(data > 0.5)) = 1;
    data(find(data < 0.5)) = 0;
    
    NOB  = size(data,2);
    
    % MSB is found first in the matrix

    value = data * (2.^( ( NOB-1):-1:0) )';
    
    yTot = value';
    
    plot(yTot)
    % Disregards the first and last 10 percent of the signal
    n = ceil(length(yTot)*0.1):ceil(length(yTot))*0.9; 
    
    % Estimated magnitudes and phases of the sinusoid
    [mag, phase, offset] = daisyExtractSine(yTot(n), n, wkT);

    
    yHat = 0;
    
    for k = 1:length(mag)
        yHat = yHat + mag(k) * sin(wkT(k)*n+phase(k));  
    end
    
    % Add the offset;
    
    yHat = yHat+offset;
    
    % Find the power ratio in dB.
    
    SNR = 10*log10(sum((yHat-offset).^2)/sum((yTot(n)-yHat).^2));
    
    disp(['Estimated SNR: ' num2str(SNR) ' dB'])


