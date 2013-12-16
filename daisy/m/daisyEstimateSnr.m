function [] = daisyEstimateSnr(filepath, WkT)
    
% 'filepath' contains path to file to read data from     
%
% WkT = 2*pi*fk/fs Contains normalized input sinusoid frequency
% If you have used coherent sampling, this becomes very easy...
    
    
    ytot = dlmread(filepath,' ');
    
    %WkT  Contains all input sinusoid angles (wT)
    %WkT = 2*pi*[0.1];
    
    ytot = ytot';
    
    % Disregards the first and last 10 percent of the signal
    n = ceil(length(ytot)*0.1):ceil(length(ytot))*0.9; 

    % Estimated magnitudes and phases of the sinusoid
    [mag,fas,offset] = daisyExtractSine(ytot(n),n,WkT);

    yhat = 0;
    for k = 1:length(mag)
        yhat = yhat+mag(k)*sin(WkT(k)*n+fas(k));  
    end
    yhat = yhat+offset;
    SNR = 10*log10(sum((yhat-offset).^2)/sum((ytot(n)-yhat).^2));
    
    disp(['Estimated SNR: ' num2str(SNR) ' dB'])


