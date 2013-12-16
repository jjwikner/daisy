function output = ADCdemo(varargin);

% Lab1 MATLAB CODE

% Created by Martin Anderson, 050201
% Last updated by man, 081107

% Set parameter default values

clear all;
close all;

x = [];                  %Empty signal vector
fin = 9.97e6;            %Carrier frequency
fs = 81.92e6;            %Sampling frequency
Nx = 8192;             %FFT length
means = 16;              %Number of FFTs to average
len = Nx*means;          %Total signal length
win = 'boxcar';          %Desired windowing function
R = 10;                  %Converter resolution
tjit = 0e-12;            %Std deviation for gaussian jitter to sampling moment
A1 = 1;                  %ADC input signal amplitude
Anfl = 1e-10;            %Noise floor a bit above MATLAB rounding noise
Vref     = 1;            % Reference voltage (single ended; range is from -Vref to Vref)
delta   = 2*Vref/(2^R);  % A quantization step
Arndn = delta/sqrt(12);  %Sets the quantization noise level corresponding
                         %to R bit rectangular quantization noise	   
k2 = 0.00;              %Second order nonlinearity
k3 = 0.00;              %Third  
k4 = 0.00;              %Fourth 
k5 = 0.00;              %Fifth 
			 

ex=1;
% Analyse input arguments
index = 1;
while index <= nargin    
    switch (lower(varargin{index}))
    case {'exercise' 'ex' 'x' 'nr' 'ovn' 'number'}
        ex = varargin{index+1};
        index = index+2;
    otherwise
        index=index+1;
    end
end

% Exercise 17
if ex==1
  win='hann1';
  fs=250e6;
  fimax=fs/2-1;
  primtal = primes(floor(Nx/10.1));
  fin = max(primtal)/Nx*fs;
  R=12;
  delta = 2*Vref/(2^R);  
  Arndn = delta/sqrt(12);
  jit_gaus = 8e-12;
  Cs = 450e-15;
  Rs = 250;  
  sk2 = 00.004;
  sk3 = 00.007; 
  sk4 = 00.0025;
  sk5 = 00.004;
  A1=1;
  npow = (Arndn/4)^2;
  
  % --- 1:
  x = sampling('signal','sine','fin',fin,'fs',fs,'ain',A1*0.97,'samples', len,'k2',sk2,'k3',sk3,'k4',sk4,'k5',sk5,'Cs',Cs,'Rs',Rs,'jit_gaus',jit_gaus);
  y = quantization('data',x.data,'R',R,'vref',1,'npow', npow);
  spec = adcfft('d',y,'skip',1,'mean','N',Nx,'w',win);   
  perf = adcperf('data',spec,'snr','sndr','sfdr','sdr','w',win,'plot')
  figure;
  plot(0:length(spec)-1,20*log10(abs(spec)),'k-')

  
  % -- 2;
  ainv = A1.*[0.00001 0.0001 0.001 0.01:0.05:sqrt(2)];
  for k = 1:length(ainv)  
   x = sampling('signal','sine','fin',fin,'fs',fs,'ain',ainv(k),'samples', len,'k2',sk2,'k3',sk3,'k4',sk4,'k5',sk5,'Cs',Cs,'Rs',Rs,'jit_gaus',jit_gaus);
    y = quantization('data',x.data,'R',R,'vref',1,'npow', npow); 
    spec = adcfft('d',y,'skip',1,'mean','N',Nx,'w',win);    
    perf = adcperf('data',spec,'snr','sndr','sfdr','sdr','w',win);
    sndr(k) = perf.sndr;
    sfdr(k) = perf.sfdr;
    snr(k) = perf.snr;
  end
  figure; 
  hold on;
  semilogx(20*log10(ainv./A1./sqrt(2)),sndr,'k-','LineWidth',2);  
  semilogx(20*log10(ainv./A1./sqrt(2)),snr,'b-','LineWidth',2);
  semilogx(20*log10(ainv./A1./sqrt(2)),sfdr,'r--','LineWidth',2); 

  ylabel('dB');
  xlabel('Input amplitude, (dBFS)');
  axis([-80 0 0 100]);
    legend('SNDR','SNR','SFDR');
 
 % -- 3:
 sndr=0;
 sfdr=0;
 sdr=0;
 snr=0;
 
  primenums = primes(floor(Nx/2.1));
  finv =primenums(3:20:length(primenums))/Nx*fs;
  for k = 1:length(finv)  
    x = sampling('signal','sine','fin',finv(k),'fs',fs,'ain',A1*0.99,'samples', len,'k2',sk2,'k3',sk3,'k4',sk4,'k5',sk5,'Cs',Cs,'Rs',Rs,'jit_gaus',jit_gaus);
    y = quantization('data',x.data,'R',R,'vref',1,'npow', npow); 
    spec = adcfft('d',y,'skip',1,'mean','N',Nx,'w',win);    
    perf = adcperf('data',spec,'snr','sndr','sfdr','sdr','w',win);
    sndr(k) = perf.sndr;
    sfdr(k) = perf.sfdr;
    snr(k) = perf.snr;
  end
  figure; 
  hold on;
  semilogx(finv./1e6,sndr,'k-','LineWidth',2);  
  semilogx(finv./1e6,snr,'b-','LineWidth',2);
  semilogx(finv./1e6,sfdr,'r--','LineWidth',2); 
  legend('SNDR','SNR','SFDR');
    ylabel('dB');
  xlabel('Input frequency, (MHz)');
  
end 
