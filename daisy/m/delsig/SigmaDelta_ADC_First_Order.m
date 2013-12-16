% Sigma Delta ADC
% First Order Modulator, 4 bit Flash ie 16 quantization levels
clc;
clear;
close all;
order = 1;
OSR = 64;
quant_levels = 17;
opt = 1;
% Synthesize NTF,Plot Frequency Response,Compute Max Attenuation in band of
% interest
NTF1 = synthesizeNTF(order,OSR,opt)
plotPZ(NTF1);
figure;
f = linspace(0,0.5,1000);
z = exp(2i*pi*f);
plot(f,dbv(evalTF(NTF1,z)));
sigma_H = dbv(rmsGain(NTF1,0,0.5/OSR))
% Realize NTF using given topology
form = 'CRFB';
fprintf('Unscaled loop filter coefficients follow ...\n');
[a,g,b,c] = realizeNTF(NTF1,form)
b(2:end) = 0;  % for a maximally flat STF, all coeffcients of b except b1 = 0
ABCD = stuffABCD(a,g,b,c,form);
% Dynamic scaling of loop filter matrix
xLim = 0.9;
f0 = 0;
[ABCDs umax] = scaleABCD(ABCD,quant_levels,f0,xLim);
% Translate the scaled matrix into coeffcients for the chosen filter
% structure
fprintf('Scaled loop filter coefficients follow ...\n');
[a g b c] = mapABCD(ABCDs,form)

% Find peak SNR for given spec
amp = [-130:5:-20 -17:2:-1];
snr = simulateSNR(NTF1,OSR,amp,[],quant_levels);
plot(amp,snr,'-b',amp,snr,'db');
[pk_snr pk_amp] = peakSNR(snr,amp);