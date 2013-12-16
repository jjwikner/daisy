% This file is used to synthesize an NTF based on the modulator order,OSR
% and whether optimization is required.
clc;
clear;
close all;
order = 5;
OSR = 64;
opt = 1;
NTF1 = synthesizeNTF(order,OSR,opt)
plotPZ(NTF1);
figure;
f = linspace(0,0.5,1000);
z = exp(2i*pi*f);
plot(f,dbv(evalTF(NTF1,z)));
sigma_H = dbv(rmsGain(NTF1,0,0.5/OSR))
figure;
quant_levels = 2;
Nfft = 2^13;  %8192
input_tone_bin = 57;
N_samples = [0: Nfft-1];
input = 0.5 * (quant_levels-1)* sin(2*pi*input_tone_bin/Nfft*N_samples);
output = simulateDSM(input,NTF1,quant_levels);
n = 1:150;
stairs(N_samples(n),input(n),'g');
hold on;
stairs(N_samples(n),output(n),'b');

% Performance evaluation at numerous input amplitudes
amp = [-130:5:-20 -17:2:-1];
snr = simulateSNR(NTF1,OSR,amp,[],quant_levels);
plot(amp,snr,'-b',amp,snr,'db');
[pk_snr pk_amp] = peakSNR(snr,amp);

% Realizing NTF for a selected structure
NTF2 = synthesizeNTF(5,64,1);
form = 'CRFB';
fprintf('Unscaled loop filter coefficients follow ...\n');
[a,g,b,c] = realizeNTF(NTF2,form)
b(2:end) = 0;  % for a maximally flat NTF, all coeffcients of b except b1 = 0
ABCD = stuffABCD(a,g,b,c,form);
[NTF2 STF2] = calculateTF(ABCD);
f = linspace(0,0.5,100);
z = exp(2i*pi*f);
magSTF2 = dbv(evalTF(STF2,z));
magNTF2 = dbv(evalTF(NTF2,z));
figure;
plot(f,magSTF2,'b',f,magNTF2,'m');
% Dynamic scaling of loop filter matrix
xLim = 0.9;
f0 = 0;
[ABCDs umax] = scaleABCD(ABCD,quant_levels,f0,xLim);
% Translate the scaled matrix into coeffcients for the chosen filter
% structure
fprintf('Scaled loop filter coefficients follow ...\n');
[a g b c] = mapABCD(ABCDs,form)




