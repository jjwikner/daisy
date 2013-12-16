% general purpose second order switched cap SigDelta ADC
OSR = 500;  % fb = 1KHz,Fs = 1MHz
filter_order = 2;
NTF = synthesizeNTF(2,500,0,2);
form = 'CIFB';
[a,g,b,c] = realizeNTF(NTF,form);
b(2:end) = 0;
ABCD = stuffABCD(a,g,b,c,form);
[ABCDs umax] = scaleABCD(ABCD);
fprintf('Coefficients for the second order filter \n');
[a,g,b,c] = mapABCD(ABCDs,form)
figure;
f = linspace(0,0.5,1000);
z = exp(2i*pi*f);
plot(f,dbv(evalTF(NTF,z)));
sigma_H = dbv(rmsGain(NTF,0,0.5/OSR))