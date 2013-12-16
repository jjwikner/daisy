% Simple current-steering DAC simulation toolbox
% No time-domain information added. This is done 
% in simDAC - although for specific applications
%
% (c) jjwikner, MERC, Ericsson Microelectronics AB
% Oct. 2000

clear matvector binvector Mx;
clear SNDR SFDR ENOB;
clear SNDRdif SFDRdif ENOBdif;

openfigures(6);

NOB     = 6;
NOS     = 2^16; 

% This is ADSL specification:
Fsample = 2.209e6;
deltaF  = 4.3125e3;
Tones   = [7 11 19];
Tones   = [17];
Amps    = (2^(NOB-3)/2)*[1 1 1/8];
Amps    = (((2^NOB)-1)/2)*ones(size(Tones)); % FS 
Phases  = randn(size(Tones));
signal  = zeros(1,NOS);
indeces = (0:(NOS-1));
freqs   = indeces/NOS*Fsample;
hfreqs  = freqs(1:(NOS/2));


signal = DMTsignal([0 Tones], [(2^NOB-1)/2 Amps],  ...
    [0 Phases], deltaF, Fsample, NOS, NOB);

%signal = round(signal/2 + 0.5*rand(1,NOS));

% If you want an offset - add a tone a 0 (DC) with the proper
% DC level in Amps, i.e., [0 11 18 19], [2^(N-1) ...]

figure(1); clf; m = axes; 
plot(hfreqs/1e6, spect20(signal)); 
set(m,'FontSize',16); 
zoom on; grid on;
axis([0 Fsample/2e6 -100 5]);
title('Input Signal');
xlabel('Frequency [MHz]');
ylabel('Normalized (!) PSD');

figure(2); clf; m = axes; 
plot(indeces/Fsample*1e6, signal); 
set(m,'FontSize',16); 
zoom on; grid on;	
axis([0 NOS/Fsample*1e6 -5 2^NOB+5 ]);
title('Input Signal');
xlabel('Time [s]');
ylabel('Amplitude [LSB]');

% Specification of DAC parasitics.

Gunit   = 0.1e-18;
Cunit   = 12e-15;
Iunit   = 1.22e-6;
sigma   = 0.0000001; 			    % Somewhat unreasonable :)

Rload   = 50;
Gload   = 1/Rload;
Cload   = 50e-9;

rhoG = Gunit/Gload;
rhoC = Cunit/Cload;

sigpos = signal;
signeg = (2^NOB-1)-sigpos;

cols = 2^4;
rows = 2^NOB / cols;
unity_matrix = ones(cols,rows);

% This is to able the simulator to make the output impedance
% become dependent on the matching errors as well.

Gsrc = Gunit * unity_matrix;  
Csrc = Cunit * unity_matrix;
Msrc = sigma*randn(size(unity_matrix)); % Somewhat stupid approach. randn unreliable.
Psrc = 0; 			    % Parasitics to be added. See simDAC.

% Gradients to be added. Typically, this can be done to Msrc, e.g.
% Msrc = Msrc + xgrad*(1:...) + ygrad*(1:...); or other types of planes

% Assignment of current sources to different bits:

% "Binary" - stupid - assignment

bitass = zeros(cols,rows,NOB); 	    % :)

for l = 1:log2(rows);
  m = 2^(-l);
  bitass(:,:,NOB-l+1) = ...
      [zeros(cols,rows*(1-2*m)) ones(cols,rows*m) zeros(cols,rows*m)];
end;	

for l = (log2(rows)+1):NOB
  m = 2^(log2(rows)-l);
  bitass(:,:,NOB-l+1) = ...
      [zeros(cols,rows-1) ...
	  [zeros(cols*(1-2*m),1); ones(cols*m,1); zeros(cols*m,1)]];
end;

% Instead a smarter random or optimized version. See updated in simDAC.

for M = 1:NOB
  Gx(M) = sum(sum(bitass(:,:,NOB-M+1).*Gsrc));
  Cx(M) = sum(sum(bitass(:,:,NOB-M+1).*Csrc));
  Mx(M) = sum(sum(bitass(:,:,NOB-M+1).*Msrc));  
end;

binvector = 2.^(NOB-(1:NOB));
matvector = (binvector+Mx)';

digpos = a2d2(sigpos, NOB);
digneg = a2d2(signeg, NOB);

% M=NOB;
% rhoGs = logspace(-13,-5,M);
% rhoGs = 1e-3;

M = 1;
rhoGs = rhoG;
for m = 1:M
   rhoG = rhoGs(m);
   outpos = Iunit*digpos*matvector./ (1 + rhoG*sigpos');
   outneg = Iunit*digneg*matvector./ (1 + rhoG*signeg');
   outdif = outpos-outneg;
   SFDR(m) = estSFDR(outpos);
   SNDR(m) = estSNDR(outpos);
   SFDRdif(m) = estSFDR(outdif);
   SNDRdif(m) = estSNDR(outdif);   
end;	
   
figure(3); clf; m = axes; 
plot(hfreqs/1e6, spect20(outpos)); 
set(m,'FontSize',16, ...
    'Ytick', [-100 -36 -18 -12 -6]); 
zoom on; grid on;
axis([0 Fsample/2e6 -100 5]);
title('Positive Output');
xlabel('Frequency [MHz]');
ylabel('Normalized (!) PSD');

figure(4); clf; m = axes; 
plot(hfreqs/1e6, spect20(outdif)); 
set(m,'FontSize',16, ...
    'Ytick', [-100 -36 -18 -12 -6]); 
zoom on; grid on;
axis([0 Fsample/2e6 -100 5]);
title('Differential Output');
xlabel('Frequency [MHz]');
ylabel('Normalized (!) PSD');

figure(5); clf; m = axes; 
plot(hfreqs/1e6, spect20(outneg)); 
set(m,'FontSize',16, ...
    'Ytick', [-100 -36 -18 -12 -6]); 
zoom on; grid on;
axis([0 Fsample/2e6 -100 5]);
title('Negative Output');
xlabel('Frequency [MHz]');
ylabel('Normalized (!) PSD');

% simulated results

if (M == 1)
  figure(6); clf; m = axes;
  SFDR = num2str(0.1*round(SFDR*10));
  ENOB = num2str(0.1*round((10*log10(SNDR)-1.76)/6.02)*10); 
  SNDR = num2str(0.1*round(10*log10(SNDR)*10));
  l = [text(0,0.2,['SFDR: ', SFDR]); ...
      text(0,0.4,['SNDR: ', SNDR]); ...
      text(0,0.6,['ENOB: ', ENOB]);];
  set(l,'FontSize',16);
  l = text(0,0.8,['Simulated single-ended results']); ...
  set(l,'FontSize',16,'Fontangle','italic');

  SFDRdif = num2str(0.1*round(SFDRdif*10));
  ENOBdif = num2str(0.1*round((10*log10(SNDRdif)-1.76)/6.02)*10); 
  SNDRdif = num2str(0.1*round(10*log10(SNDRdif)*10));
  l = [text(0,1.2,['SFDR: ', SFDRdif]); ...
      text(0,1.4,['SNDR: ', SNDRdif]); ...
      text(0,1.6,['ENOB: ', ENOBdif]);];
  set(l,'FontSize',16);
  l = text(0,1.8,['Simulated differential results']); ...
  set(l,'FontSize',16,'FontAngle','italic');
  axis([0 1 0 2]); axis off;
end;
  
%%%
%% 
%% figure(1); clf; m = axes; 
%% semilogx(rhoGs, SFDR); 
%% set(m,'FontSize',14,'XTick',rhoGs(1:3:M)); 
%% zoom on; grid on;
%% axis([0.8*min(rhoGs) 1.25*max(rhoGs) 0 120]);
%% title('Simulated single-ended SFDR');
%% ylabel('SFDR [dBc]');
%% xlabel('Conductance ratio');
%% 
%% figure(2); clf; m = axes; 
%% semilogx(rhoGs, SNDR); 
%% set(m,'FontSize',14,'XTick',rhoGs(1:3:M)); 
%% zoom on; grid on;
%% axis([0.8*min(rhoGs) 1.25*max(rhoGs) 0 120]);
%% title('Simulated single-ended SNDR');
%% ylabel('SNDR [dBc]');
%% xlabel('Conductance ratio');
%% 
%% figure(3); clf; m = axes; 
%% semilogx(rhoGs, SFDRdif); 
%% set(m,'FontSize',14,'XTick',rhoGs(1:3:M)); 
%% zoom on; grid on;
%% axis([0.8*min(rhoGs) 1.25*max(rhoGs) 0 120]);
%% title('Simulated differential SFDR');
%% ylabel('SFDR [dBc]');
%% xlabel('Conductance ratio');
%% 
%% figure(4); clf; m = axes; 
%% semilogx(rhoGs, SNDRdif); 
%% set(m,'FontSize',14,'XTick',rhoGs(1:3:M)); 
%% zoom on; grid on;
%% axis([0.8*min(rhoGs) 1.25*max(rhoGs) 0 120]);
%% title('Simulated differential SNDR');
%% ylabel('SNDR [dBc]');
%% xlabel('Conductance ratio');

