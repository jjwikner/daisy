
% For simulation of MTPR test.

NOB     = 14;
NOS     = 2^14;
Fsample = 10.001e6;
deltaF  = 156.25e3;
tones   = [1:32];


amps    = (2^NOB-1)/(32) * ones(size(tones));
phases  = 2*pi*rand(size(tones));

L = length(tones);
dmtsigset = zeros(L, NOS);
for m = 1:(L-1)
  mtones = tones([1:(m-1) (m+1):L]);
  mamps  = amps([1:(m-1) (m+1):L]);
  mphase = phases([1:(m-1) (m+1):L]);
  
  dmtsig = ...
      DMTsignal(mtones, mamps, mphase, ...
      deltaF, Fsample, NOS, NOB);
  
    pause;
    figure(1);
    plot(spect20(dmtsig,'blackman'));
    figure(2);
    plot(dmtsig);
  PAR(m)  = estPAR(dmtsig);
  MEAN(m) = mean(dmtsig);
  MTPR(m) = estMTPR(dmtsig, ...
      deltaF*[(m+3/4) (m+5/4)]/Fsample, ...
      deltaF*[(m-1/4) (m+1/4)]/Fsample);
  

  dmtsigset(m,:) = dmtsig;
end;

mtones = tones([1:(L-1)]);
mamps  = amps([1:(L-1)]);
mphase = phases([1:(L-1)]);

dmtsig  = ...
    DMTsignal(mtones, mamps, mphase, ...
    deltaF, Fsample, NOS, NOB);

%pause;
%figure(1);
%plot(spect20(dmtsig,'blackman'));
%figure(2);
%plot(dmtsig);
PAR(L) = estPAR(dmtsig);
MEAN(L) = mean(dmtsig);
dmtsigset(L,:) = dmtsig;

  
