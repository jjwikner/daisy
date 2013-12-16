function [SFDR, SNR, THD, SINAD, EFF_BITS] = distortion(logfft);

nop = max(size(logfft));

% Det antas att malogfftimala vardet ar signalenergin...

signal_pos = find(logfft==max(logfft(2:nop/2)));
signal_pos = signal_pos(1);

fuzz = 6;  % Maste andras med avseende pa antal sampel och frekvenskomponent och sa vidare...

if (fuzz > signal_pos)
  if (signal_pos == 1) 
    fuzz = 0;
  else
    fuzz = signal_pos-1;
  end;
end;

second_largest_pos = find(logfft==max([max(logfft(2:signal_pos-fuzz)) max(logfft(signal_pos+fuzz:nop/2))]));

SFDR = logfft(signal_pos)-logfft(second_largest_pos);
krupp = floor(nop/(2*signal_pos));

harmonics_pos(krupp-1) = 0;

for i = 2:krupp
  yxikyxi = find(logfft==max(logfft(i*signal_pos-fuzz:i*signal_pos+fuzz)));
  harmonics_pos(i-1) = yxikyxi(1);
end;

signal_energy = 0;

for k = signal_pos-fuzz:signal_pos+fuzz
  signal_energy = signal_energy + 10^(logfft(k)/10);
  logfft(k) = -inf;
end

harmonics_energy = 0;

for i = 1:max(size(harmonics_pos))
  hi = harmonics_pos(i);
  for k = hi-fuzz/2:hi+fuzz/2
     harmonics_energy = harmonics_energy + 10^(logfft(k)/10);
     logfft(k) = -inf;
  end
end;

THD = 10*log10(harmonics_energy / signal_energy);

noise_energy = 0;

%for i = 2:nop/2
%  noise_energy = noise_energy + (10^(logfft(i)/10));
%end;

SNR = 10*log10(signal_energy / noise_energy);

SINAD = 10*log10(signal_energy / (noise_energy+harmonics_energy));

EFF_BITS = (SINAD - 1.76) / 6.02;





