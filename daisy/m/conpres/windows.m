clear w;
w(444,7) = 0;

w(1:444,1) = bartlett(444);
wlabels(1,1:8) = 'bartlett';
w(1:444,2) = blackman(444);
wlabels(2,1:8) = 'blackman';
w(1:444,3) = boxcar(444);
wlabels(3,1:8) = ' boxcar ';
w(1:444,4) = hamming(444);
wlabels(4,1:8) = 'hamming ';
w(1:444,5) = hanning(444);
wlabels(5,1:8) = 'hanning ';
w(1:444,6) = kaiser(444,0.1);
wlabels(6,1:8) = ' kaiser ';
w(1:444,7) = triang(444);
wlabels(7,1:8) = 'triangul';
wlabels(8,1:8) = '        ';

for i = 1:7 
  figure(i);
  clg;
  whitebg('w');
  K = 20*log10(abs(fft(yx.*w(1:444,i))));
  MaxK = max(K);
  K = K-MaxK;
  MaxK = max(K);
  plot(K,'k');
  axis([0 446 min(K)-5 5]);
  ylabel('20*log(FFT)');
  xlabel(wlabels(i,1:8));
end


