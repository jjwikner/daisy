function outspect = daisySpect20(input_signal, ignoreDc, wind_type);
%
% outspect = daisySpect20(input_signal, ignoreDc, wind_type);
%
% Returns the spectrum of the input_signal in a
% 20dB scale. Wind_type is the window function.
% spect20(input_signal) uses a hanning window,
% others are 'none', 'hamming', ...
% 
% (c) JJWikner, MERC, Ericsson Components AB. April 26, 1999.
%
% On a balcony in Marbella, Spain.
%
% Changed at the Linköping university, 11 years later !

u = length(input_signal);
m = floor(u/2);

if (nargin == 1)
    wind_type = 'hanning';
    ignoreDc  = 1;
end;

if (nargin == 2)
    wind_type = 'hanning';
end;

if (size(input_signal,2) > size(input_signal,1))
     input_signal = input_signal';
end;

if ignoreDc
    input_signal = input_signal - mean(input_signal);
end;
    

if strcmp(wind_type,'hanning')
  input_signal = input_signal .* ...
      hanning(u);

elseif strcmp(wind_type,'blackman')
  input_signal = input_signal .* ...
      blackman(u);

elseif strcmp(wind_type,'hamming')
  input_signal = input_signal .* ...
      hamming(u);
end;
outspect = 20*log10(abs(fft(input_signal)));
outspect = outspect - max(outspect);
outspect = outspect(1:m);

