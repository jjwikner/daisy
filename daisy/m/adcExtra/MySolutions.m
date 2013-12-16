function output = MySolutions(varargin);
%
% ETI220 - Integrated A/D and D/A Converters
%
% Solutions for assignment 1
%
% Created by N/N 2008-11-??
% Last updated by N/N 2008-12-??
%
% Example: (to run exercise 1)
% >> Ass1_Solutions('x',1)
%

% Parameter default values

ex      = 1;             % Run first exercise by default
x       = [];            % Empty signal vector
fin     = 9.97e6;        % Signal frequency
fs      = 81.92e6;       % Sampling frequency
Nx      = 8192;          % FFT length
means   = 16;            % Number of FFTs to average
len     = Nx*means;      % Total signal length
win     = 'rect';        % Desired windowing function
R       = 10;            % Converter resolution
tjit    = 0e-12;         % Std deviation for gaussian jitter to sampling moment
A1      = 1;             % ADC input signal amplitude
Anfl    = 1e-10;         % Noise floor a bit above MATLAB rounding noise
Vref    = 1;             % Reference voltage (single ended; range is from -Vref to Vref)
delta   = 2*Vref/(2^R);  % A quantization step
Arndn   = delta/sqrt(12);% Sets the quantization noise level corresponding
                         % to R bit rectangular quantization noise	   
k2      = 0.000;         % Second order nonlinearity
k3      = 0.000;         % Third order nonlinearity
k4      = 0.000;         % Fourth order nonlinearity
k5      = 0.000;         % Fifth order nonlinearity


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


% Exercise 1
if ex==1
  % Set rectangular window
  win   = 'rect';
  % Generate and sample signal
  x     = sampling('signal','sine','fin',fin,'fs',fs,'ain',A1,'samples',len);
  % Make FFT
  spec  = adcfft('d',x.data,'skip',1,'mean','N',Nx,'w',win);  
  % Plot
  figure(1); clf;
  plot(0:length(spec)-1,20*log10(abs(spec)),'k-')
end

% Exercise 2
if ex==2
    % Put YOUR OWN ORIGINAL solution here!
end

% Exercise 3
if ex==3
    % Put YOUR OWN ORIGINAL solution here!
end

% Exercise 4
if ex==4
    % Put YOUR OWN ORIGINAL solution here!
end

% Exercise 5
if ex==5
    % Put YOUR OWN ORIGINAL solution here!
end

% Exercise 6
if ex==6
    % Put YOUR OWN ORIGINAL solution here!
end

% Exercise 7
if ex==7
    % Put YOUR OWN ORIGINAL solution here!
end

% Exercise 8
if ex==8
    % Put YOUR OWN ORIGINAL solution here!
end

% Exercise 9
if ex==9
    % Put YOUR OWN ORIGINAL solution here!
end

% Exercise 10
if ex==10
    % Put YOUR OWN ORIGINAL solution here!
end

% Exercise 11
if ex==11
    % Put YOUR OWN ORIGINAL solution here!
end

% Exercise 12
if ex==12
    % Put YOUR OWN ORIGINAL solution here!
end

% Exercise 13
if ex==13
    % Put YOUR OWN ORIGINAL solution here!
end

% Exercise 14
if ex==14
    % Put YOUR OWN ORIGINAL solution here!
end

% Exercise 15
if ex==15
    % Put YOUR OWN ORIGINAL solution here!
end

% Exercise 16
if ex==16
    % Put YOUR OWN ORIGINAL solution here!
end

% Exercise 17
if ex==17
    % Put YOUR OWN ORIGINAL solution here!
end
 
% Exercise 18
if ex==18
    % Put YOUR OWN ORIGINAL solution here!
end 
