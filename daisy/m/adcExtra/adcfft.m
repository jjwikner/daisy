function output = adcfft(varargin);
%
%
% function output = adcfft(varargin);
%
% Calculate a correct adc fft from transient data according to the
% specified input arguments. 
%
% Takes a vector and calculates the frequency spectrum.
% Outputs a vector with the frequency bins from 0->Fs/2
%
% Input arguments:
% 'data'   value: Input data vector.
% 'osr'    value: Oversampling ratio. osr = fs/(2*fb)
% 'skip'   value: Integer value of initial samples to exclude from the fft.
% 'N'      value: No of bins to use in the fft.
% 'mean'          Do a mean value of many ffts.
% 'win'    value: Windowing function
% 'window' value: Windowing function
%                 'hamming','hann1','hann2','rect','blackman'
%
% EXAMPLE:
% >> x=(0:1024*128-1)/1024*2*pi*13;
% >> y = sin(x) + sin(x*3)*.0001+sin(x*6)*.0001+randn(1,1024*128)/sqrt(12)/(2^10);
% >> spec = manfft('d',y,'skip',10,'mean','N',4096/4,'w','rect');
%
% Revision history:
% 040801 - Created by Martin Anderson, ES-LTH.
% 040810 - Added varargin and comments - MAN, new name manfft.m
% 040810 - Added 'skip' - MAN
% 040810 - Added 'mean' - MAN
% 050403 - Added hann windowing /man
% 050628 - Changed name to adcfft.m /man
% 050628 - Added hann2 windowing /man
% 050702 - Added capability to handle oversampling systems /man
% 081107 - Moved window attenuation compensation to adcperf /man
% 081107 - Changed from boxcar to rectwin and reduced code length /man
% 081107 - Added more comments /man
%
% (c) 2004 - Martin Anderson, ES-LTH.
%     For educational and research purposes only. 
%     All other use is strictly prohibited.
%

skip=1;         % Number of initial samples to exclude
osr=1;          % No oversampling by default
N=1024;         % Number of bins to use in one fft
mean=0;         % Calculate mean value of many ffts
n=1;            % Default no of ffts to take mean value of
w = rectwin(N); % Use rectangular window as default
window=0;       % Use rectangular window as default

% Analyze input arguments
index = 1;
while index <= nargin    
    switch (lower(varargin{index}))
    case {'data' 'sig' 'indata' 'd'}
        data = varargin{index+1};
        index = index+2;
    case {'n'}
        N = varargin{index+1}; 
        index = index+2;
    case {'skip'}
        skip = varargin{index+1};
        index = index+2;    
    case {'osr'}
        osr = varargin{index+1};
        index = index+2;
    case {'mean'}
        mean = 1;
        index = index+1;
    case {'win' 'window' 'w'}
        if (strcmp(varargin{index+1},{'boxcar' 'rect' 'rectwin'})) 
            w = rectwin(N);
	    window=0;
        elseif (strcmp(varargin{index+1},'hann1')) 
            w = hann1(N);
            w = w';
	    window=1;        
        elseif (strcmp(varargin{index+1},'hann2')) 
            w = hann2(N);
            w = w';
	    window=2;
        elseif (strcmp(varargin{index+1},'blackman')) 
            w = blackman(N);
	    window=0;
        elseif (strcmp(varargin{index+1},'hamming')) 
            w = hamming(N);
	    window=0;
        else
            w =rectwin(N);  
	    window=0;
        end
        index = index+2;
    otherwise
        index=index+1;
    end
end

% Remove initial samples
data = data(skip:length(data));

% Calculate the number of FFTs to run
if mean
    n = floor(length(data)/N);          % Nr of averages
else
    n = 1;
end  

% Calculate power spectrum
data = data';
ss   = zeros(N/2,1); 
%
for i=1:n
  s  = fft(data((1:N)+N*(i-1) ).*w )/N; % FFT with window
  ss = ss+2*abs(s(1:N/2)).^2;           % Accumulate power. Times 2 because single-sided output.
end

% Return averaged spectrum
output = sqrt(ss/n);                    % Divide power by number of averaged FFTs
