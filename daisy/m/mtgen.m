function signal = mtgen(varargin)

% MTGEN.M
%
% signal = mtgen(varargin)
%
% Multi-tone signal IFFT generator.
% 
% Input arguments are stated as keyword, value, keyword, value, ...
% Keywords are:
%
% 'samplefrequency','fs','fsample' - default 2.208e6
% 'bins', 'tones'                  - default 4.3125e3*[32 64 128]
% 'NOS','numberofsamples','length' - default 1024
% 'amplitudes', 'amps','a'         - default sqrt(2)*ones(1,3);
% 'phases','phi'                   - default [0 pi/4 pi/2]
% 'offset','dc','os'               - default 0
%
% If multiple tones are specified, but the amplitudes and/or 
% phases is/are scalar, these values will be used for all tones.
%
% The phase is specified in radian.
%
% Notice that DC, Amplitude, and phases can be specified in 
% complex numbers forcing the output signal to be complex. 
% This would be useful for IQ signal generation.
%
% Created 22/11/1
%
% J Jacob Wikner (JJW), Ericsson Microelectronics
% jacob.j.wikner@mic.ericsson.se
%
%

% Revision history
%
% 17/12/1 by micolan
% The construction of the frequency domain vector was changed to
% [dclevel*NOS SIGNAL 0 fliplr(conj(SIGNAL))]
% in order to make the function function properly. Original code commented.
%

% Default values
Fs     = 2*1.104e6;
NOS    = 2048;
tones  = 4.3125e3*[32 64 128];
os     = 0;
phases = [0 pi/4 pi/2];
amps   = sqrt(2)*ones(1,3);


% Parse through the input arguments
nargs = nargin;
args  = varargin;
na    = 1;
while (na < nargs)
  
  switch lower(args{na})
  case {'samplefrequency','fs','fsample'}
    Fs   = args{na+1};
  case {'bins', 'tones'}
    tones = args{na+1};
  case {'nos','numberofsamples','length'}
    NOS  = args{na+1};
  case {'amplitudes', 'amps','a'}
    amps = args{na+1};
  case {'phases','phi'}          
    phases = args{na+1};
  case {'offset','dc','os'}      
    os = args{na+1};
  otherwise
    warning(['Hmm! Unrecognized keyword: ', ...
	    args{na}, ' in mtgen!']);
  end;
  na = na + 2;
end;

% FGG    = zeros(1, NOS); % original code
% FGG(1) = os* NOS; % original code
F    = zeros(1, round(NOS/2)-1); %revised code 17/12/1
bins   = round(NOS*tones/Fs) + 0;   % 1 here ??? - JJW
vals   = amps.*exp(j*phases).*ones(size(bins)) * (NOS/2);

F(bins + 1)       = vals; 
%FGG(NOS - bins +1) = conj(fliplr(FGG(bins + 1))); % original code
%FGG(NOS - bins +1) = conj(fliplr(vals)); % original code
FGG=[os*NOS F 0 fliplr(conj(F))]; %revised code 17/12/1

signal = ifft(FGG);

signal = real(signal); 		    % Safety margin (?)
% The latter comment could be removed if IQ signalling is wanted.
% This is for a latter version though.









