function DMT_sig = ...
    DMTsignal(tones, amps, phases, deltaF, Fsample, NOS, NOB);

%
%  DMT_sig = DMTsignal(tones, amps, phases, deltaF, Fsample, NOS, NOB);
%
% tones   : Tones to be present in the signal
% amps    : Corresponding amplitude levels
% phases  : Corresponding phase shift
% deltaF  : Frequency spacing between each tone
% Fsample : Sample frequency
% NOS     : Number of samples in the output signal
% NOB     : Number of bits for quantization. 
%           1 is the reference level.
%
% The output signal has a zero offset unless the 0th tone is
% specified in tones. Be sure to specify the corresponding amps
% for the DC level as well as the phase that should be zero due
% to the internal summation of cosine waves. 
%
% By letting out the NOB variable, the output is not quantized.
%
% With no inputs, a full ADSL signal of 2^16 samples is given.
% All amplitude levels are equal but the phases are random. The
% resolution is 14 bits.
%
% (c) J Jacob Wikner, MERC, Ericsson Microelectronics AB
% Feb 17, 2000.
%
% Bug with 2's compl. clipping comparison corrected 000726. /JJW

% The tones describe the positions of the sinusoids, hence
% they will appear att tones*deltaF.



% It is important that the size of the vectors are all equal.

if (nargin == 0)
  % 14-bit full ADSL signal.
  NOB = 14;
  NOS = 2^16;
  Fsample = 2.208e6;
  deltaF  = 4.3125e3;
  tones = [ 10:127 129:255];
  amps  = (2^NOB-1)/(64*sqrt(2)) * ones(size(tones));
%  amps(1) = (2^NOB-1)/2/NOS;
  phases = 2*pi*rand(size(tones));
 % phases(1) = 0;
  truncate = 1;
end;  

if (nargin == 6)
  truncate = 0;
else 
  truncate = 1;
end;

if (size(tones) ~= size(amps)) or (size(tones) ~= size(phases))
  error('Incorrect vector lengths - tones, amps, phases');
else
  
  % Begin
  indeces = (0:(NOS-1));
  L = length(tones);
  DMT_sig = zeros(size(indeces));
  for l = 1:L
    DMT_sig = DMT_sig + amps(l) * ...
	cos(2*pi*deltaF*tones(l)*indeces/Fsample+phases(l));
  end;
  
end;

if (truncate)
  warning on;
  DMT_sig = ceil(DMT_sig);
  if (find(tones==0))
    % Offset binary
    if (DMT_sig(find(DMT_sig<0)) ...
	  | (DMT_sig(find(DMT_sig>2^NOB-1))))
      warning('The signal is clipping!');
    end;
    DMT_sig(find(DMT_sig<0))=zeros(size(find(DMT_sig<0)));
    DMT_sig(find(DMT_sig>(2^NOB-1)))= ...
	(2^NOB-1)*ones(size(find(DMT_sig>(2^NOB-1))));
  else
    % 2's complement
    if (DMT_sig(find(abs(DMT_sig)>2^(NOB-1))))
      
      warning('The signal is clipping!');
    end;
    DMT_sig(find(DMT_sig<(-2^(NOB-1)))) = ...
	zeros(size(find(DMT_sig<(-2^(NOB-1)))));
    DMT_sig(find(DMT_sig>2^(NOB-1))) = ...
	(2^(NOB-1))*ones(size(find(DMT_sig>2^(NOB-1))));
  end;

  warning backtrace;
end;

  
