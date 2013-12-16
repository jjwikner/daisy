function digSignal=a2d2(anaSignal, NOB, reflev, codetype, rn)

% function digSignal=a2d2(anaSignal,NOB,reflev,codetype,rn)
%
% The input, anaSignal, is a vector containing the values to 
% convert. The output, digSignal, is a matrix containing 
% the bits corresponding to each value of the input. 
%
% The input is quantized using the reference value, 
% reflev (default = 1), and NOB, number of bits.
%
% If NOB are given as a two column vector (e.g [NOB NOTB])
% the NOTB bits of the total NOB binary bits will be 
% thermometer coded. 
%
% The digital code, codetype, can be
%    '2c'   : Two's complement
%    'ob'   : Offset binary
% Default is Offset binary. 
%
% As an extra random error in the anaSignal, rn can be used.
%
% (c) JJWikner, MERC, Ericsson Components AB., 1999
%
% Changes to the code at Linköping university, 001019, NUA 
%  - The segmented output option added  
%
% Related
%
% d2a2

if (nargin==2)
  reflev = 1;
  codetype = 'ob';
  rn = 0;
elseif (nargin==3)
  codetype = 'ob';
  rn = 0;
elseif (nargin==4)
  rn = 0;
end;

if strcmp(codetype, 'ob') 	    % Offset binary code 
  for l=1:1:NOB(1)
    a = (anaSignal-reflev*2^(NOB(1)-l)>=0);
    anaSignal=anaSignal-a*2^(NOB(1)-l)*reflev+rn*randn(1);
    digSignal(:,l)=a';
  end
  
elseif strcmp(codetype,'2c')        % 2's complement
  if (size(anaSignal,1) > size(anaSignal,2))
    anaSignal = anaSignal';
  end;
  digSignal(:,1) = (anaSignal<0)';
  anaSignal = anaSignal+digSignal(:,1)'*2^(NOB(1)-1)*reflev;
  for l=2:1:NOB(1)
    a = (anaSignal-reflev*2^(NOB(1)-l)>=0);
    anaSignal = anaSignal-a*2^(NOB(1)-l)*reflev+rn*randn(1);
    digSignal(:,l)=a';
  end
end;


% Thermometer code the NOTB MSBs (not sure if it works with 2's compl.)

if length(NOB) == 2; 		    
  NOTB = NOB(2);
  for i = 1:length(anaSignal)
    therm_out(i,1:2^NOTB-1) = ...
	[ zeros(1,2^NOTB-1-digSignal(i,1:NOTB)*2.^(NOTB-1:-1:0)') ...
	    ones(1,digSignal(i,1:NOTB)*2.^(NOTB-1:-1:0)') ];
  end;
  tmp_digSignal = [ therm_out digSignal(:,NOTB+1:NOB(1)) ];
  digSignal = tmp_digSignal;
end;

