function Ana_Signal=d2a2(Dig_Signal, NOB, reflev, codetype)


% function Dig_Signal=a2d(Signal,NOB,reflev,codetype)
%
% The input, Dig_Signal, is a matrix containing the bits
% corresponding to the output vector, Ana_Signal. 
%
% The output is generated using the reference level, 
% reflev (default = 1), and NOB, number of bits.
% The digital code, codetype, can be
%    '2c'   : Two's complement
%    'ob'   : Offset binary
% Default is Offset binary. 
%
% Related
%
% a2d2


if (nargin == 2)
  reflev = 1;
  codetype = 'ob';
elseif (nargin == 3)
  codetype = 'ob';
end;
  
if strcmp(codetype,'ob') 		% Offset binary code
  Ana_Signal = reflev * ...
      Dig_Signal * ...
      (2.^(NOB-(1:NOB)))';
elseif strcmp(codetype,'2c') 		% 2's complement
  Ana_Signal = reflev * ...
      Dig_Signal(:,2:NOB) * ...
      (2.^(NOB-1-(1:(NOB-1))))';
  Ana_Signal = Ana_Signal - Dig_Signal(:,1)*2^(NOB-1)*reflev;
end;
  

