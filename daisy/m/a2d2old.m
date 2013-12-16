function Dig_Signal=a2d2(Ana_Signal, NOB, reflev, codetype, rn)

% function Dig_Signal=a2d2(Ana_Signal,NOB,reflev,codetype,rn)
%
% The input, Ana_Signal, is a vector containing the values to 
% convert. The output, Dig_Signal, is a matrix containing 
% the bits corresponding to each value of the input. 
%
% The input is quantized using the reference value, 
% reflev (default = 1), and NOB, number of bits. 
% The digital code, codetype, can be
%    '2c'   : Two's complement
%    'ob'   : Offset binary
% Default is Offset binary. 
%
% As an extra random error in the Ana_Signal, rn can be used.
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

if strcmp(codetype, 'ob') % Offset binary code 
  for l=1:1:NOB
    a = (Ana_Signal-reflev*2^(NOB-l)>=0);
    Ana_Signal=Ana_Signal-a*2^(NOB-l)*reflev+rn*randn(1);
    Dig_Signal(:,l)=a';
  end
elseif strcmp(codetype,'2c') % 2's complement
  if (size(Ana_Signal,1)>size(Ana_Signal,2))
    Ana_Signal = Ana_Signal';
  end;
  Dig_Signal(:,1) = (Ana_Signal<0)';
  Ana_Signal = Ana_Signal+Dig_Signal(:,1)'*2^(NOB-1)*reflev;
  for l=2:1:NOB
    a = (Ana_Signal-reflev*2^(NOB-l)>=0);
    Ana_Signal=Ana_Signal-a*2^(NOB-l)*reflev+rn*randn(1);
    Dig_Signal(:,l)=a';
  end
end;  
