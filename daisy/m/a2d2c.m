function digital = a2d2c(analog, NOB, ref, code)

% digital = a2d2c(analog, NOB, ref, code)
%
% The input, analog, is a vector containing the values to convert.
% The output, digital, is a matrix containing the bits corresponding 
% to each value of the input. 
%
% The input is quantized using the reference value, ref (default = 1), 
% and NOB, number of bits.
%
% If NOB is given by a two-column vector (e.g [NOB NOTB]) the NOTB 
% bits of the total NOB binary bits will be thermometer coded. 
%
% The digital code, code, can be
%    '2c'   : Two's complement
%    'ob'   : Offset binary
% Default is Offset binary. 
%
% (c) JJWikner, MERC, Ericsson Components AB., 1999
%
% Changes to the code at Linköping university, 001019, NUA 
%  - The segmented output option added  
% Changes to the code at Linköping university, 001221, JJW 
%  - Segmentation option modified
% NOT removed and spelling corrected, LiU, 010119, JJW
%
% Related
%
% d2a2
% a2d2
    

if (nargin==2)
  ref = 1;
  code = 'ob';
  rn = 0;
elseif (nargin==3)
  code = 'ob';
  rn = 0;
elseif (nargin==4)
  rn = 0;
end;
if (size(analog,1)>size(analog,2))
  analog = analog';
end;

if strcmp(code, 'ob') 	    % Offset binary code 
  if (length(NOB)>1) % Segmentation
    
    NOTB = NOB(2);
    NOB = NOB(1);
  else
    NOB = NOB;
    NOTB = 1;
  end;
  
  weights = ref*[ones(1,2^NOTB-1)*2^(NOB-NOTB) 2.^(NOB-((NOTB+1):NOB))];

  for l = 1:length(weights)
    a            = ((analog - weights(l)) >= 0);
    analog       = analog - a*weights(l);
    digital(:,l) = a';    
  end
  
  % cosmetic changes
  digital(:,1:(2^NOTB-1))=fliplr(digital(:,1:(2^NOTB-1)));
  
elseif strcmp(code,'2c')        % 2's complement - No thermometer ?
  digital(:,1) = (analog<0)';
  analog = analog+digital(:,1)'*2^(NOB(1)-1)*ref;
  for l=2:1:NOB(1)
    a = (analog-ref*2^(NOB(1)-l)>=0);
    analog=analog-a*2^(NOB(1)-l)*ref+rn*randn(1);
    digital(:,l)=a';
  end
end;

