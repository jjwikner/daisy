function printoperand(operand, fid, maxlength);

% Check number of input arguments
if nargin < 1
   error('Not enough input arguments');
end
if nargin > 3
   error('Too many input arguments');
end
if nargin < 2
   fid = 1;
end
if nargin < 3
   maxlength = [];
else
   spaces = num2str(zeros(1, max(maxlength)));
   spaces(find(spaces == ' ')) = [];
   spaces(find(spaces == '0')) = ' ';
end

% Load operand mapping
operandmapping;

% Print the operand
d = 0; g = 1; s = 2;
adaptor = 0;
argformat = getargformat(operand(1));
[op_str, type_str] = getopstr(operand);

numinout = getnumofinout(operand(1));

% PRINT OPERAND
fprintf(fid, '%s', op_str);
% PRINT ARGUMENTS
str = '';
index = 1;
argformat = [argformat, 9];
for argid = 1:length(argformat)
   tab = 1;
   if argid == 1
      new_str = 'id: ';
   elseif numinout(1) & (argid == 2)
      new_str = 'in: ';
   elseif numinout(2) & (argid == 2 + numinout(1))
      new_str = 'out: ';
   elseif (argformat(argid) == d) & (argid == 2 + sum(numinout))
      new_str = 'bits: ';
   elseif (argformat(argid) == g) & (argid == 2 + sum(numinout))
      new_str = 'coeff: ';
   elseif argformat(argid) == s
      new_str = 'type: ';
   else
      if argid ~= length(argformat)
         str = [str, ', '];
         tab = 0;
      else
         tab = 1;
      end
   end
   if tab
      if length(str)
         if ~length(maxlength) | (index > 4)
            str = sprintf('%s\t', str);
         else
            str = sprintf('%s%s', str, spaces(1:maxlength(index) - length(str) + 3));
            index = index + 1;
         end
         fprintf(fid, '%s', str);
      end
      str = new_str;
   end
   if argformat(argid) == d
      str = sprintf('%s%d', str, operand(argid + 1));
   elseif argformat(argid) == g
      str = sprintf('%s%0.3g', str, operand(argid + 1));
   elseif argformat(argid) == s
      str = sprintf('%s%s', str, type_str);
   end
end
fprintf(fid, '\n');

