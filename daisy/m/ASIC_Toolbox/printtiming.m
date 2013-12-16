function printtiming(operand, fid, maxlength);

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

col = length(operand);
data = operand(col - 4:col);
str = '';
for arg = 1:4
   switch arg
      case 1, str = 'Loops: ';
      case 2, str = 'Start: ';
      case 3, str = 'Lat: ';
      case 4, str = 'Exe: ';  
   end
   str = sprintf('%s%d', str, data(arg + (arg > 1)));
   if (arg == 1) & ~isnan(data(2))
      str = sprintf('%s, %d', str, data(arg + 1));
   end
   if ~length(maxlength) | (arg == 4)
      fprintf(fid, '%s\t', str);
   else
      fprintf(fid, '%s%s', str, spaces(1:maxlength(arg) - length(str) + 3));
   end
end

