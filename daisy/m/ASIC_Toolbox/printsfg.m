function printsfg(sfg, filename);

% Check number of input arguments
if nargin < 1
   error('Not enough input arguments');
end
if nargin > 2
   error('Too many input arguments');
end

% Set a file identifier
if nargin < 2
   fid = 1;
else
   if ~isstr(filename)
      error('The filename must be a string');
   end
   if exist(filename)
      warning(sprintf('Overwriting the file: %s', filename));
   end
   fid = fopen(filename, 'wt');
end

% Print all operands
maxlength = getmaxlength(sfg);
for operand = 1:size(sfg, 1)
   
   % Fulhack, fixa en generell lösning
   if (size(sfg, 1) > 9) & (operand < 10)
      fprintf(fid, ' ');
   end
   
   fprintf(fid, '%d. ', operand);
   printoperand(sfg(operand, :), fid, maxlength);
end

% Close the file
if fid ~= 1
   fclose(fid);
end
