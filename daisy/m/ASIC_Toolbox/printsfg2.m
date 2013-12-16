function printsfg2(sfg, filename);

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
sfgsize = size(sfg);
for operand = 1:sfgsize(1)
    fprintf(fid, '%d. ', operand);
    printoperand2(sfg(operand, :), fid);
end

% Close the file
if fid ~= 1
   fclose(fid);
end
