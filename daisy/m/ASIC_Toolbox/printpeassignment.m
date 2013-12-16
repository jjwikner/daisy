function printpeassignment(pea, filename);

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

% Column titles
fprintf(fid, 'PE | Operand      | Id-list\n');
fprintf(fid, '---|--------------|--------------------------------');

% Print all PE assignments
for pea_nr = 1:size(pea, 2)
   pea_vec = pea{pea_nr};
   op_str = getopstr(pea_vec(1));
   fprintf(fid, '\n%2.2g | %s    | ', pea_nr, op_str);
   % Print all operand id:s assigned to this PE
   for op_nr = 2:length(pea_vec)
      fprintf(fid, '%d', pea_vec(op_nr));
      if op_nr < length(pea_vec)
         fprintf(fid, ', ');
      end
   end
end

fprintf(fid, '\n');

% Close the file
if fid ~= 1
   fclose(fid);
end
