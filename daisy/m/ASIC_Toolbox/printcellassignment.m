function printcellassignment(ca, filename);

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

% Schedule time
mv = ca{1};
scheduletime = mv(1, 1);
fprintf(fid, 'Schedule time: %d\n\n', scheduletime);

% Column titles
fprintf(fid, 'CELL | Storage           | Source               | Consume\n');
fprintf(fid, '     | Nr   Start   End  | Operand   Id   Port  | Time   Operand   Id   Port\n');
fprintf(fid, '-----|-------------------|----------------------|---------------------------\n');

% Print all cell assignments
for ca_nr = 1:size(ca, 2)
   % Print cell number
   fprintf(fid, '%3.3g  |', ca_nr - 1);
   % Print all variables in this cell
   mv = ca{ca_nr};
   for var_nr = 2:size(mv, 1)
      if var_nr > 2
         fprintf(fid, '     |');
      end
      % Extract variableinfo and remove NaN:s
      var = mv(var_nr, :);
      var(find(isnan(var))) = [];
      % Get operand strings
      op_str   = getopstr(var(3));
      op_str_c = getopstr(var(7));
      % Print
      fprintf(fid, '%3.3g%7.3g%7.3g  | %s%3.3g%6.3g   |%4.3g    %s%3.3g%6.3g\n', ...
         var_nr-1, var(1), var(2), op_str, var(4), var(5), var(6), op_str_c, var(8), var(9));
      var(1:9) = [];
      % Print extra consumers
      while length(var) > 3
         op_str_c = getopstr(var(2));
         fprintf(fid, '     |                   |                      |%4.3g    %s%3.3g%6.3g\n', ...
            var(1), op_str_c, var(3), var(4));
         var(1:4) = [];
      end
   end
   if ca_nr < size(ca, 2)
      fprintf(fid, '-----|-------------------|----------------------|---------------------------\n');
   end
end

% Close the file
if fid ~= 1
   fclose(fid);
end
