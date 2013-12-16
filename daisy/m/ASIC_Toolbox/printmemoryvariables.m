function printmemoryvariables(mv, filename);

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
scheduletime = mv(1, 1);
fprintf(fid, 'Schedule time: %d\n\n', scheduletime);
mv(1, :) = [];

% Column titles
fprintf(fid, 'Storage           | Source               | Consume\n');
fprintf(fid, 'Nr   Start   End  | Operand   Id   Port  | Time   Operand   Id   Port\n');
fprintf(fid, '------------------|----------------------|---------------------------\n');

% Sort
mv = sortrows(mv, 1);

% Print all variables
for mv_nr = 1:size(mv, 1)
   
   % Fulhack, fixa en generell lösning
   if mv_nr < 10
      fprintf(fid, ' ');
   end
   
   % Extract row and remove NaN:s
   mv_row = mv(mv_nr, :);
   mv_row(find(isnan(mv_row))) = [];
   % Get operand strings
   op_str   = getopstr(mv_row(3));
   op_str_c = getopstr(mv_row(7));
   % Print
   fprintf(fid, '%d%7.3g%7.3g  | %s%3.3g%6.3g   |%4.3g    %s%3.3g%6.3g\n', ...
      mv_nr, mv_row(1), mv_row(2), op_str, mv_row(4), mv_row(5), mv_row(6), op_str_c, mv_row(8), mv_row(9));
   mv_row(1:9) = [];
   % Print extra consumers
   while length(mv_row) > 3
      op_str_c = getopstr(mv_row(2));
      fprintf(fid, '                  |                      |%4.3g    %s%3.3g%6.3g\n', ...
         mv_row(1), op_str_c, mv_row(3), mv_row(4));
      mv_row(1:4) = [];
   end
end

% Close the file
if fid ~= 1
   fclose(fid);
end
