function printinterconnection(mv, pea, filename);

% Check number of input arguments
if nargin < 2
   error('Not enough input arguments');
end
if nargin > 3
   error('Too many input arguments');
end

% Set a file identifier
if nargin < 3
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

% Remove the schedule time
mv(1, :) = [];

% Column titles
fprintf(fid, '\nPE | Operand      | Port (In)   Read | Port (Out)   Write');
fprintf(fid, '\n---|--------------|------------------|-------------------');

% Print all PE assignments
for pea_nr = 1:size(pea, 2)
   pea_vec = pea{pea_nr};
   op_str = getopstr(pea_vec(1));
   fprintf(fid, '\n%2.2g | %s    |', pea_nr, op_str);
   % Extract the communication between the memory and this PE
   numinout = getnumofinout(pea_vec(1));
   communication_read  = zeros(numinout(1), 1);
   communication_write = zeros(numinout(2), 1);
   for mv_nr = 1:size(mv, 1)
      % Extract row and remove NaN:s
      mv_row = mv(mv_nr, :);
      mv_row(find(isnan(mv_row))) = [];
      % Write
      if mv_row(3) == pea_vec(1)
         if length(find(mv_row(4) == pea_vec(2:end)))
            communication_write(mv_row(5)) = communication_write(mv_row(5)) + 1;
         end
      end
      % Read
      mv_row(1:5) = [];
      while length(mv_row) > 3
         if mv_row(2) == pea_vec(1)
            if length(find(mv_row(3) == pea_vec(2:end)))
               communication_read(mv_row(4)) = communication_read(mv_row(4)) + 1;
            end
         end
         mv_row(1:4) = [];
      end
   end
   % Print the communication between the memory and this PE
   for port_nr = 1:max(numinout)
      if port_nr > 1
         fprintf(fid, '\n   |              |');
      end
      if port_nr > numinout(1)
         fprintf(fid, '                  |');
      else
         fprintf(fid, '%4.3g%12.3g  |', port_nr, communication_read(port_nr));
      end
      if port_nr <= numinout(2)
         fprintf(fid, '%4.3g%13.3g', port_nr, communication_write(port_nr));
      end
   end
   if pea_nr < size(pea, 2)
      fprintf(fid, '\n---|--------------|------------------|-------------------');
   end
end

fprintf(fid, '\n');

% Close the file
if fid ~= 1
   fclose(fid);
end
