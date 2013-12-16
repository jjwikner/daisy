function printstarttimesfast(schedule, filename);

%Check number of input arguments
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
spaces = '      ';
maxlength_timing = getmaxlength_timing(schedule);
maxidlength = ceil(log10(max(schedule(:, 2)) + 1));
fprintf(fid, 'Schedule time: %d', schedule(1, 1));
sch = schedule;
schedule = schedule(2:end, :);

for ix = 1:length(schedule(:,1))
    operand = schedule(ix, :);
    op_str = getopstr(operand);
    id_str = num2str(operand(2));
    op_str(find(op_str == ' ')) = [];
    [starttime(ix), upperbound(ix), lowerbound(ix)] = getstarttimesfast(sch, op_str, operand(2));
end

for op_nr = 1:length(schedule(:,1))
   
   fprintf(fid, '\n');
   % Fulhack, fixa en generell l?sning
   if (size(schedule, 1) > 9) & (op_nr < 10)
      fprintf(fid, ' ');
   end
   
   operand = schedule(op_nr, :);
   op_str = getopstr(operand);
   id_str = num2str(operand(2));
   fprintf(fid, '%d. %s id: %s%s', op_nr, op_str, id_str, spaces(1:maxidlength - length(id_str) + 3));
   fprintf(fid, 'Start: %d   Lower: %d   Upper: %d', starttime(op_nr), lowerbound(op_nr), upperbound(op_nr));
end

fprintf(fid, '\n');

% Close the file
if fid ~= 1
  fclose(fid);
end
