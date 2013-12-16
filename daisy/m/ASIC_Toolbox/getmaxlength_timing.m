function maxlength = getmaxlength_timing(schedule);

% loops, start, latency

[row, col] = size(schedule);
timing = schedule(:, col - 4:col);

maxlength = zeros(1, 3);
for op_number = 1:row
   data = timing(op_number, :);
   str = '';
   for arg = 1:3
      if arg == 1
         str = 'Loops: ';
      elseif arg == 2
         str = 'Start: ';
      else
         str = 'Lat: ';
      end
      str = sprintf('%s%d', str, data(arg + (arg > 1)));
      if (arg == 1) & ~isnan(data(2))
         str = sprintf('%s, %d', str, data(arg + 1));
      end
      if length(str) > maxlength(arg)
         maxlength(arg) = length(str);
      end
   end
end
