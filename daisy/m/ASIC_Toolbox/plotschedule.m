function plotschedule(schedule);

% Check number of input arguments
if nargin < 1
   error('Not enough input arguments');
end
if nargin > 1
   error('Too many input arguments');
end

% Text message
disp('Latency in cyan');
disp('Execution time in yellow');

figure;

% Schedule time
scheduletime = schedule(1, 1);
title(sprintf('Schedule time: %d', scheduletime));
schedule(1, :) = [];

% Remove delay, in and out
operandmapping;
schedule(find(schedule(:, 1) == op_in), :) = [];
schedule(find(schedule(:, 1) == op_out), :) = [];
schedule(find(schedule(:, 1) == op_delay), :) = [];

% Sort
schedule = sortrows(schedule, length(schedule(1, :)) - 2);

% Plot
[max_nr, col] = size(schedule);
for op_nr = 1:max_nr
   operand = schedule(op_nr, :);
   timedata = operand(col - 4:col);
   op_str = getopstr(operand);
   op_str(find(op_str == ' ')) = [];
   txt_message = [op_str, ', ', num2str(operand(2))];
   plottiming(scheduletime, op_nr, timedata(3), mod(timedata(3) + timedata(4), scheduletime), txt_message, 'c', 'lower');
   plottiming(scheduletime, op_nr, timedata(3), mod(timedata(3) + timedata(5), scheduletime), txt_message, 'y', 'upper');
end

% Line to mark the scheduletime
h = line([scheduletime scheduletime], [0 max_nr]);
set(h, 'Color', 'r', 'LineStyle', '-.');
axis([0 (scheduletime + 1) 0 max_nr]);
