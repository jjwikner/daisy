function plotmemoryvariables(mv);

% Check number of input arguments
if nargin < 1
   error('Not enough input arguments');
end
if nargin > 1
   error('Too many input arguments');
end

figure;

% Schedule time
scheduletime = mv(1, 1);
title(sprintf('Schedule time: %d', scheduletime));
mv(1, :) = [];

% Sort
mv = sortrows(mv, 1);

% Plot all variables
max_nr = size(mv, 1);
for mv_nr = 1:max_nr
   % Extract row
   mv_row = mv(mv_nr, :);
   % Create text string
   op_str = getopstr(mv_row(3));
   op_str(find(op_str == ' ')) = [];
   txt_message = [op_str, ' ', num2str(mv_row(4)), ':', num2str(mv_row(5))];
   % Plot
   plottiming(scheduletime, mv_nr, mv_row(1), mv_row(2), txt_message);
end

% Line to mark the scheduletime
h = line([scheduletime scheduletime], [0 max_nr]);
set(h, 'Color', 'r', 'LineStyle', '-.');
axis([0 (scheduletime + 1) 0 max_nr]);
