function plotcellassignment(ca);

% Check number of input arguments
if nargin < 1
   error('Not enough input arguments');
end
if nargin > 1
   error('Too many input arguments');
end

figure;

% Schedule time
mv = ca{1};
scheduletime = mv(1, 1);
title(sprintf('Schedule time: %d', scheduletime));

% Plot all cell assignments
max_nr = size(ca, 2);
for ca_nr = 1:max_nr
   mv = ca{ca_nr};
   for var_nr = 2:size(mv, 1)
      % Extract variableinfo
      var = mv(var_nr, :);
      % Create text string
      op_str = getopstr(var(3));
      op_str(find(op_str == ' ')) = [];
      txt_message = [op_str, ' ', num2str(var(4)), ':', num2str(var(5))];
      % Plot
      plottiming(scheduletime, ca_nr, var(1), var(2), txt_message);
   end
end

% Line to mark the scheduletime
h = line([scheduletime scheduletime], [0 max_nr]);
set(h, 'Color', 'r', 'LineStyle', '-.');
axis([0 (scheduletime + 1) 0 max_nr]);
