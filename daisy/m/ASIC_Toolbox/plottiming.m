function plottiming(scheduletime, nr, starttime, endtime, message, color, pos);
% pos = 'fill', 'upper', or 'lower'

% Check number of input arguments
if nargin < 4
   error('Not enough input arguments');
end
if nargin > 7
   error('Too many input arguments');
end

% Fix missing input arguments
if (nargin < 6) | ~length(color)
   color = 'c';
end
if (nargin < 7) | ~length(pos)
   pos = 'fill';
end

% Box position
switch lower(pos)
   case 'fill',  y = nr - 1;   height = 1;   e_color = 'k';
   case 'lower', y = nr - 1;   height = 0.5; e_color = color;
   case 'upper', y = nr - 0.5; height = 0.5; e_color = color;
end

% Box
if starttime == endtime
   h = line([starttime starttime], [y (y + height)]);
   set(h, 'Color', color);
elseif starttime < endtime
   h = rectangle('Position', [starttime, y, endtime - starttime, height]);
   set(h, 'FaceColor', color, 'EdgeColor', e_color)
else
   h = rectangle('Position', [starttime, y, scheduletime - starttime, height]);
   set(h, 'FaceColor', color, 'EdgeColor', e_color)
   if endtime
      h = rectangle('Position', [0, y, endtime, height]);
      set(h, 'FaceColor', color, 'EdgeColor', e_color)
   end
end

% Text
if nargin > 4
   text(starttime, (nr - 0.5), sprintf(' %s', message), 'Color', 'k');
end
