function [ph] = lollipop(x, y, color)

% lollipop(x,y,color)	Plot lollipops (o's and sticks)

if (nargin < 3)
    color = '';
end


h = ishold;
hold on;

%Plot circles
ph(1) = plot(x, y, ['o' color]);

% Make x and y row vectors, then plot as sticks
x = x(:)'; 
y = y(:)';

x = [x; x; nan*ones(size(x))];

y = [y; zeros(2,length(y))];

ph(2) = plot(x(:),y(:),color);
set(ph,'LineWidth',2);

if ~h
    hold off;
end
