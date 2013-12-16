function lh = arrow(X,Y,ArrowType,ArrowWidth,ArrowColor);

% arrow(X,Y,ArrowType,ArrowWidth,ArrowColor);
% 
% Adds an arrow in vectors X and Y to the current axes. 
% The arrow head is plotted in the end point of the line.
%
% If X and Y are matrices the same size, a multisegmented 
% arrow is plotted with one arrow head plotted in the startpoint.
%
% ArrowType : '0' = One arrow head (default) 
%             '1' = Two arrow heads
%
% ArrowWidth = linewidth of the arrow... 
%              default value is 1
% ArrowColor = color (RGB vector) of the arrow...
%              default value is [0 0 0] (black)
%              
% (c) Niklas U. Andersson & K. Ola Andersson, MERC, 
%     Ericsson Microelectronics AB. Okt. 9, 2000.
%     Linkoping, Sweden.



if (nargin == 2) 
  ArrowWidth = 1;
  ArrowColor = [0 0 0]; 
  ArrowType = 0;
elseif (nargin == 3)
  ArrowWidth = 1;
  ArrowColor = [0 0 0];
elseif (nargin == 4)
  ArrowColor = [0 0 0];
end;
  
% Save the axis to compensate for the current scale
% in the figure

axlar = axis;

% Calculate the angle of the arrow line

d_alpha = pi/8;

if X(1) == X(2)
  alpha = pi/2;
elseif Y(1) == Y(2)
  alpha = 0;
else
  alpha = atan((Y(2)-Y(1))/(axlar(4)-axlar(3))/...
    ((X(2)-X(1))/(axlar(2)-axlar(1))));
end;

% In which direction does the arrow point?

if (Y(1)>Y(2) | X(1)>X(2))
  d_alpha = pi/8 - pi;
end;

if (Y(1)>Y(2) & X(1)<X(2))
 d_alpha = pi/8;
end;

if (Y(1)>Y(2) & X(1)>X(2))     
  d_alpha = pi/8 - pi;
end;

% Calculate the arrow head size 

r = 0.03;

dx1 =  r * cos(alpha+d_alpha)*(axlar(2)-axlar(1));
dy1 =  r * sin(alpha+d_alpha)*(axlar(4)-axlar(3));

dx2 =  r * cos(alpha-d_alpha)*(axlar(2)-axlar(1));
dy2 =  r * sin(alpha-d_alpha)*(axlar(4)-axlar(3));
 

% Plot the arrow 

hold on;

if ArrowType == 0
  lh = [line(X,Y);...
      line([X(2) X(2)-dx1],[Y(2) Y(2)-dy1]);...
      line([X(2) X(2)-dx2],[Y(2) Y(2)-dy2]);];
else
  lh = [line(X,Y);...
      line([X(1) X(1)+dx1],[Y(1) Y(1)+dy1]);...
      line([X(1) X(1)+dx2],[Y(1) Y(1)+dy2]);...
      line([X(2) X(2)-dx1],[Y(2) Y(2)-dy1]);...
      line([X(2) X(2)-dx2],[Y(2) Y(2)-dy2]);];
end;

set(lh,'LineWidth',ArrowWidth,...
    'Color',ArrowColor);

hold off;






