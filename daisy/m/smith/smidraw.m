function smidraw(rvalues, xvalues, axismode)
%SMIDRAW Draw Smith chart
%        SMIDRAW(RVALUES, XVALUES) draws a customized Smith chart using RVALUES 
%        for the constant-r-circles and XVALUES for the constant-x-circles.
%
%        The r = 0 circle which encloses the Smith chart and the horizontal line 
%        x = 0 is for technical reasons always drawn.
%
%        To control the drawing of the numbers on the axes you can use 
%        SMIDRAW(RVALUES, XVALUES, AXISMODE) where AXISMODE is a string that
%        can be one of following:
%                'x'    Number the x-values.
%                'r'    Number the r-values.
%                'rx'   Number both.
%
%        If you do not specify AXISMODE no numbers will be printed.
%
%        See also: SMISTAND, SMIRCIRC, SMIXCIRC.

% Set up the undo information.
global SMITH_CHART_UNDO
% The number of points for plotting the circles and curves. If you increase this
% parameter, copy and paste might not work under MS-WINDOWS.
coarseness = 63;
hold on
if nargin > 2 
  if axismode == 'x'
    printxnumbers = 1;
  elseif axismode == 'r'
    printrnumbers = 1;
  elseif (axismode == 'rx') | (axismode == 'xr')
    printxnumbers = 1;
    printrnumbers = 1;
  elseif axismode ~= ''
    error('Third argument to SMIDRAW must be ''x'', ''r'' or ''rx''.');
  end
end
% Draw the horisontal line
plot([-1 1], [0 0]);
% Add the r = 0 circle if not already done.
if sum(rvalues~=0)
  rvalues = [0 rvalues];
end
% Draw the r-circles
for r = rvalues
  smircirc(r);
  if printrnumbers & (r ~= 0)
    xpos = z2gamma(r);
    h=text(xpos, 0, num2str(r));
    set(h, 'VerticalAlignment', 'top', 'HorizontalAlignment', 'right');
  end
end
v=linspace(0,sqrt(100), coarseness);
r = v.^2;
r = [0 r];
for x = abs(xvalues)
  z = z2gamma(r+j*x*ones(1, coarseness+1));
  plot(z);
  plot(conj(z));
  if printxnumbers
    xpos = real(z2gamma(j*x));
    ypos = imag(z2gamma(j*x));
    h=text([xpos xpos], [ypos -ypos], [' j' num2str(x) ; '-j' num2str(x)]);
    set(h(1),'VerticalAlignment', 'bottom');
    set(h(2),'VerticalAlignment', 'top');
    if xpos == 0 
      set(h, 'Horizontalalignment', 'center');
    elseif xpos < 0
      set(h, 'Horizontalalignment', 'right');
    end
    h=text(-1, 0, '0');
    set(h, 'VerticalAlignment', 'middle', 'HorizontalAlignment', 'right');
  end
end
% The user should not be able to remove the chart itself.
SMITH_CHART_UNDO = [];
