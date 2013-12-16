function [A] = smiinv(z);
%SMIINV Invert impedance in the Smith chart
%       SMIINV(Z) plots an impedance and its corresponding admittance
%       into the Smith chart. The two imittances are connected with
%       a straight line.
%
%       A = SMIINV(Z) returns the points for plotting in vector A but
%       does not plot them.
%
%       See also SMIIM, SMIROTG, SMIROTL.

global SMITH_CHART_UNDO
if nargout == 0
  h=plot(z2gamma([z 1/z]));
  set(h, 'LineWidth', 2);
  SMITH_CHART_UNDO = [SMITH_CHART_UNDO h];
else
  A = z2gamma([z 1/z]);
end

