function [h] = smircirc(r)
%SMIRCIRC Draw impedance circle in the Smith chart.
%         [H] = SMIRCIRC(R) draws a circle with r = R and returns
%         a handle to the circle in H.

global SMITH_CHART_UNDO
theta=linspace(-pi, pi, 63);
z = (r/(r+1) + 1/(r+1)*exp(j*theta));
if nargout > 0
  h = plot(z,'k');
  SMITH_CHART_UNDO = [SMITH_CHART_UNDO h];
else
  SMITH_CHART_UNDO = [SMITH_CHART_UNDO plot(z)];
end
