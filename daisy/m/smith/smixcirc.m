function [h] = smixcirc(r)
%SMIXCIRC Draw admittance circle in the Smith chart.
%         [H] = SMIXCIRC(G) draws a circle with g = G and returns
%         a handle to the circle in H. 
%
%         See also SMIRCIRC, SMIDRAW, SMISTAND.

global SMITH_CHART_UNDO
theta=linspace(-pi, pi, 63);
z = -(r/(r+1) + 1/(r+1)*exp(j*theta));
if nargout > 0
  h = plot(z);
  SMITH_CHART_UNDO = [SMITH_CHART_UNDO h];
else
  SMITH_CHART_UNDO = [SMITH_CHART_UNDO plot(z)];
end