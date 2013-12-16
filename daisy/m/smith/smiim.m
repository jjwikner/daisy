function [A] = smiim(zstart, zend);
%SMIIM  Plot imittance into the Smith chart.
%       When an imittance (impedance or admittance) is added in parallel
%       or series with an existing imittance the result will be a new
%       imittance.
%
%       SMIIM(ZSTART, ZEND) plots a curve between ZSTART, which can
%       be regarded as the original immitance, and ZEND, which is the
%       new imittance.
%
%       The example belows shows how an impedance of 25 Ohm is connected
%       in series with (100+40i) Ohm. Notice the normalization of Z1 and
%       Z2:
%             Z1 = 100+40i
%             Z2 = Z1 + 25
%             SMIIM(Z1/50, Z2/50)
%
%       A = SMIIM(ZSTART, ZEND) does not plot anything but instead 
%       returns in vector A the points that constitutes the plot.
%
%       See also SMIINV, SMIROTG, SMIROTL.

global SMITH_CHART_UNDO
coarseness = 63;
a = linspace(real(zend), real(zstart), coarseness);
b = linspace(imag(zend), imag(zstart), coarseness);
if nargout == 0
  h=plot(z2gamma(a+j*b));
  set(h, 'Linewidth', 2);
  SMITH_CHART_UNDO = [SMITH_CHART_UNDO h];
else 
  A = z2gamma(a+j*b);
end