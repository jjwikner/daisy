function [A] = smirotg(zstart, zend);
%SMIROTG Rotate impedance in the Smith chart towards generator.
%        When you move along a transmission line the impedance you
%        are looking into will change.
%
%        SMIROTG(ZSTART, ZEND) plots an arc between the original
%        impedance ZSTART and the new impedance ZEND. SMIROTG assumes
%        that you are moving towards the generator.
%
%        A = SMIROTG(ZSTART, ZEND) does not plot anything but instead
%        returns in vector A the points that constitutes the plot.
%
%        See also SMIROTL, SMIIM, SMIINV.

global SMITH_CHART_UNDO
coarseness = 63;
a = linspace(abs(z2gamma(zstart)), abs(z2gamma(zend)), coarseness);
ang1 = angle(z2gamma(zstart));
ang2 = angle(z2gamma(zend));
if ang1 < ang2 
  ang1 = ang1 + 2 * pi;
end
b = linspace(ang1, ang2, coarseness);
if nargout == 0 
  h=plot(a.*exp(j*b));
  set(h, 'LineWidth', 2);
  SMITH_CHART_UNDO = [SMITH_CHART_UNDO h];
else 
  A = a.*exp(j*b);
end