function [A] = smirotl(zstart, zend);
%SMIROTL Rotate impedance in the Smith chart towards load.
%        When you move along a transmission line the impedance you
%        are looking into will change.
%
%        SMIROTL(ZSTART, ZEND) plots an arc between the original
%        impedance ZSTART and the new impedance ZEND. SMIROTG assumes
%        that you are moving towards the load.
%
%        A = SMIROTL(ZSTART, ZEND) does not plot anything but instead
%        returns in vector A the points that constitutes the plot.
%
%        See also SMIROTG, SMIIM, SMIINV.

if nargout == 0 
  smirotg(zend, zstart);
else
  A = smirotg(zend, zstart);
end