% Smith Chart Toolbox
% Version 1.0   13-Oct-95
% Author: Soeren Laursen, slau93@control.auc.dk
%
% The Smith Chart Toolbox contains routines for drawing and working
% with Smith charts. The Toolbox is designed for making illustrations
% --not for design. I have tried to make the m-files as flexible as
% possible so you can make the charts look the way you want. If think
% there are some functions missing, please let me know.
%
% To draw the charts you would probably first call SMISTAND to draw
% the chart and then you would use the other functions to plot the
% "traces" of the components. 
%
% To plot S-parameters you should just call SMISTAND and then enter
% PLOT(S) where S is a vector containing the s-parameters.
%
% To make an interactive approach possible there is a SMIUNDO routine
% that will allow you to step back through your operations.
%
% NOTE: The Smith Chart Toolbox assumes that all impedances and 
% admittances are normalized with respect to the characteristic 
% impedance Z0.
%
% Functions that work on the Smith chart
%   smistand Draw a standard smith chart.
%   smidraw  Draw a customized smith chart.
%   smircirc Draw impedance circle in the Smith chart.
%   smixcirc Draw admittance circle in the Smith chart.
%   smiim    Plot imittance trace into the chart.
%   smirotg  Rotate towards generator.
%   smirotl  Rotate towards load.
%   smiinv   Convert from impedance to admittance and vice versa.
%   smiundo  Undo last drawing operation.
%
% Other functions
%   z2gamma  Convert from impedance to reflection coefficient.
%   gamma2z  Convert from impedance to reflection coefficient.
