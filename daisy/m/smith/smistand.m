function smistand
%SMISTAND Draw standard Smith chart.
%         SMISTAND draws a normal Smith chart that can be used for
%         subsequent plotting.
%
%         See also SMIDRAW.
clf
% Make sure the circles get round
axis('equal')
axis('off')
smidraw([0.1 0.2 0.5 1 2 4], [0.1 0.2 .5 1.0 2 4], 'xr');
