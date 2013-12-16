%
% User: jacobw
% Project name: lafe
% Project area: /site/edu/es/EXJOBB/videoFront/
%
% Department of Electrical Engineering
% Linkoping University 
%
% Thu Feb 25 08:41:01 CET 2010
%

% This is a demo for setting up the pretty plotting for documentation.

close all

% Some dummy curve for illustration
NOS = 1024;
x = 0:(NOS-1);
y = sin(2*pi*x*daisyPrimeSig(1e6,1e9,NOS)/1e9);

% Always assign handles!
fh = figure; clf; 

% Also for the axes.
ah = axes; set(ah, 'FontSize', 14);

% Plot the function and assign handle.
ph = plot(x, y);

% Use some relative scaling on the axis
axisScaleFactor = 0.9; 

% (You might need to add a constant in case 
% max or min y are 0)

axis([ min(x) max(x) ...
       min([ axisScaleFactor*min(y) min(y)/axisScaleFactor ])  ...
       max([ axisScaleFactor*max(y) max(y)/axisScaleFactor ])]);

axis([ min( get(get(gca,'children'),'XData') ) ...
       max( get(get(gca,'children'),'XData') ) ...
       min([ axisScaleFactor*min( get(get(gca,'children'),'YData'))  ...
             min(get(get(gca,'children'),'YData'))/axisScaleFactor ])  ...
       max([ axisScaleFactor*max(get(get(gca,'children'),'YData')) ...
             max(get(get(gca,'children'),'YData') )/axisScaleFactor ])]);

% Plot with thicker lines       
set(ph, 'LineWidth', 2);

% Modify the indexes properly. If you plot 1024 values, do not print
% 1000, instead print some of the 2's component values

set(ah, 'XTick', 128*[1 2 3 4 5 6 7 ]);

% Always add labels and title

xh = xlabel('Index, n ');
yh = ylabel('Amplitude, V');
th = title('Simulated sinusoid waveform');



% Setting up the paper format
% 
columnWidth = 16.8; % 17 cm for DIN-A4

% All this could be added to a function, where input 
% arguments are the figure handle fh and the file name.

% GET RID OF INCHES!!! GRRR! AARRRGH!
set(fh, 'PaperUnits', 'centimeters');
set(fh, 'Units', 'centimeters');

% Use portrait to avoid rotation to enable import to openoffice
set(fh, 'PaperSize', [columnWidth 0.1*round(10*columnWidth/sqrt(2))]);
set(fh, 'PaperOrientation', 'portrait');
set(fh, 'PaperPositionMode','manual')
set(fh, 'PaperPosition', [0 0 columnWidth 0.1*round(10*columnWidth/sqrt(2))]);

% Change the position to something that corresponds to the eps
% and document for wysiwyg.

oldPosition = get(fh, 'Position');
set(fh, 'Position', [1 1 1 1] + [0 0 columnWidth 0.1*round(10*columnWidth/sqrt(2))]);
print( fh, '-depsc2','-r300', '-tiff', './daisyVideoTop/doc/plotDemo.eps');

% A flag could be used to restore the position 
% set(fh, 'Position',oldPosition);
