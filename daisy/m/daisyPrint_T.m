
clc; close all; clear all;

NOS = 2^12;
fSample = 1e9;
T = 1/fSample;
f_ = linspace(0, 8*fSample, NOS);

% See WEEKLY 21.
for O = 1:4
    sinc(O,:) = T/O * exp(-j*pi*f_*T/O) .* sin(pi*f_*T/O) ./ (pi*f_*T/O);
end;
x = f_/fSample;
y = abs(sinc)/T;
fh = figure(1); clf; 
ah = axes; set(ah, 'FontSize', 8); hold on;
plotStyles = {'g', 'c', 'b', 'r', '=^'};
for O = 1:4
    ph(O) = plot(x, y(O,:),plotStyles{O});
end;
     set(ph, 'LineWidth', 2);   
axisScaleFactor = 0.9; 
axis([ min(x) max(x) ...
       min([ axisScaleFactor*min(y) min(y)/axisScaleFactor ])  ...
       max([ axisScaleFactor*max(y) max(y)/axisScaleFactor ])]);
set(ph, 'LineWidth', 2);
set(ah, 'XTick', [0 1 2 3 4 5 6 7 8]);% 9 10 11 12]);
xh = xlabel('Frequency normalized w.r.t. sample frequency [-]');
yh = ylabel('Normalized amplitude gain [-]');
th = title('Simulated return-to-zero transfer functions');
columnWidth = 17; % 17 cm for DIN-A4
set(fh, 'PaperUnits', 'centimeters');
set(fh, 'Units', 'centimeters');
set(fh, 'PaperSize', [columnWidth 0.1*round(10*columnWidth/sqrt(2))]);
set(fh, 'PaperOrientation', 'portrait');
set(fh, 'PaperPositionMode','manual')
set(fh, 'PaperPosition', [0 0 columnWidth 0.1*round(10*columnWidth/sqrt(2))]);
oldPosition = get(fh, 'Position');
set(fh, 'Position', [1 1 1 1] + [0 0 columnWidth 0.1*round(10*columnWidth/sqrt(2))]);
lh = legend('O=1', 'O=2', 'O=3', 'O=4');
set(lh,'FontSize',8);


% tekdok snippet, to print the figure as EPS file in corresponding directory
daisyPrint(mfilename)

% tekdok snippet, to Auto-change the EPS file into different formats
%unix('daisyPlotForMatlab')
