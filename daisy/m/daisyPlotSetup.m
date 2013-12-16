function daisyPlotSetup(fileName)
% =====================================================
% All this could be added to a function, where input 
% arguments are the figure handle fh and the file name.
fh = gcf;
columnWidth = 16.8; % 17 cm for DIN-A4
set(fh, 'PaperUnits', 'centimeters');
set(fh, 'Units', 'centimeters');
set(fh, 'PaperSize', [columnWidth 0.1*round(10*columnWidth/sqrt(2))]);
set(fh, 'PaperOrientation', 'portrait');
set(fh, 'PaperPositionMode','manual')
set(fh, 'PaperPosition', [0 0 columnWidth 0.1*round(10*columnWidth/sqrt(2))]);
oldPosition = get(fh, 'Position');
set(fh, 'Position', [1 1 1 1] + [0 0 columnWidth 0.1*round(10*columnWidth/sqrt(2))]);
print( fh, '-depsc2', ...
       '-r300', '-tiff', fileName);
set(fh, 'Position', oldPosition);
% =====================================================
