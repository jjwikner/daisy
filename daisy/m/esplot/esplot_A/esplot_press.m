function form=esplot_press()
% 



%% Defenitions 
    form.ver='A';
    form.unit = 'centimeters';
    form.figOrientation = 'portrait';
    
    form.figXSize = 24.0;
    form.figYSize = 16.0;
   
    form.color = [1 1 1];

    form.titleFontName='Helvetica';
    form.titleFontSize=26;
    
    form.subTitleFontName='Helvetica';
    form.subTitleFontSize=20;
    
    form.textFontName='Helvetica'; 
    form.textFontSize=20;
    
    form.axesFontName='Helvetica';
    form.axesFontSize=20;
    
    form.legendFontName='Helvetica';
    form.legendFontSize=15;
    
    form.nDualMarker = 5;

    form.lineFormat=[struct('lineWidth', 2,  'color', [0   0   1  ], 'lineStyle', '-' , 'marker', 'none', 'markerSize',  8, 'dual', false, 'dualColor', [0.0 0.0 0.9])
                     struct('lineWidth', 2,  'color', [0   0   1  ], 'lineStyle', '--', 'marker', 'none', 'markerSize',  8, 'dual', false, 'dualColor', [0.0 0.0 0.9])
                     struct('lineWidth', 2,  'color', [0.9 0   0.9], 'lineStyle', '-' , 'marker', 'o',    'markerSize',  8, 'dual', true,  'dualColor', [0.8 0.0 0.8])
                     struct('lineWidth', 2,  'color', [0.9 0   0.9], 'lineStyle', '--', 'marker', 'o',    'markerSize',  8, 'dual', true,  'dualColor', [0.8 0.0 0.8])
                     struct('lineWidth', 2,  'color', [0   0.9 0  ], 'lineStyle', '-' , 'marker', 's',    'markerSize',  8, 'dual', true,  'dualColor', [0.0 0.8 0.0])
                     struct('lineWidth', 2,  'color', [0   0.9 0  ], 'lineStyle', '--', 'marker', 's',    'markerSize',  8, 'dual', true,  'dualColor', [0.0 0.8 0.0])
                     struct('lineWidth', 2,  'color', [0   0   0.8], 'lineStyle', '-' , 'marker', '>',    'markerSize',  7, 'dual', true,  'dualColor', [0.0 0.0 0.7])
                     struct('lineWidth', 2,  'color', [0   0   0.8], 'lineStyle', '--', 'marker', '>',    'markerSize',  7, 'dual', true,  'dualColor', [0.0 0.0 0.7])
                     struct('lineWidth', 2,  'color', [0.5 0   0.8], 'lineStyle', '-' , 'marker', 'h',    'markerSize', 10, 'dual', true,  'dualColor', [0.4 0.0 0.7])
                     struct('lineWidth', 2,  'color', [0.5 0   0.8], 'lineStyle', '--', 'marker', 'h',    'markerSize', 10, 'dual', true,  'dualColor', [0.4 0.0 0.7])
                     struct('lineWidth', 2,  'color', [0   0.5 0  ], 'lineStyle', '-' , 'marker', 'x',    'markerSize', 10, 'dual', true,  'dualColor', [0.0 0.4 0.0])
                     struct('lineWidth', 2,  'color', [0   0.5 0  ], 'lineStyle', '--', 'marker', 'x',    'markerSize', 10, 'dual', true,  'dualColor', [0.0 0.4 0.0])];
                     

                 
                 
%% precalculations                 
                 
    form.nLineFormat = size(form.lineFormat,1);

end

