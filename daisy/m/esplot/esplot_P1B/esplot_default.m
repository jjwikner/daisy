function form=esplot_default()
% Format definition 
% esplot_default


%% Defenition
    form.ver='B';
    form.unit = 'centimeters';
    form.figOrientation = 'portrait';
    form.figXSize = 17.0;
    form.figYSize = 12.0;
    %Framecolor
    form.color = [1 1 1];

    form.titleFontName='Helvetica';
    form.titleFontSize=26;
    
    form.subTitleFontName='Helvetica';
    form.subTitleFontSize=22;
    
    form.textFontName='Helvetica'; 
    form.textFontSize=14;
    
    form.axesFontName='Helvetica';
    form.axesFontSize=14;
    
    form.legendFontName='Helvetica';
    form.legendFontSize=14;
    
    form.nDualMarker = 5;

    form.lineFormat=[struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '-' , 'marker', 'none', 'markerSize', 8, 'dual', false)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '--', 'marker', 'none', 'markerSize', 8, 'dual', false)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '-' , 'marker', 'o',    'markerSize', 8, 'dual', true)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '--', 'marker', 'o',    'markerSize', 8, 'dual', true)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '-' , 'marker', 's',    'markerSize', 8, 'dual', true)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '--', 'marker', 's',    'markerSize', 8, 'dual', true)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '-' , 'marker', '>',    'markerSize', 7, 'dual', true)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '--', 'marker', '>',    'markerSize', 7, 'dual', true)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '-' , 'marker', 'h',    'markerSize', 10, 'dual', true)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '--', 'marker', 'h',    'markerSize', 10, 'dual', true)];
                     

                 
                 
%% precalculations                 
                 
    form.nLineFormat = size(form.lineFormat,1);

    %% Legend box adjustments
    form.legendbox.scale = [1.2 1.2];           % Linear scaling
    form.legendbox.borderoffset = [0.05 0.00];  % Fixed extra scaling
    form.legendbox.borderspace = [0.00 0.00];   % Fixed spacing to plot window

end

