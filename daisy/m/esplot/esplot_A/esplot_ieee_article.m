function form=esplot_ieee_article()
% Format definition 
% esplot_default

scale=0.45;

%% Defenition
    form.ver='A';
    form.unit = 'centimeters';
    form.figOrientation = 'portrait';
    form.figXSize = 11.0/scale;
    form.figYSize = 8.0/scale;
    %Framecolor
    form.color = [1 1 1];

    form.titleFontName='Times';
    form.titleFontSize=10/scale;
    
    form.subTitleFontName='Times';
    form.subTitleFontSize=8.5/scale;
    
    form.textFontName='Times'; 
    form.textFontSize=8.5/scale;
    
    form.axesFontName='Times';
    form.axesFontSize=8.5/scale;
    
    form.legendFontName='Times';
    form.legendFontSize=8.5/scale;
    
    form.nDualMarker = 5;

    form.lineFormat=[struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '-' , 'marker', 'none', 'markerSize',   4/scale, 'dual', false)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '--', 'marker', 'none', 'markerSize',   4/scale, 'dual', false)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '-' , 'marker', 'o',    'markerSize',   4/scale, 'dual', true)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '--', 'marker', 'o',    'markerSize',   4/scale, 'dual', true)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '-' , 'marker', 's',    'markerSize',   4/scale, 'dual', true)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '--', 'marker', 's',    'markerSize',   4/scale, 'dual', true)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '-' , 'marker', '>',    'markerSize',   4/scale, 'dual', true)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '--', 'marker', '>',    'markerSize',   4/scale, 'dual', true)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '-' , 'marker', 'h',    'markerSize', 5.5/scale, 'dual', true)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '--', 'marker', 'h',    'markerSize', 5.5/scale, 'dual', true)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '-' , 'marker', 'x',    'markerSize', 5.5/scale, 'dual', true)
                     struct('lineWidth', 2,  'color', [0 0 0], 'lineStyle', '--', 'marker', 'x',    'markerSize', 5.5/scale, 'dual', true)];
                     

                 
                 
%% precalculations                 
                 
    form.nLineFormat = size(form.lineFormat,1);

end

