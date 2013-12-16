function form=esplot_ieee_article()
% Format definition 
% esplot_default

scale=0.45;

%% Defenition
    form.ver='B';
    form.unit = 'centimeters';
    form.figOrientation = 'portrait';
    %form.figXSize = 11.0/scale;
    %form.figYSize = 8.0/scale;
    form.figXSize  = esplot_pica2cm(20.5)/scale; %21-0.25*2
    form.figYSize  = esplot_pica2cm(14.5)/scale; %15-0.25*2
    form.figMargin = esplot_pica2cm(0.25)/scale;
    
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
    
    form.nDualMarker = 7;

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
    
%% Legend box adjustments
    form.legendbox.scale = [1.25 1.25];           % Linear scaling
    form.legendbox.borderoffset = [0.12 0.00];  % Fixed extra scaling
    form.legendbox.borderspace = [0.00 0.00];   % Fixed spacing to plot window
    

end

function cm=esplot_pica2cm(pica)
    cm = 30.5 / 72 * pica; 
end


