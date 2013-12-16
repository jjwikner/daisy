function fig = demDACgui()
global NOB layers axs nobh layh
% (c) jjwikner,MERC


% This should be a file containing the data.
% load DACdatavector DACdatavector
% Further on it will be types (hence fprintf...)
% Typically: set(DACdata,'unit impedance',1e-12) etc.
%% 
%% DACdatavector = ...
%%     [0.1e-18; ... 		    % Gunit = unit conductance
%%     12e-15; ... 		    % Cunit = unit capacitance
%%     1.22e-6; ... 		    % Iunit = unit output current
%%     0; ... 			    % Gpar = avg parasitic conductance
%%     0; ... 			    % Cpar = avg parasitic capacitance
%%     
%%     
%%     sigma   = 0.01;
%% sigma   = 0.0000000000000001;
%% % lite orimliga storlekar pa sigma
%% 
%% Rload   = 50;
%% Gload   = 1/Rload;
%% Cload   = 50e-9;
%%  				    
%% 	
%%     ];
%%     
%% Gunit = DACdatavector(1);

close all;
wndwid = 0.65;
wndhgt = 0.9;
btnwid = 0.20; btnwid = wndwid/3;
btnhgt = 0.03; btnhgt = wndhgt/35;
btnspcX = btnwid*1.5;
btnspcY = btnhgt*2;
Xstart = 0.03;
Ystart = 0.03; % this should be larger than (or equal to) btnhgt
frmhgt = 1-(2*Ystart);
frmwid = 2*Xstart+btnwid;
F1startY = Ystart+btnhgt+btnspcY;
F2startY = (1-Ystart-frmhgt);
frmclr = [0.2 0.8 0.8];
bkgclr = [0.7 0.8 0.8];

FileName = 'C:\My Documents\jacob\matlab\DAC\simDACgui.m';


% Main Simulation Window 

h0 = figure('Units','normalized', ...
    'Color',bkgclr, ...
    'Name', 'DAC Main Simulation Window', ...
    'NumberTitle', 'off', ... 
    'PaperType','A4', ...
    'PaperOrientation', 'landscape', ...
    'FileName',FileName, ...
    'PaperUnits','normalized', ...
    'PaperPosition',[0.17971 0.029823 0.64057 0.94035], ...
    'Position',[0.340 0.03 wndwid wndhgt], ...
    'Tag','DAC Main Simulation Window', ...
    'ToolBar','none');

% Frame for interactive interface

h1 = uicontrol('Parent',h0, ...
    'Units','normalized', ...
    'BackgroundColor',frmclr, ...
    'ListboxTop',0, ...
    'Position',[Xstart Ystart frmwid frmhgt], ...
    'Style','frame', ...
    'Tag','Frame1');

h1 = uicontrol('Parent',h0, ...
    'Units','normalized', ...
    'BackgroundColor',bkgclr, ...
    'ListboxTop',0, ...
    'Position',[Xstart*2 1-Ystart-btnspcY btnwid btnhgt], ...
    'Style','text', ...
    'String','Number of bits', ...
    'Tag','StaticText1');

nobh = uicontrol('Parent',h0, ...
    'Units','normalized', ...
    'BackgroundColor',[1 1 1], ...
    'ListboxTop',0, ...
    'Position',[2*Xstart 1-Ystart-2*btnspcY btnwid btnhgt], ...
    'Style','edit', ...
    'String',num2str(NOB), ...
    'Tag','EditText1', ...
    'Callback','drawDEMtree(''update NOB'')');

h1 = uicontrol('Parent',h0, ...
    'Units','normalized', ...
    'BackgroundColor',bkgclr, ...
    'ListboxTop',0, ...
    'Position',[Xstart*2 1-Ystart-3*btnspcY btnwid btnhgt], ...
    'Style','text', ...
    'String','Number of layers', ...
    'Tag','StaticText1');

layh = uicontrol('Parent',h0, ...
    'Units','normalized', ...
    'BackgroundColor',[1 1 1], ...
    'ListboxTop',0, ...
    'Position',[2*Xstart 1-Ystart-4*btnspcY btnwid btnhgt], ...
    'Style','edit', ...
    'String',num2str(layers), ...
    'Tag','EditText1', ...
    'Callback','drawDEMtree(''update layers'')');

axs = axes('Parent',h0, ...
    'Units','normalized', ...
    'CameraUpVector',[0 1 0], ...
    'Color',[1 1 1], ...
    'ColorOrder',[1 1 1], ...
    'Position',[frmwid+3*Xstart Ystart 1-4*Xstart-frmwid frmhgt], ...
    'Tag','Axes1', ...
    'XColor',[0 0 0], ...
    'YColor',[0 0 0], ...
    'ZColor',[0 0 0]);

h2 = text('Parent',axs, ...
    'Color',[0 0 0], ...
    'HandleVisibility','off', ...
    'HorizontalAlignment','center', ...
    'Position',[0.5 -0.2 9.2], ...
    'Tag','Axes1Text4', ...
    'VerticalAlignment','cap');
set(get(h2,'Parent'),'XLabel',h2);

h2 = text('Parent',axs, ...
    'Color',[0 0 0], ...
    'HandleVisibility','off', ...
    'HorizontalAlignment','center', ...
    'Position',[-0.15 0.5 9.2], ...
    'Rotation',90, ...
    'Tag','Axes1Text3', ...
    'VerticalAlignment','baseline');
set(get(h2,'Parent'),'YLabel',h2);

h2 = text('Parent',axs, ...
    'Color',[0 0 0], ...
    'HandleVisibility','off', ...
    'HorizontalAlignment','center', ...
    'Position',[1 1 1], ...
    'Tag','Axes1Text1', ...
    'VerticalAlignment','bottom');
set(get(h2,'Parent'),'Title',h2);

