function conpres(operation,extraop);

% MatLab function to present results from AD measurements.

% ========================================================
% Global variables for the user interface and handles.
% ========================================================

% Global variables for buttons, popup, info text

global XPOS_TEXT Y_TEXT1 Y_TEXT2 Y_TEXT3 E_TEXT1 E_TEXT2 E_TEXT3 E_TEXT4;
global ENOB_TEXT FUNCTYPE_TEXT NOB_TEXT;
global XPOS_ACT Y_ACT1 Y_ACT2 Y_ACT3 E_ACT1 E_ACT2 E_ACT3 E_ACT4;
global FUNCTYPE_ACT AMAX_ACT AMIN_ACT START_ACT STOP_ACT;
global NOB_PRES ENOB_PRES;
global GRAPHTYPE_ACT FILE_ACT;

% Global variables for the 
 
global FITLINE AXISP BCOLOR PLOTTYPE MAXY MINY FUNCTYPE;
global WITNESSLINE YVERTICAL DATAFILE FUNCTIONNAMES;
global NEWX NEWY STRUCTURE CURSORSTATE YENDPTS MAXX MINX XOFFSET XFACTOR;
global DIGITALVALUES XVALUES YVALUES AMPMAX AMPMIN STARTS STOPS;
global E1 E2 E3 E4 NOB ENOB;

% ========================================================

if (nargin < 1)			% checking if the call of conpres program
    ACT = 'setup';		% is of startup mode
elseif isstr(operation)
    ACT = operation;		% else a special operation type is specified
end;

% ***********
% ** SETUP **
% ***********

if (strcmp(ACT,'setup'))

% Initialization of the figure window

figNo = figure( ...		
	 'Name','Converters Presentation', ...
	 'NumberTitle','off', ...
	 'BackingStore','off');

% Initialization of the global function variables
% The first specifications in the setup define
% a sine function with the amplitude 1,
% two periods, 128 samples per period, 8 bit
% two-complement resolution

FUNCTIONNAMES = ['sine' 'ramp' 'sawt' 'tria' 'sqR0' 'sq+-'];
AMPMAX = 0.0004;
AMPMIN = -0.0004;
NOB = 8;
FUNCTYPE = 1;
DIGITALVALUES = AD_convert(FUNCTIONNAMES(4*(FUNCTYPE-1)+1:4*FUNCTYPE),AMPMAX,2,128,NOB,1,AMPMAX/(2^(NOB-1)-1));
STARTS = 1;
STOPS = size(DIGITALVALUES,1);
NOB = size(DIGITALVALUES,2);
XFACTOR = 1;
XOFFSET = 0;
DATAFILE = '<None>';
PLOTTYPE = 1;
FUNCTYPE = 1;
YVALUES = DA_convert(DIGITALVALUES(STARTS:STOPS,1:NOB),1,AMPMAX/(2^(NOB-1)-1),1);
XVALUES = (1:size(YVALUES,2));
E1 = mean(YVALUES);
E2 = std(YVALUES);
E3 = 0;
E4 = 0;
ENOB = 0;

% Initialization of the global graph functions

AXISP = [0.10 0.10 0.55 0.80];
MAXX = max(XVALUES);
MINX = min(XVALUES);
rangex = MAXX-MINX;
MAXY = max(YVALUES);
MINY = min(YVALUES);
CURSORSTATE = 'arrow';
YENDPTS = get(gca,'YLim');
YVERTICAL = linspace(YENDPTS(1),YENDPTS(2))';
xvert = ones(size(YVERTICAL)) * (MAXX+MINX)/2;
NEWX = xvert(1);	
NEWY = YVALUES(NEWX*XFACTOR + XOFFSET);
BCOLOR = [0.5 0.5 0.5];
zoom on;

% Initialization of user interface buttons

top = 0.97;
bottom = 0.03;
labelColor = [0.8 0.8 0.8];
btnWid = 0.20;
btnHt = 0.03;
right = 0.97;
left = right - btnWid;
spacing = 0.02;

% Initialization of the input frame

frmBorder = 0.02;
frmPos = [left-frmBorder bottom-frmBorder btnWid+2*frmBorder (top-bottom)+2*frmBorder];
h = uicontrol( ...
     'Style','frame', ...
     'Units','normalized', ...
     'Position',frmPos, ...
     'BackgroundColor',[0.5 0.5 0.5]);

% Definition of the input ACTs positions

RefBtnPos	= [left			top-2*btnHt			btnWid		btnHt];
RefActBtnPos    = [left			top-8*btnHt-3*spacing		0.95*btnWid/2	btnHt];
xActPos		= [left			top-9*btnHt-6*spacing		btnWid		btnHt];
yActPos		= [left			top-btnHt-(btnHt+spacing)*6	btnWid		btnHt];
infoBtnPos	= [left			bottom				0.95*btnWid/2	btnHt];
closeBtnPos	= [left+1.05*btnWid/2	bottom				0.95*btnWid/2	btnHt];
SpecActPos	= [left			top-3*btnHt			0.95*btnWid/2	btnHt];

% Definition of the input ACTs functions

FILE_TEXT	= uicontrol('Style','text',...
		   'Position',RefBtnPos + [0 btnHt 0 0],...
		   'Units','normalized','BackgroundColor',BCOLOR,...
	           'ForegroundColor','white','String','Loaded Data File');

FILE_ACT	= uicontrol('Style','edit', ...
     		  'Units','normalized', ...
		  'Position',RefBtnPos, ...
		  'BackgroundColor','white',...
		  'String',DATAFILE, ...
		  'Callback','conpres(''FILE_ACT'');');

START_TEXT	= uicontrol('Style','text', ...
		   'Position',SpecActPos,...
		   'Units','normalized','BackgroundColor',BCOLOR,...
	           'ForegroundColor','white','String','Start');

START_ACT	= uicontrol('Style','edit', ...
     		  'Units','normalized', ...
		  'Position',SpecActPos - [0 btnHt 0 0], ...
		  'BackgroundColor','white',...
		  'String',STARTS, ...
		  'Callback','conpres(''newstart'');');

STOP_TEXT	= uicontrol('Style','text',...
		   'Position',SpecActPos - [-1.05*btnWid/2 0 0 0],...
		   'Units','normalized','BackgroundColor',BCOLOR,...
	           'ForegroundColor','white','String','Stop');

STOP_ACT	= uicontrol('Style','edit', ...
     		  'Units','normalized', ...
		  'Position',SpecActPos - [-1.05*btnWid/2 btnHt 0 0], ...
		  'BackgroundColor','white',...
		  'String',DATAFILE, ...
		  'Callback','conpres(''newstop'');');

AMAXPOS_TEXT	= uicontrol('Style','text',...
		   'Position',SpecActPos - [0 2*btnHt 0 0],...
		   'Units','normalized','BackgroundColor',BCOLOR,...
	           'ForegroundColor','white','String','Amax');

AMAX_ACT	= uicontrol('Style','edit', ...
     		  'Units','normalized', ...
		  'Position',SpecActPos - [0 3*btnHt 0 0], ...
		  'BackgroundColor','white',...
		  'String',AMPMAX, ...
		  'Callback','conpres(''newmax'');');

AMIN_TEXT	= uicontrol('Style','text',...
		   'Position',SpecActPos - [-1.05*btnWid/2 2*btnHt 0 0],...
		   'Units','normalized','BackgroundColor',BCOLOR,...
	           'ForegroundColor','white','String','Amin');

AMIN_ACT	= uicontrol('Style','edit', ...
     		  'Units','normalized', ...
		  'Position',SpecActPos - [-1.05*btnWid/2 3*btnHt 0 0], ...
		  'BackgroundColor','white',...
		  'String',AMPMIN, ...
		  'Callback','conpres(''newmin'');');

NOB_TEXT	= uicontrol('Style','text',...
		   'Position',SpecActPos - [0 4*btnHt 0 0],...
		   'Units','normalized','BackgroundColor',BCOLOR,...
	           'ForegroundColor','white','String','No of bits');

NOB_PRES	= uicontrol('Style','text', ...
     		  'Units','normalized', ...
		  'Position',SpecActPos - [0 5*btnHt 0 0], ...
		  'BackgroundColor','white',...
		  'String',NOB);

ENOB_TEXT	= uicontrol('Style','text',...
		   'Position',SpecActPos - [-1.05*btnWid/2 4*btnHt 0 0],...
		   'Units','normalized','BackgroundColor',BCOLOR,...
	           'ForegroundColor','white','String','No of eff. bits');

ENOB_PRES	= uicontrol('Style','text', ...
     		  'Units','normalized', ...
		  'Position',SpecActPos - [-1.05*btnWid/2 5*btnHt 0 0], ...
		  'BackgroundColor','white',...
		  'String',ENOB);

GRAPHTYPE_TEXT	= uicontrol('Style','text',...
		   'Position',RefActBtnPos + [0 btnHt 0 0],...
		   'Units','normalized','BackgroundColor',BCOLOR,...
	           'ForegroundColor','white','String','Graph Type');

GRAPHTYPE_ACT	= uicontrol('Style','Popup',...
		   'String','Time|FFT|Log FFT|Histogram|DNL|INL',...
		   'Position',RefActBtnPos,'Units','normalized',...
		   'CallBack','conpres(''change_plottype'')');

% Default
set(GRAPHTYPE_ACT,'Value',PLOTTYPE);

FUNCTYPE_TEXT	= uicontrol('Style','text',...
		   'Position',RefActBtnPos + [1.05*btnWid/2 btnHt 0 0],...
		   'Units','normalized','BackgroundColor',BCOLOR,...
	           'ForegroundColor','white','String','Function Type');

FUNCTYPE_ACT	= uicontrol('Style','Popup',...
		   'String','Sine|Sawtooth|Ramp|Triangular|Pulse 0/1|Pulse +/-',...
		   'Position',RefActBtnPos+[1.05*btnWid/2 0 0 0],'Units','normalized',...
		   'CallBack','conpres(''change_FUNCTYPE'')');

% Default
set(FUNCTYPE_ACT,'Value',FUNCTYPE);

XPOS_TEXT	= uicontrol('Style','text',...
		   'Position',xActPos + [0 btnHt 0 0],...
		   'Units','normalized','BackgroundColor','blue',...
	           'ForegroundColor','black','String','X Value');

XPOS_ACT	= uicontrol('Style','edit','Position',xActPos,...
		   'Units','normalized','String',num2str(NEWX),...
		   'ForegroundColor','white','BackgroundColor','blue',...
		   'CallBack','conpres(''edittext'')');

Y_TEXT1		= uicontrol('Style','text',...
		   'Position',xActPos - [0 btnHt 0 0],...
	           'Units','normalized','BackgroundColor','blue',...
	           'ForegroundColor','black','String','Y Value');

Y_ACT1		= uicontrol('Style','text','Units','normalized',...
		   'Position',xActPos - [0 2*btnHt 0 0],...
		   'String',num2str(NEWY),...
	           'ForegroundColor','white','BackgroundColor','blue');

Y_TEXT2		= uicontrol('Style','text',...
		   'Position',xActPos - [0 3*btnHt 0 0],...
	           'Units','normalized','BackgroundColor','blue',...
	           'ForegroundColor','black','String','Y Max');

Y_ACT2		= uicontrol('Style','text','Units','normalized',...
		   'Position',xActPos - [0 4*btnHt 0 0],...
		   'String',num2str(MINY),...
	           'ForegroundColor','white','BackgroundColor','blue');

Y_TEXT3		= uicontrol('Style','text',...
		   'Position',xActPos - [0 5*btnHt 0 0],...
	           'Units','normalized','BackgroundColor','blue',...
	           'ForegroundColor','black','String','Y Min');

Y_ACT3		= uicontrol('Style','text','Units','normalized',...
                   'Position',xActPos - [0 6*btnHt 0 0],...
		   'String',num2str(MINY),...
		   'ForegroundColor','white','BackgroundColor','blue');	

E_TEXT1		= uicontrol('Style','text',...
		   'Position',xActPos - [0 7*btnHt+spacing 0 0],...
	           'Units','normalized','BackgroundColor','red',...
	           'ForegroundColor','black','String','Mean Value');

E_ACT1		= uicontrol('Style','text','Units','normalized',...
                   'Position',xActPos - [0 8*btnHt+spacing 0 0],...
		   'String',num2str(E1),...
		   'ForegroundColor','white','BackgroundColor','red');	

E_TEXT2		= uicontrol('Style','text',...
		   'Position',xActPos - [0 9*btnHt+spacing 0 0],...
	           'Units','normalized','BackgroundColor','red',...
	           'ForegroundColor','black','String','Standard Deviation');

E_ACT2		= uicontrol('Style','text','Units','normalized',...
                   'Position',xActPos - [0 10*btnHt+spacing 0 0],...
		   'String',num2str(E2),...
		   'ForegroundColor','white','BackgroundColor','red');	

E_TEXT3		= uicontrol('Style','text',...
		   'Position',xActPos - [0 11*btnHt+spacing 0 0],...
	           'Units','normalized','BackgroundColor','red',...
	           'ForegroundColor','black','String','');

E_ACT3		= uicontrol('Style','text','Units','normalized',...
                   'Position',xActPos - [0 12*btnHt+spacing 0 0],...
		   'String',num2str(E3),...
		   'ForegroundColor','white','BackgroundColor','red');	

E_TEXT4		= uicontrol('Style','text',...
		   'Position',xActPos - [0 13*btnHt+spacing 0 0],...
	           'Units','normalized','BackgroundColor','red',...
	           'ForegroundColor','black','String','');

E_ACT4		= uicontrol('Style','text','Units','normalized',...
                   'Position',xActPos - [0 14*btnHt+spacing 0 0],...
		   'String',num2str(E4),...
		   'ForegroundColor','white','BackgroundColor','red');	

RESET_BUTTON	= uicontrol('Style','Pushbutton','Position',infoBtnPos+[0 btnHt+spacing 0 0],...
		   'Units','normalized','BackgroundColor','blue',...
		   'Callback','conpres(''reset'')', ...
		   'String','Reset');

INFO_BUTTON	= uicontrol('Style','Pushbutton','Position',infoBtnPos,...
		   'Units','normalized','BackgroundColor','yellow',...
		   'Callback','conpres(''info'')', ...
		   'String','Info');

CLOSE_BUTTON	= uicontrol('Style','Pushbutton','Position',closeBtnPos,...
		   'Units','normalized','BackgroundColor','green',...
		   'Callback','conpres(''done'')', ...
		   'String','Close');

conpres('setdata');

% END OF SETUP AND USER INTERFACE SETUP

% *********
% * RESET *
% *********

elseif (strcmp(ACT,'reset')),
%	zoom out;

% END OF RESET

% ***************
% * INFORMATION *
% ***************

elseif (strcmp(ACT,'info')),
    ttlStr=get(gcf,'Name');
    hlpStr= ...                                                 
        ['  =========================================                '
	 '                                ConPres                      '
	 '                      Converter PRES 1996            '
	 '  =========================================                '
	 '                                                             '
	 '  Program to present results from measured AD converters.    '
	 '                                                             '
	 '  Menu operations:                                           '
	 '                                                             '
	 '  Loaded Data File,                                          '
	 '  file will be loaded with the csv format (se MatLab Help).  '
	 '                                                             '
 	 '  Start,                                                     '
	 '  defines start value of the data, could be used to zoom,    '
	 '  but also affects FFT, LogFFT, Histogram, DNL and INL.      '
	 '                                                             '
	 '  Stop,                                                      '
	 '  defines start value of the data, could be used to zoom,    '
	 '  but also affects FFT, LogFFT, Histogram, DNL and INL.      '
	 '                                                             '
	 '  Amax,                                                      '
	 '  defines the maximal positive amplitude.                    '
	 '                                                             '
	 '  Amin,                                                      '
	 '  defines the minimal negative amplitude.                    '
	 '                                                             '
	 '  No of bits,                                                '
	 '  the number of bits is displayed.                           '
	 '                                                             '
	 '  No of eff. bits,                                           '
	 '  will be computed as soon as the LogFFT mode once is chosen '
	 '                                                             '
	 '  Graph Type, here the PRES type can be chosen:      '
	 '     Time: Time domain PRES                          '
	 '     FFT: Fast Fourier Transform, (Frequency Domain)         '
	 '     LogFFT: Logarithmic FFT, (Frequency Domain)             ' 
	 '     Histogram: Displays the occurance of digital codes      '
	 '     DNL: Differential Nonlinearity, DNL is computed stat-   '
	 '     istically, the ideal statistical beahaviour is set by   '
	 '     Function Type below.                                    '
	 '                                                             '
	 '  Function Type,                                             '
	 '  expected function type can be set; sine, sawtooth, ramp,   '
	 '  triangular, pulse 0/1 and pulse +/-                        '
	 '                                                             '
	 '  In the blue ACT special values is the displayed. Theese  '
	 '  values depend on the Graph Type and thE value chosen by    '
	 '  the graphic interface. The dashed line can be moved by     '
	 '  using the mouse. The position can also be given from the   '
	 '  keyboard.                                                  '
	 '                                                             '
	 '  The red ACT diaplays special computed values, as for     '
	 '  example SNR, SINAD, THD and similuar.                      '
	 '                                                             '
	 '  Info Button, displays this message.                        '
	 '                                                             '
	 '  Close Button, ends the program.                            '
	 '                                                             '
	 '  =================================================          '];
    helpfun(ttlStr,hlpStr);                                     

% END OF INFORMATION

% *****************
% * LOAD NEW DATA *
% *****************

elseif strcmp(ACT,'FILE_ACT'),
    DATAFILE = get(FILE_ACT,'String');
    DIGITALVALUES = csvread(DATAFILE);
    NOB = size(DIGITALVALUES,2);
    STARTS = 1;
    STOPS = size(DIGITALVALUES,1);
    YVALUES = DA_convert(DIGITALVALUES(STARTS:STOPS,1:NOB),1,AMPMAX/(2^(NOB-1)-1),0);
    XVALUES = (1:size(YVALUES,2));
    MAXX = max(XVALUES);
    MINX = min(XVALUES);
    rangex = MAXX - MINX;
    MAXY = max(YVALUES);
    MINY = min(YVALUES);
    PLOTTYPE = 1;   
    set(GRAPHTYPE_ACT,'Value',PLOTTYPE);
    XFACTOR = 1;
    XOFFSET = 0; 
    NEWX = (MAXX+MINX)/2;
    NEWY = YVALUES(NEWX*XFACTOR+XOFFSET); 
    conpres('setdata');
% END OF LOAD NEW DATA

% ***********************
% * CHANGE FUNCTYPE *
% ***********************

elseif strcmp(ACT,'change_FUNCTYPE')
  FUNCTYPE = get(FUNCTYPE_ACT,'Value');
  conpres('change_plottype');
% END OF CHANGE FUNCTYPE


% **********
% * MOTION *
% **********

elseif strcmp(ACT,'motion'),
    if extraop == 0,					
        cp = get(gca,'CurrentPoint');
        cx = cp(1,1);
        cy = cp(1,2);
        fuzz = 0.01 * (MAXX - MINX);       
        online = cy > YENDPTS(1)  & cy < YENDPTS(2) & cx > NEWX - fuzz & cx < NEWX + fuzz;
        if online & strcmp(CURSORSTATE,'arrow'),
            CURSORSTATE = 'crosshair';
            set(gcf,'Pointer',CURSORSTATE);
        elseif ~online & strcmp(CURSORSTATE,'crosshair'),
            CURSORSTATE = 'arrow';
            set(gcf,'Pointer',CURSORSTATE);
        end
    else
        cp = get(gca,'CurrentPoint');
        NEWX=cp(1,1);
        if NEWX > MAXX
            NEWX = MAXX;
        end
        if NEWX < MINX
            NEWX = MINX;
        end
	NEWY = YVALUES(NEWX*XFACTOR + XOFFSET);

	set(XPOS_ACT,'String',num2str(NEWX));
        set(Y_ACT1,'String',num2str(NEWY));
        set(WITNESSLINE,'XData',NEWX*ones(size(YVERTICAL)));
    end

% END OF MOTION

% ********
% * DOWN *
% ********

elseif strcmp(ACT,'down'),
    set(gcf,'WindowButtonMotionFcn','conpres(''motion'',1)');
    set(gcf,'WindowButtonUpFcn','conpres(''up'')');

% END OF DOWN

% ******
% * UP *
% ******

elseif strcmp(ACT,'up'),
    set(gcf,'WindowButtonMotionFcn','conpres(''motion'',0)');
    set(gcf,'WindowButtonUpFcn','');

% END OF UP

% ************
% * SET DATA *
% ************

elseif strcmp(ACT,'setdata')
    cla
    CURSORSTATE = 'arrow';
    set(gcf,'Units','Normalized');
    nh=gca;
    if (PLOTTYPE == 1)		% Time domain
	YVALUES = DA_convert(DIGITALVALUES(STARTS:STOPS,1:NOB),1,AMPMAX/(2^(NOB-1)-1),0);
	XVALUES = (1:size(YVALUES,2));
	XFACTOR = 1;
	XOFFSET = 0;
	E1 = mean(YVALUES);
	E2 = std(YVALUES);
	E3 = 0;	E4 = 0;
	xlabel('Sample');				
	ylabel('Amplitude');				
	set(XPOS_TEXT,'String','Sample No.')
	set(Y_TEXT1,'String','Amplitude (A,V)');
	set(Y_TEXT2,'String','Max. Amplitude (A,V)'); 
	set(Y_TEXT3,'String','Min. Amplitude (A,V)');
	set(E_TEXT1,'String','Mean Amplitude (A,V)');
	set(E_TEXT2,'String','Std Dev. of Ampl.');
	set(E_TEXT3,'String','');	
	set(E_TEXT4,'String','');
    elseif (PLOTTYPE == 2)		% FFT
	YVALUES = abs(fft(DA_convert(DIGITALVALUES(STARTS:STOPS,1:NOB),1,AMPMAX/(2^(NOB-1)-1),0)));
	XVALUES = 2*pi*(1:size(YVALUES,2))/size(YVALUES,2);
	XFACTOR = size(YVALUES,2)/(2*pi);
	XOFFSET = 0;
        E1 = mean(YVALUES);
	hyxibyxi = find(YVALUES==max(YVALUES));
	E2 = hyxibyxi(1)/XFACTOR - XOFFSET; E3 = 0; E4 = 0;
	ylabel('Power (dB)');				
	xlabel('Frequency');				
	set(XPOS_TEXT,'String','Norm. Frequency');
	set(Y_TEXT1,'String','Power');
	set(Y_TEXT2,'String','Max. Power');
	set(Y_TEXT3,'String','Min. Power'); 
	set(E_TEXT1,'String','Mean Value');
	set(E_TEXT2,'String','Max Value Position');
	set(E_TEXT3,'String','');
	set(E_TEXT4,'String','');
    elseif (PLOTTYPE == 3)		% Log FFT
	YVALUES = 20*log10(abs(fft(DA_convert(DIGITALVALUES(STARTS:STOPS,1:NOB),1,AMPMAX/(2^(NOB-1)-1),0))));
	XVALUES = 2*pi*(1:size(YVALUES,2))/size(YVALUES,2);
	XFACTOR = size(YVALUES,2)/(2*pi);
	XOFFSET = 0;
	[E1, E2, E3, E4, ENOB] = distortion(YVALUES);
	ylabel('Power (dB)');				
	xlabel('Frequency');			
	set(XPOS_TEXT,'String','Norm. Frequency');
	set(Y_TEXT1,'String','Log. Power [dB]');
	set(Y_TEXT2,'String','Max. Power [dB]');
	set(Y_TEXT3,'String','Min. Power [dB]');
	set(E_TEXT1,'String','SFDR [dB]'); 
	set(E_TEXT2,'String','SNR [dB]');
	set(E_TEXT3,'String','THD [dB]');
	set(E_TEXT4,'String','SINAD [dB]');
    elseif (PLOTTYPE == 4)		% Histogram
	YVALUES = histogram(DIGITALVALUES(STARTS:STOPS,1:NOB),1,1);
	XVALUES = (-(size(YVALUES,2)-1)/2:(size(YVALUES,2)-1)/2);
	XFACTOR = 1;
	XOFFSET = (size(YVALUES,2)+1)/2;
	E1 = 0;	E2 = 0; E3 = 0; E4 = 0;
	ylabel('Probability of code');		
	xlabel('Code No');		
	set(XPOS_TEXT,'String','Code No.');
	set(Y_TEXT1,'String','Probability');
	set(Y_TEXT2,'String','Max. Probability');
	set(Y_TEXT3,'String','Min. Probability');
	set(E_TEXT1,'String','');
	set(E_TEXT2,'String','');
	set(E_TEXT3,'String','');
	set(E_TEXT4,'String','');
    elseif (PLOTTYPE == 5)		% DNL
	YVALUES = DNL(DIGITALVALUES(STARTS:STOPS,1:NOB),FUNCTIONNAMES(4*(FUNCTYPE-1)+1:4*FUNCTYPE),AMPMAX, AMPMIN);
	XVALUES = (-(size(YVALUES,2)-1)/2:(size(YVALUES,2)-1)/2);
	XFACTOR = 1;
	XOFFSET = (size(YVALUES,2)+1)/2;
	E1 = (max(YVALUES) - min(YVALUES));
	E2 = 0;	E3 = 0; E4 = 0;
	ylabel('DNL (LSB)');				
	xlabel('Code No');				
	set(XPOS_TEXT,'String','Code No.'); 
	set(Y_TEXT1,'String','DNL [LSB]');
	set(Y_TEXT2,'String','Max. DNL [LSB]');
	set(Y_TEXT3,'String','Min. DNL [LSB]');
	set(E_TEXT1,'String','DNL Value [LSB]');
	set(E_TEXT2,'String','');
	set(E_TEXT3,'String','');
	set(E_TEXT4,'String','');
    elseif (PLOTTYPE == 6)		% INL
	YVALUES = INL(DIGITALVALUES(STARTS:STOPS,1:NOB),FUNCTIONNAMES(4*(FUNCTYPE-1)+1:4*FUNCTYPE),AMPMAX, AMPMIN);
	XVALUES = (-(size(YVALUES,2)-1)/2:(size(YVALUES,2)-1)/2);
	XFACTOR = 1;
	XOFFSET = (size(YVALUES,2)+1)/2;
	E1 = YVALUES(size(YVALUES,2));
	E2 = 0;
	E3 = 0;
	E4 = 0;
	ylabel('INL (LSB)');				
	xlabel('Acc. Code No');				
	set(XPOS_TEXT,'String','Code No.'); 
	set(Y_TEXT1,'String','Acc. INL [LSB]');
	set(Y_TEXT2,'String','Max. INL [LSB]'); 
	set(Y_TEXT3,'String','Min. INL [LSB]');
	set(E_TEXT1,'String','INL Value [LSB]');
	set(E_TEXT2,'String','');
	set(E_TEXT3,'String','');
	set(E_TEXT4,'String','');
    end;


    MAXX = max(XVALUES);
    MINX = min(XVALUES);
    rangex = MAXX - MINX;
    MAXY = max(YVALUES); 
    MINY = min(YVALUES);

    set(nh,'NextPlot','add','DrawMode','fast','Box','on');
    set(nh,'Position',AXISP,'Color','k');
    set(nh,'XLim',[MINX MAXX]);
    set(nh,'Ylim',[MINY MAXY]);

    if (PLOTTYPE == 1) 
      [XXX,YYY] = stairs(XVALUES,YVALUES);
      FITLINE = plot(XXX,YYY);
    else
      FITLINE = plot(XVALUES,YVALUES);
    end; 

    YENDPTS = get(nh,'YLim');
    YVERTICAL = linspace(YENDPTS(1),YENDPTS(2))';
    xvert = ones(size(YVERTICAL)) * (MAXX+MINX)/2;
    NEWX  = xvert(1);
    obs = max(size(YVALUES));
    NEWY = YVALUES(NEWX*XFACTOR + XOFFSET);
    WITNESSLINE = plot(xvert,YVERTICAL,'w--','Erasemode','xor');
    set(WITNESSLINE,'ButtonDownFcn','conpres(''down'')');
    set(gcf,'Backingstore','off','WindowButtonMotionFcn','conpres(''motion'',0)');

    set(XPOS_ACT,'String',num2str(NEWX));
    set(Y_ACT1,'String',num2str(NEWY));
    set(Y_ACT2,'String',num2str(MAXY));
    set(Y_ACT3,'String',num2str(MINY));
    set(E_ACT1,'String',num2str(E1(1))); 
    set(E_ACT2,'String',num2str(E2));
    set(E_ACT3,'String',num2str(E3)); 
    set(E_ACT4,'String',num2str(E4));
    set(NOB_PRES,'String',num2str(NOB));
    set(ENOB_PRES,'String',num2str(ENOB));
    set(START_ACT,'String',num2str(STARTS));
    set(STOP_ACT,'String',num2str(STOPS));
    set(AMAX_ACT,'String',num2str(AMPMAX));
    set(AMIN_ACT,'String',num2str(AMPMIN));
    zoom on;
    conpres('edittext');

% END OF SET DATA

% *************
% * EDIT TEXT *
% *************

elseif strcmp(ACT,'edittext'),
    NEWX=str2num(get(XPOS_ACT,'String'));
    if NEWX > MAXX
        NEWX = MAXX;
        set(XPOS_ACT,'String',num2str(NEWX));
    end
    if NEWX < MINX
        NEWX = MINX;
        set(XPOS_ACT,'String',num2str(NEWX));
    end;
    NEWY = YVALUES(NEWX*XFACTOR + XOFFSET);
    set(Y_ACT1,'String',num2str(NEWY));
    set(WITNESSLINE,'XData',NEWX*ones(size(YVERTICAL)),'Ydata',YVERTICAL);

% END OF EDIT TEXT

% **************************
% * NEW START SAMPLE POINT *
% **************************

elseif strcmp(ACT,'newstart')

    STARTS = str2num(get(START_ACT,'String'));
    if (STARTS < 1) 
       STARTS = 1;
    elseif (STARTS >= STOPS)
       STARTS = STOPS-1;
    end;
    conpres('setdata');

% END OF NEW START SAMPLE POINT

% *************************
% * NEW STOP SAMPLE POINT *
% *************************

elseif strcmp(ACT,'newstop')
    STOPS = str2num(get(STOP_ACT,'String'));
    if (STOPS > size(DIGITALVALUES,1))
       STOPS = size(DIGITALVALUES,1);
    elseif (STOPS <= STARTS)
      STOPS = STARTS+1;
    end;
    conpres('setdata');
% END OF NEW STOP SAMPLE POINT

% *************************
% * NEW MAXIMUM AMPLITUDE *
% *************************

elseif strcmp(ACT,'newmax')
    AMPMAX = str2num(get(AMAX_ACT,'String'));
    if (AMPMAX <= AMPMIN)
       AMPMAX = AMPMIN;
    end;
    conpres('setdata');

% END OF NEW MAXIMUM AMPLITUDE

% ************************* 
% * NEW MINIMUM AMPLITUDE * 
% ************************* 

elseif strcmp(ACT,'newmin')
    AMPMIN = str2num(get(AMIN_ACT,'String'));
    if (AMPMIN >= AMPMAX)
      AMPMIN = AMPMAX;
    end;
    conpres('setdata');

% END OF NEW MINIMUN AMPLITUDE

% *******************
% * CHANGE PLOTTYPE *
% *******************

elseif strcmp(ACT,'change_plottype')
	PLOTTYPE = get(GRAPHTYPE_ACT,'Value');
	zoom out;
	zoom off;
	conpres('setdata');

% END CHANGE PLOTTYPE

% ********
% * DONE *
% ********

elseif strcmp(ACT,'done'),
	clear global XPOS_ACT Y_ACT1 Y_ACT2 Y_ACT3 WITNESSLINE YVERTICAL DATAFILE;
	clear global XPOS_TEXT Y_TEXT1 Y_TEXT2 Y_TEXT3 E_TEXT1 E_ACT1 E_TEXT2 E_ACT2;
	clear global E_TEXT3 E_ACT3 E_TEXT4 E_ACT4 FUNCTYPE_TEXT FUNCTYPE_ACT;
	clear global AMAX_ACT AMIN_ACT START_ACT STOP_ACT NOB_PRES NOB_TEXT;
	clear global FUNCTIONNAMES;
	clear global NEWX NEWY STRUCTURE CURSORSTATE YENDPTS MAXX MINX XOFFSET XFACTOR;
	clear global DIGITALVALUES XVALUES YVALUES AMPMAX AMPMIN STARTS STOPS;
	clear global GRAPHTYPE_ACT FILE_ACT FITLINE AXISP BCOLOR PLOTTYPE MAXY MINY FUNCTYPE;
	clear global E1 E2 E3 E4 NOB ENOB;	
	close;

% END DONE

end

% END OF FUNCTION



















