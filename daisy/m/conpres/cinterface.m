function cinterface(operation,extraop, extraop2);

% Global variables for buttons, popup, info text

global	XPOS_TEXT Y_TEXT1 Y_TEXT2 Y_TEXT3 E_TEXT1 E_TEXT2 E_TEXT3 E_TEXT4	...
	ENOB_TEXT FUNCTYPE_TEXT NOB_TEXT XPOS_ACT Y_ACT1 Y_ACT2 Y_ACT3;
global	E_ACT1 E_ACT2 E_ACT3 E_ACT4 FUNCTYPE_PRES AMAX_ACT AMIN_ACT START_ACT	...
	STOP_ACT PERLENGTH_ACT NOB_ACT ENOB_PRES GRAPHTYPE_ACT FILE_PRES;
global	FITLINE AXISP BCOLOR PLOTTYPE MAXY MINY FUNCTYPE WITNESSLINE YVERTICAL	...
	DATAFILE NEWX NEWY STRUCTURE CURSORSTATE YENDPTS MAXX MINX XOFFSET;
global	XFACTOR DIGITALVALUES XVALUES YVALUES AMPMAX AMPMIN STARTS STOPS	...
	E1 E2 E3 E4 NOB ENOB PERIODLENGTH DATAPATH WINDOWTYPE;
global  WINDOWTYPE_PRES MENUHANDLES PERL_PRES AMPL_PRES PHASE_PRES OFFSET_PRES	...
	ANALOGVALUES nh;

if (nargin < 1)			% checking if the call of cinterface program
    ACT = 'setup';		% is of startup mode
elseif isstr(operation)
    ACT = operation;		% else a special operation type is specified
end;

% ***********
% ** SETUP **
% ***********

if (strcmp(ACT,'setup'))

AMPMAX = 4;
AMPMIN = -4;
NOB = 4;
FUNCTYPE = 1;
[ANALOGVALUES,tresh,delta] = adconvert('sine',1024,1113,NOB,AMPMAX-1,AMPMIN+1,10,AMPMAX,0.6,AMPMAX/100);
STARTS = 1;
STOPS = max(size(ANALOGVALUES));
XFACTOR = 1;
XOFFSET = 0;
PERIODLENGTH = 128;
DATAFILE = '<None>';
PLOTTYPE = 1;
FUNCTYPE = 1;
YVALUES = ANALOGVALUES;
XVALUES = (1:max(size(YVALUES)));
E1 = mean(YVALUES);	E2 = std(YVALUES);
E3 = 0;			E4 = 0;
ENOB = 0;		
WINDOWTYPE = 1;

% Initialization of the figure window

ScreenSize= get(0,'ScreenSize');
figNo = figure(	'Name','ConPres version 2.0',	...
		'NumberTitle','off',		...
		'BackingStore','off',		...
		'Position',[ScreenSize(1:2)+0.06*ScreenSize(3:4) 0.88*ScreenSize(3:4)]);

% Initialization of the global graph functions

AXISP = [0.05 0.08 0.65 0.85];
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
perl = 200;
ampl = 4;
ephase = 0;
offset = 0;


% Initialization of presentation fields

top = 0.97;
bottom = 0.03;
labelColor = [0.8 0.8 0.8];
btnWid = 0.20;
btnHt = 0.03;
right = 0.97;
left = right - btnWid;
spacing = 0.02;
BCOLOR = [0.5 0.5 0.5];

% Initialization of the input frame

frmBorder = 0.02;
frmPosLeft = [	left-frmBorder		...
		bottom-frmBorder	...
		btnWid+2*frmBorder	...
		(top-bottom)+2*frmBorder];

h = uicontrol(	'Style','frame',	...
		'Units','normalized',	...	
		'Position',frmPosLeft,	...
		'BackgroundColor',BCOLOR);

[fieldpos Bgcolor Txtcolor] = cFielddefinitions(left, top, btnWid/2, btnHt, BCOLOR, 46);

FILE_TEXT	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(1,:),		...
			'BackgroundColor',BCOLOR,		...
			'ForegroundColor','white',		...
			'String','Data File');

FILE_PRES	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(2, :),		...
			'BackgroundColor','white',		...
			'String',DATAFILE,			...
			'Callback','cinterface(''newfile'',''D'');');

START_TEXT	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(3, :),		...
			'BackgroundColor',Bgcolor(3,:),		...
			'ForegroundColor',Txtcolor(3,:),	...
			'String','Start');

START_ACT	= uicontrol('Style','edit',			...
			'Units','normalized',			...
			'Position',fieldpos(5, :),		...
			'BackgroundColor',Bgcolor(5,:),		...
			'ForegroundColor',Txtcolor(5,:),	...
			'String', STARTS,			...
			'Callback','cinterface(''newstart'');');

STOP_TEXT	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(4, :),		...
			'BackgroundColor',Bgcolor(4,:),		...
			'ForegroundColor',Txtcolor(4,:),	...
			'String','Stop');

STOP_ACT	= uicontrol('Style','edit',			...
			'Units','normalized',			...
			'Position',fieldpos(6,:),		...
			'BackgroundColor',Bgcolor(6,:),		...
			'String',DATAFILE,			...
			'ForegroundColor',Txtcolor(6,:),	...
			'Callback','cinterface(''newstop'');');

AMAXPOS_TEXT	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(7, :),		...
			'BackgroundColor',Bgcolor(7,:),		...
			'ForegroundColor',Txtcolor(7,:),	...
			'String','Amax');

AMAX_ACT	= uicontrol('Style','edit',			...
			'Units','normalized',			...
			'Position',fieldpos(9,:),		...
			'BackgroundColor',Bgcolor(9,:),		...
			'ForegroundColor',Txtcolor(9,:),	...
			'String',AMPMAX,			...
			'Callback','cinterface(''newmax'');');

AMIN_TEXT	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(8,:),		...
			'BackgroundColor',Bgcolor(8,:),		...
			'ForegroundColor',Txtcolor(8,:),	...
			'String','Amin');

AMIN_ACT	= uicontrol('Style','edit',			...
			'Units','normalized',			...
			'Position',fieldpos(10,:),		...
			'BackgroundColor',Bgcolor(10,:),	...
			'ForegroundColor',Txtcolor(10,:),	...
			'String',AMPMIN,			...
			'Callback','cinterface(''newmin'');');

NOB_TEXT	= uicontrol('Style','text',			...
			'Position',fieldpos(11,:),		...
			'BackgroundColor',Bgcolor(11,:),	...
			'ForegroundColor',Txtcolor(11,:),	...
			'Units','normalized',			...
			'String','NOB');

NOB_ACT		= uicontrol('Style','edit',			...
			'Units','normalized',			...
			'Position',fieldpos(13,:),		...
			'BackgroundColor',Bgcolor(13,:),	...
			'ForegroundColor',Txtcolor(13,:),	...
			'String',NOB,				...
			'Callback','cinterface(''new_NOB'');');

ENOB_TEXT	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(12,:),		...
			'BackgroundColor',Bgcolor(12,:),	...
			'ForegroundColor',Txtcolor(12,:),	...
			'String','ENOB');

ENOB_PRES	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(14,:),		...
			'BackgroundColor',Bgcolor(14,:),	...
			'ForegroundColor',Txtcolor(14,:),	...
			'String',ENOB);

PERLENGTH_TEXT	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(15,:),		...
			'BackgroundColor',Bgcolor(15,:),	...
			'ForegroundColor',Txtcolor(15,:),	...
			'String','Periodlength');

PERLENGTH_ACT	= uicontrol('Style','edit',			...
			'Units','normalized',			...
			'Position',fieldpos(17,:),		...
			'BackgroundColor',Bgcolor(17,:),	...
			'ForegroundColor',Txtcolor(17,:),	...
			'String', PERIODLENGTH,			...
			'CallBack','cinterface(''new_periodlength'')');

FUNCTYPE_TEXT	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(16,:),		...
			'BackgroundColor',Bgcolor(16,:),	...
			'ForegroundColor',Txtcolor(16,:),	...
			'String','Function');

FUNCTYPE_PRES	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(18,:),		...
			'BackgroundColor',Bgcolor(18,:),	...
			'ForegroundColor',Txtcolor(18,:),	...
			'String',cFunctionnames(FUNCTYPE));

XPOS_TEXT	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(19,:),		...
			'BackgroundColor',Bgcolor(19,:),	...
			'ForegroundColor',Txtcolor(19,:),	...
			'String','X Value');

XPOS_ACT	= uicontrol('Style','edit',			...
			'Units','normalized',			...
			'Position',fieldpos(21,:),		...
			'BackgroundColor',Bgcolor(21,:),	...
			'ForegroundColor',Txtcolor(21,:),	...
			'String',num2str(NEWX),			...
			'CallBack','cinterface(''edittext'')');

Y_TEXT1		= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(20,:),		...
			'BackgroundColor',Bgcolor(20,:),	...
			'ForegroundColor',Txtcolor(20,:),	...
			'String','Y Value');

Y_ACT1		= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(22,:),		...
			'BackgroundColor',Bgcolor(22,:),	...
			'ForegroundColor',Txtcolor(22,:),	...
			'String',num2str(NEWY));

Y_TEXT2		= uicontrol('Style','text',			...
			'Units', 'normalized',			...
			'Position',fieldpos(23,:),		...
			'BackgroundColor',Bgcolor(23,:),	...
			'ForegroundColor',Txtcolor(23,:),	...
			'String','Y Max');

Y_ACT2		= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(25,:),		...
			'BackgroundColor',Bgcolor(25,:),	...
			'ForegroundColor',Txtcolor(25,:),	...
			'String',num2str(MINY));

Y_TEXT3		= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(24,:),		...
			'BackgroundColor',Bgcolor(24,:),	...
			'ForegroundColor',Txtcolor(24,:),	...
			'String','Y Min');

Y_ACT3		= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(26,:),		...
			'BackgroundColor',Bgcolor(26,:),	...
			'ForegroundColor',Txtcolor(26,:),	...
			'String',num2str(MINY));

E_TEXT1		= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(27,:),		...
			'BackgroundColor',Bgcolor(27,:),	...
			'ForegroundColor',Txtcolor(27,:),	...
			'String','Average');

E_ACT1		= uicontrol('Style','text',			...
			'Units','normalized',...
			'Position',fieldpos(29,:),		...
			'BackgroundColor',Bgcolor(29,:),	...
			'ForegroundColor',Txtcolor(29,:),	...
			'String',num2str(E1));

E_TEXT2		= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(28,:),		...
			'BackgroundColor',Bgcolor(28,:),	...
			'ForegroundColor',Txtcolor(28,:),	...
			'String','Std Dev');

E_ACT2		= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(30,:),		...
			'BackgroundColor',Bgcolor(30,:),	...
			'ForegroundColor',Txtcolor(30,:),	...
			'String',num2str(E2));

E_TEXT3		= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(31,:),		...
			'BackgroundColor',Bgcolor(31,:),	...
			'ForegroundColor',Txtcolor(31,:),	...
			'String','');

E_ACT3		= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(33,:),		...
			'BackgroundColor',Bgcolor(33,:),	...
			'ForegroundColor',Txtcolor(33,:),	...
			'String',num2str(E3));

E_TEXT4		= uicontrol('Style','text',			...
		        'Units','normalized',			...
			'Position',fieldpos(32,:),		...
			'BackgroundColor',Bgcolor(32,:),	...
			'ForegroundColor',Txtcolor(32,:),	...
			'String','');

E_ACT4		= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(34,:),		...
			'BackgroundColor',Bgcolor(34,:),	...
			'ForegroundColor',Txtcolor(34,:),	...
			'String',num2str(E4));

WINDOW_TEXT	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(35,:),		...
			'BackgroundColor',Bgcolor(35,:),	...
			'ForegroundColor',Txtcolor(35,:),	...
			'String','Window');

WINDOWTYPE_PRES	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(37,:),		...
			'BackgroundColor',Bgcolor(37,:),	...
			'ForegroundColor',Txtcolor(37,:),	...
			'String',cWindownames(WINDOWTYPE));

PERL_TEXT	= uicontrol('Style','text',			...
		        'Units','normalized',			...
			'Position',fieldpos(39,:),		...
			'BackgroundColor',Bgcolor(39,:),	...
			'ForegroundColor',Txtcolor(39,:),	...
			'String','Est Period');

PERL_PRES	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(41,:),		...
			'BackgroundColor',Bgcolor(41,:),	...
			'ForegroundColor',Txtcolor(41,:),	...
			'String',num2str(perl));

AMPL_TEXT	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(40,:),		...
			'BackgroundColor',Bgcolor(40,:),	...
			'ForegroundColor',Txtcolor(40,:),	...
			'String','Est Ampl');

AMPL_PRES	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(42,:),		...
			'BackgroundColor',Bgcolor(42,:),	...
			'ForegroundColor',Txtcolor(42,:),	...
			'String',num2str(ampl));

PHASE_TEXT	= uicontrol('Style','text',			...
		        'Units','normalized',			...
			'Position',fieldpos(43,:),		...
			'BackgroundColor',Bgcolor(43,:),	...
			'ForegroundColor',Txtcolor(43,:),	...
			'String','Est Phase');

PHASE_PRES	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(45,:),		...
			'BackgroundColor',Bgcolor(45,:),	...
			'ForegroundColor',Txtcolor(45,:),	...
			'String',num2str(ephase));

OFFSET_TEXT	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(44,:),		...
			'BackgroundColor',Bgcolor(44,:),	...
			'ForegroundColor',Txtcolor(44,:),	...
			'String','Est Offset');

OFFSET_PRES	= uicontrol('Style','text',			...
			'Units','normalized',			...
			'Position',fieldpos(46,:),		...
			'BackgroundColor',Bgcolor(46,:),	...
			'ForegroundColor',Txtcolor(46,:),	...
			'String',num2str(offset));

set(FUNCTYPE_PRES,'String',cFunctionnames(FUNCTYPE));
set(PERLENGTH_ACT, 'Value', PERIODLENGTH);

% Handles for the top Menu bar.

[labels, calls] = cMenuContents;
MENUHANDLES = makemenu(gcf, labels, calls);

cinterface('viewtype', 0);

% END OF SETUP AND USER INTERFACE SETUP

% ***************
% * INFORMATION *
% ***************

elseif (strcmp(ACT,'info')),
    ttlStr=get(gcf,'Name');
    hlpStr= chelpinfo('info');
    helpfun(ttlStr,hlpStr);                                     

% END OF INFORMATION

% ********
% * HELP *
% ********

elseif (strcmp(ACT,'help')),
    ttlStr=get(gcf,'Name');
    hlpStr= chelpinfo('help');
    helpfun(ttlStr,hlpStr);                                     

% END OF INFORMATION


% *********************
% * CREATE IDEAL DATA *
% *********************


elseif strcmp(ACT, 'createidealdata')
   if (extraop == 1)
	FUNCTYPE = 1;
	AMPMAX = 1;
	AMPMIN = -1;
	NOB = 10;
	[ANALOGVALUES,tresh,delta] = adconvert('sine',1024,1113,NOB,AMPMAX,AMPMIN,2067,AMPMAX,0.6,AMPMAX/100);
	STARTS = 1;
	STOPS = max(size(ANALOGVALUES));
	PERIODLENGTH = 128;
	DATAFILE = '<None>';
	WINDOWTYPE = 1;
	perl = PERIODLENGTH;
	ampl = AMPMAX;
	ephase = 0;
	offset = 0;
   elseif (extraop == 2)
	FUNCTYPE = 2;
	AMPMAX = 1;
	AMPMIN = -1;
	NOB = 10;
	[ANALOGVALUES,tresh,delta] = adconvert('sine',1024,1113,NOB,AMPMAX,AMPMIN,2067,AMPMAX,0.6,AMPMAX/100);
	STARTS = 1;
	STOPS = max(size(ANALOGVALUES));
	PERIODLENGTH = 128;
	DATAFILE = '<None>';
	WINDOWTYPE = 1;
	perl = PERIODLENGTH;
	ampl = AMPMAX;
	ephase = 0;
	offset = 0;
   elseif (extraop == 3)
	FUNCTYPE = 3;
	AMPMAX = 1;
	AMPMIN = -1;
	NOB = 10;
	[ANALOGVALUES,tresh,delta] = adconvert('sine',1024,1113,NOB,AMPMAX,AMPMIN,2067,AMPMAX,0.6,AMPMAX/100);
	STARTS = 1;
	STOPS = max(size(ANALOGVALUES));
	PERIODLENGTH = 128;
	DATAFILE = '<None>';
	WINDOWTYPE = 1;
	perl = PERIODLENGTH;
	ampl = AMPMAX;
	ephase = 0;
	offset = 0;
   elseif (extraop == 4)
	FUNCTYPE = 4;
	AMPMAX = 1;
	AMPMIN = -1;
	NOB = 10;
	[ANALOGVALUES,tresh,delta] = adconvert('sine',1024,1113,NOB,AMPMAX,AMPMIN,2067,AMPMAX,0.6,AMPMAX/100);
	STARTS = 1;
	STOPS = max(size(ANALOGVALUES));
	PERIODLENGTH = 128;
	DATAFILE = '<None>';
	WINDOWTYPE = 1;
	perl = PERIODLENGTH;
	ampl = AMPMAX;
	ephase = 0;
	offset = 0;
   elseif (extraop == 5)
	FUNCTYPE = 5;
	AMPMAX = 1;
	AMPMIN = -1;
	NOB = 10;
	[ANALOGVALUES,tresh,delta] = adconvert('sine',1024,1113,NOB,AMPMAX,AMPMIN,2067,AMPMAX,0.6,AMPMAX/100);
	STARTS = 1;
	STOPS = max(size(ANALOGVALUES));
	PERIODLENGTH = 128;
	DATAFILE = '<None>';
	WINDOWTYPE = 1;
	perl = PERIODLENGTH;
	ampl = AMPMAX;
	ephase = 0;
	offset = 0;
   elseif (extraop == 6)
	FUNCTYPE = 6;
	AMPMAX = 1;
	AMPMIN = -1;
	NOB = 10;
	[ANALOGVALUES,tresh,delta] = adconvert('sine',1024,1113,NOB,AMPMAX,AMPMIN,2067,AMPMAX,0.6,AMPMAX/100);
	STARTS = 1;
	STOPS = max(size(ANALOGVALUES));
	PERIODLENGTH = 128;
	DATAFILE = '<None>';
	WINDOWTYPE = 1;
	perl = PERIODLENGTH;
	ampl = AMPMAX;
	ephase = 0;
	offset = 0;
   end;
   cinterface('viewtype',1);

% *****************
% * LOAD NEW DATA *
% *****************


elseif strcmp(ACT, 'loadnewfile')

   if (extraop == 'S')			% Spice format
	[DF, FP] = uigetfile('*.tr*', 'Spice file name:',100, 100);
	if DF
		DATAFILE = DF;
		DATAPATH = FP;
		cinterface('newfile', 'S', DATAFILE);
	end;
   elseif (extraop == 'D')		% Digital Data
	[DF, FP] = uigetfile('*.csv', 'Digital data file name:',100, 100);
	if DF
		DATAFILE = DF;
		DATAPATH = FP;
		cinterface('newfile', 'D', DATAFILE);
	end;
   elseif (extraop == 'A')		% Analog Data
	[DF, FP] = uigetfile('*.ada', 'Analog data file name:',100, 100);
	if DF
		DATAFILE = DF;
		DATAPATH = FP;
		cinterface('newfile', 'A', DATAFILE);
	end;
    end;

  cinterface('edittext');

% *****************
% *    NEW FILE   *
% *****************

elseif strcmp(ACT,'newfile'),
    if (extraop == 'S')			% Spice format
      
    elseif (extraop == 'D') 		% Digital Data
      set(FILE_PRES,'String',DATAFILE);
      DIGITALVALUES = csvread(DATAFILE);
      NOB = size(DIGITALVALUES,2);
      STARTS = 1;
      STOPS = size(DIGITALVALUES,1);
      PLOTTYPE = 1;   
      set(GRAPHTYPE_ACT,'Value',PLOTTYPE);
      ANALOGVALUES = DA_convert(DIGITALVALUES, 1,AMPMAX/(2^NOB-1), 0);
    elseif (extraop == 'A') 		% Analog Data
      set(FILE_PRES,'String',DATAFILE);
      fid = fopen(DATAFILE,'r');
      ANALOGVALUES = fscanf(fid,' 	%10d');  
      STARTS = 1;
      STOPS = max(size(ANALOGVALUES,1));
      PLOTTYPE = 1;   
	set(GRAPHTYPE_ACT,'Value',PLOTTYPE);    
      end;
      cinterface('viewtype',0);
      % END OF LOAD NEW DATA
      
      % *****************
      % *    SAVE FILE   *
      % *****************
      
    elseif strcmp(ACT,'newfile'),
      if (extraop == 'A') 		% Analog Data
	
      elseif (extraop == 'D') 		% Digital Data
	
      end;    
      cinterface('edittext');
      % END OF LOAD NEW DATA
      
      % ***********************
      % * CHANGE PARAMETERS   *
      % ***********************
      
    elseif strcmp(ACT,'compute_parameters')
      datavalues = YVALUES;
      [perl ampl ephase offset] = cfuncparams(datavalues, FUNCTYPE);
      set(PERL_PRES,'String',num2str(perl));
      set(AMPL_PRES,'String',num2str(ampl));
      set(PHASE_PRES,'String',num2str(ephase));
      set(OFFSET_PRES,'String',num2str(offset));
      cinterface('viewtype', 0);
      % END OF CHANGE FUNCTYPE
      
      % ***********************
      % * CHANGE FUNCTYPE *
      % ***********************
      
    elseif strcmp(ACT,'change_functype')
      FUNCTYPE = extraop;
      set(FUNCTYPE_PRES,'String',cFunctionnames(FUNCTYPE));
      cinterface('viewtype', 0);
      % END OF CHANGE FUNCTYPE
      
      % ***********************
      % * CHANGE WINDOWTYPE   *
% ***********************

elseif strcmp(ACT,'change_window')
  WINDOWTYPE = extraop;
  set(WINDOWTYPE_PRES,'String',cWindownames(WINDOWTYPE));
  cinterface('viewtype', 0);
% END OF CHANGE FUNCTYPE

% ***********************
% * CHANGE PERIODLENGTH *
% ***********************

elseif strcmp(ACT,'new_periodlength')
  PERIODLENGTH = str2num(get(PERLENGTH_ACT,'String'));
  if PERIODLENGTH < 1
    PERIODLENGTH = 1;
  end;
  cinterface('viewtype', 0);
% END OF CHANGE PERIODLENGTH


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
    set(gcf,'WindowButtonMotionFcn','cinterface(''motion'',1)');
    set(gcf,'WindowButtonUpFcn','cinterface(''up'')');

% END OF DOWN

% ******
% * UP *
% ******

elseif strcmp(ACT,'up'),
    set(gcf,'WindowButtonMotionFcn','cinterface(''motion'',0)');
    set(gcf,'WindowButtonUpFcn','');

% END OF UP

% ************
% * SET DATA *
% ************

elseif strcmp(ACT,'viewtype');
    cla
    CURSORSTATE = 'arrow';
    set(gcf,'Units','Normalized');
    nh=gca;
    if (extraop == 0) 
       extraop = PLOTTYPE;
    end;

    KVALUES = cwindow(ANALOGVALUES(STARTS:STOPS), WINDOWTYPE, 7.18);

    if (extraop == 1)		% Time domain
	PLOTTYPE = 1;
	YVALUES = KVALUES;
	XVALUES = (1:max(size(YVALUES)));
	XFACTOR = 1;
	XOFFSET = 0;
	E1 = mean(YVALUES);
	E2 = std(YVALUES);
	E3 = 0;	E4 = 0;
	xlabel('Sample');				
	ylabel('Ampl.');				
	set(XPOS_TEXT,'String','Sample No.')
	set(Y_TEXT1,'String','Amplitude');
	set(Y_TEXT2,'String','Max Ampl'); 
	set(Y_TEXT3,'String','Min Ampl');
	set(E_TEXT1,'String','Average ');
	set(E_TEXT2,'String','Std Dev');
	set(E_TEXT3,'String','');	
	set(E_TEXT4,'String','');
    elseif (extraop == 2)		% FFT
	PLOTTYPE = 2;
	YVALUES = abs(fft(KVALUES));
	XVALUES = (1:max(size(YVALUES)))/max(size(YVALUES));
	XFACTOR = max(size(YVALUES))/(2*pi);
	XOFFSET = 0;
        E1 = mean(YVALUES);
	hyxibyxi = find(YVALUES==max(YVALUES));
	E2 = hyxibyxi(1)/XFACTOR - XOFFSET; E3 = 0; E4 = 0;
	ylabel('Power (dB)');				
	xlabel('Frequency');				
	set(XPOS_TEXT,'String','Norm Freq');
	set(Y_TEXT1,'String','Power');
	set(Y_TEXT2,'String','Max Pwr');
	set(Y_TEXT3,'String','Min Pwr'); 
	set(E_TEXT1,'String','Average ');
	set(E_TEXT2,'String','Max Pos');
	set(E_TEXT3,'String','');
	set(E_TEXT4,'String','');
    elseif (extraop == 3)		% Log FFT
	PLOTTYPE = 3;
	YVALUES = 20*log10(abs(fft(KVALUES)));
	YVALUES = YVALUES-max(YVALUES);
	XVALUES = (1:max(size(YVALUES)))/max(size(YVALUES));
	XFACTOR = max(size(YVALUES))/(2*pi);
	XOFFSET = 0;
	[E1, E2, E3, E4, ENOB] = distortion(YVALUES);
	ylabel('Power (dB)');				
	xlabel('Frequency');			
	set(XPOS_TEXT,'String','Norm Freq');
	set(Y_TEXT1,'String','Log Pwr [dB]');
	set(Y_TEXT2,'String','Max Pwr [dB]');
	set(Y_TEXT3,'String','Min Pwr [dB]');
	set(E_TEXT1,'String','SFDR [dB]'); 
	set(E_TEXT2,'String','SNR [dB]');
	set(E_TEXT3,'String','THD [dB]');
	set(E_TEXT4,'String','SINAD [dB]');
    elseif (extraop == 4)		% Histogram
	PLOTTYPE = 4;
	YVALUES = histogram(ANALOGVALUES, NOB);
	XVALUES = (-(max(size(YVALUES))-1)/2:(max(size(YVALUES))-1)/2);
	XFACTOR = 1;
	XOFFSET = (max(size(YVALUES))+1)/2;
	E1 = 0;	E2 = 0; E3 = 0; E4 = 0;
	ylabel('Probability of code');		
	xlabel('Code No');		
	set(XPOS_TEXT,'String','Code No');
	set(Y_TEXT1,'String','Prob');
	set(Y_TEXT2,'String','Max Prob');
	set(Y_TEXT3,'String','Min Prob');
	set(E_TEXT1,'String','');
	set(E_TEXT2,'String','');
	set(E_TEXT3,'String','');
	set(E_TEXT4,'String','');
    elseif (extraop == 5)		% DNL
	PLOTTYPE = 5;
	YVALUES = DNL(KVALUES, cFunctionnames(FUNCTYPE), NOB, AMPMAX, AMPMIN);
	XVALUES = (-(max(size(YVALUES))-1)/2:(max(size(YVALUES))-1)/2);
	XFACTOR = 1;
	XOFFSET = (max(size(YVALUES))+1)/2;
	E1 = (max(YVALUES) - min(YVALUES));
	E2 = 0;	E3 = 0; E4 = 0;
	ylabel('DNL (LSB)');				
	xlabel('Code No');				
	set(XPOS_TEXT,'String','Code No'); 
	set(Y_TEXT1,'String','DNL [LSB]');
	set(Y_TEXT2,'String','Max DNL [LSB]');
	set(Y_TEXT3,'String','Min DNL [LSB]');
	set(E_TEXT1,'String','DNL Value [LSB]');
	set(E_TEXT2,'String','');
	set(E_TEXT3,'String','');
	set(E_TEXT4,'String','');
    elseif (extraop == 6)		% INL
	PLOTTYPE = 6;
	YVALUES = INL(KVALUES, cFunctionnames(FUNCTYPE), NOB, AMPMAX, AMPMIN);
	XVALUES = (-(max(size(YVALUES))-1)/2:(max(size(YVALUES))-1)/2);
	XFACTOR = 1;
	XOFFSET = (max(size(YVALUES))+1)/2;
	E1 = YVALUES(size(YVALUES));
	E2 = 0; E3 = 0;	E4 = 0;
	ylabel('INL (LSB)');				
	xlabel('Acc. Code No');				
	set(XPOS_TEXT,'String','Code No'); 
	set(Y_TEXT1,'String','Acc. INL [LSB]');
	set(Y_TEXT2,'String','Max INL [LSB]'); 
	set(Y_TEXT3,'String','Min INL [LSB]');
	set(E_TEXT1,'String','INL Value [LSB]');
	set(E_TEXT2,'String','');
	set(E_TEXT3,'String','');
	set(E_TEXT4,'String','');
    elseif (extraop == 7);
        PLOTTYPE = 7;
	YVALUES = KVALUES;
	XVALUES = (1:PERIODLENGTH);
	XFACTOR = 1;
	XOFFSET = 0;
        J = ceil((STOPS-STARTS)/PERIODLENGTH);
        L = PERIODLENGTH * J;
        K = zeros(1,L);
        K(1:max(size(YVALUES))) = YVALUES(1:max(size(YVALUES)));
        YVALUES = K;
	E1 = mean(YVALUES);
	E2 = std(YVALUES);
	E3 = 0;	E4 = 0;
	xlabel('Sample');				
	ylabel('Ampl.');				
	set(XPOS_TEXT,'String','Sample No')
	set(Y_TEXT1,'String','Amplitude');
	set(Y_TEXT2,'String','Max Ampl'); 
	set(Y_TEXT3,'String','Min Ampl');
	set(E_TEXT1,'String','Average');
	set(E_TEXT2,'String','Std Dev');
	set(E_TEXT3,'String','');	
	set(E_TEXT4,'String','');
    elseif (extraop == 8);
	PLOTTYPE = 8;
	YVALUES = xcorr(KVALUES, KVALUES);
	XVALUES = (1:max(size(YVALUES)));
	XFACTOR = 1;
	XOFFSET = 0;
	E1 = mean(YVALUES);
	E2 = std(YVALUES);
	E3 = 0;	E4 = 0;
	xlabel('Periodic samples');				
	ylabel('Ampl.');				
	set(XPOS_TEXT,'String','Sample No.')
	set(Y_TEXT1,'String','Amplitude');
	set(Y_TEXT2,'String','Max Ampl'); 
	set(Y_TEXT3,'String','Min Ampl');
	set(E_TEXT1,'String','Average');
	set(E_TEXT2,'String','Std Dev');
	set(E_TEXT3,'String','');	
	set(E_TEXT4,'String','');
    elseif (extraop == 9)		% DNLdyn
	PLOTTYPE = 9;
	YVALUES = dnldyn(KVALUES, NOB, AMPMAX, AMPMIN);
	XVALUES = (-(max(size(YVALUES))-1)/2:(max(size(YVALUES))-1)/2);
	XFACTOR = 1;
	XOFFSET = (max(size(YVALUES))+1)/2;
	E1 = (max(YVALUES) - min(YVALUES));
	E2 = 0;	E3 = 0; E4 = 0;
	ylabel('DNL (LSB)');				
	xlabel('Code No');				
	set(XPOS_TEXT,'String','Code No'); 
	set(Y_TEXT1,'String','DNL [LSB]');
	set(Y_TEXT2,'String','Max DNL [LSB]');
	set(Y_TEXT3,'String','Min DNL [LSB]');
	set(E_TEXT1,'String','DNL Value [LSB]');
	set(E_TEXT2,'String','');
	set(E_TEXT3,'String','');
	set(E_TEXT4,'String','');
    elseif (extraop == 10)		% INLdyn
	PLOTTYPE = 10;
	YVALUES = dnldyn(KVALUES, NOB, AMPMAX, AMPMIN);
	YVALUES = inldyn(YVALUES);
	XVALUES = (-(max(size(YVALUES))-1)/2:(max(size(YVALUES))-1)/2);
	XFACTOR = 1;
	XOFFSET = (max(size(YVALUES))+1)/2;
	E1 = YVALUES(size(YVALUES,2));
	E2 = 0;	E3 = 0;	E4 = 0;
	ylabel('INL (LSB)');				
	xlabel('Acc. Code No');				
	set(XPOS_TEXT,'String','Code No'); 
	set(Y_TEXT1,'String','Acc. INL [LSB]');
	set(Y_TEXT2,'String','Max INL [LSB]'); 
	set(Y_TEXT3,'String','Min INL [LSB]');
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
 
    if (PLOTTYPE == 1)|(PLOTTYPE == 5)|(PLOTTYPE == 6)|(PLOTTYPE==9)|(PLOTTYPE == 10) 
      [XXX,YYY] = stairs(XVALUES,YVALUES);
      FITLINE = plot(XXX,YYY);
    elseif PLOTTYPE == 7
      hold on;
      for i = 0:ceil((STOPS-STARTS)/PERIODLENGTH)-1
	[XXX,YYY] = stairs(XVALUES, YVALUES(1+i*PERIODLENGTH:(i+1)*PERIODLENGTH));
	FITLINE = plot(XXX,YYY);
      end;     
      hold off;
    else
       FITLINE = plot(XVALUES,YVALUES);
    end; 
    YENDPTS = get(nh,'YLim'); 
    YVERTICAL = linspace(YENDPTS(1),YENDPTS(2))';
    xvert = ones(size(YVERTICAL)) * (MAXX+MINX)/2;
    NEWX  = xvert(1);
    obs = max(size(YVALUES));
    size(YVALUES)
    NEWY = YVALUES(NEWX*XFACTOR + XOFFSET);
    if PLOTTYPE == 7
      hold on;
      WITNESSLINE = plot(xvert,YVERTICAL,'w--','Erasemode','xor');
      hold off;
    else
      WITNESSLINE = plot(xvert,YVERTICAL,'w--','Erasemode','xor');
    end;
    set(WITNESSLINE,'ButtonDownFcn','cinterface(''down'')');
    set(gcf,'Backingstore','off','WindowButtonMotionFcn','cinterface(''motion'',0)');
    set(FILE_PRES,'String', DATAFILE);
    set(XPOS_ACT,'String',num2str(NEWX));
    set(Y_ACT1,'String',num2str(NEWY));
    set(Y_ACT2,'String',num2str(MAXY));
    set(Y_ACT3,'String',num2str(MINY));
    set(E_ACT1,'String',num2str(E1(1))); 
    set(E_ACT2,'String',num2str(E2));
    set(E_ACT3,'String',num2str(E3)); 
    set(E_ACT4,'String',num2str(E4));
    set(NOB_ACT,'String',num2str(NOB));
    set(ENOB_PRES,'String',num2str(ENOB));
    set(START_ACT,'String',num2str(STARTS));
    set(STOP_ACT,'String',num2str(STOPS));
    set(AMAX_ACT,'String',num2str(AMPMAX));
    set(AMIN_ACT,'String',num2str(AMPMIN));
    set(GRAPHTYPE_ACT,'Value',PLOTTYPE);
    cinterface('edittext');
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
    cinterface('viewtype', 0);

% END OF NEW START SAMPLE POINT

% *************************
% * NEW STOP SAMPLE POINT *
% *************************

elseif strcmp(ACT,'newstop')
    STOPS = str2num(get(STOP_ACT,'String'));
    if (STOPS > max(size(ANALOGVALUES)))
       STOPS = max(size(ANALOGVALUES))
    elseif (STOPS <= STARTS)
      STOPS = STARTS+1;
    end;
    cinterface('viewtype', 0);
% END OF NEW STOP SAMPLE POINT

% *************************
% * NEW MAXIMUM Ampl. *
% *************************

elseif strcmp(ACT,'newmax')
    AMPMAX = str2num(get(AMAX_ACT,'String'));
    if (AMPMAX <= AMPMIN)
       AMPMAX = AMPMIN;
    end;
    cinterface('viewtype', 0);
% END OF NEW MAXIMUM Ampl.

% **********************
% * NEW NUMBER OF BITS *
% **********************

elseif strcmp(ACT,'new_NOB')
    NOB = str2num(get(NOB_ACT,'String'));
    if (NOB < 1) 
	NOB = 1;
    end;
    cinterface('viewtype', 0);
% END OF NEW MAXIMUM Ampl.

% ************************* 
% * NEW MINIMUM Ampl. * 
% ************************* 

elseif strcmp(ACT,'newmin')
    AMPMIN = str2num(get(AMIN_ACT,'String'));
    if (AMPMIN >= AMPMAX)
      AMPMIN = AMPMAX;
    end;
    cinterface('viewtype', 0);
% END OF NEW MINIMUN Ampl.

% ***********
% * ZOOMING *
% ***********

elseif strcmp(ACT,'zooming')
   if strcmp(extraop,'toggle')
      set(gcf,'Units','pixels');
      zoom;
   elseif strcmp(extraop,'zoomout')
      zoom out;
  end;
  cinterface('viewtype', 0);
% END OF NEW MINIMUN Ampl.


% ********
% * DONE *
% ********

elseif strcmp(ACT,'done'),
	clear global	XPOS_TEXT Y_TEXT1 Y_TEXT2 Y_TEXT3 E_TEXT1 E_TEXT2 E_TEXT3 E_TEXT4	...
			ENOB_TEXT FUNCTYPE_TEXT NOB_TEXT XPOS_ACT Y_ACT1 Y_ACT2 Y_ACT3;
	clear global	E_ACT1 E_ACT2 E_ACT3 E_ACT4 FUNCTYPE_PRES AMAX_ACT AMIN_ACT START_ACT	...
			STOP_ACT PERLENGTH_ACT NOB_PRES ENOB_PRES GRAPHTYPE_ACT FILE_PRES;
	clear global	FITLINE AXISP BCOLOR PLOTTYPE MAXY MINY FUNCTYPE WITNESSLINE YVERTICAL	...
			DATAFILE NEWX NEWY STRUCTURE CURSORSTATE YENDPTS MAXX MINX XOFFSET;
	clear global	XFACTOR DIGITALVALUES XVALUES YVALUES AMPMAX AMPMIN STARTS STOPS	...
			E1 E2 E3 E4 NOB ENOB PERIODLENGTH DATAPATH;
	close;
% END DONE

end

% END OF FUNCTION




















