function Fig = inputbox(DlgName, PromptString, AcceptCallback, DefEditStr)
no = nargout;

% Check if figure is already on screen
[flag,fig] = figflag(DlgName);
if  flag,
   % No need to create new dialog
   if no,
      Fig = fig;
   end
   return
end

TextSize	= size(PromptString,2);
Numberoffields	= size(PromptString,1);

% Define default position
set(0,'Unit','pixels');
ScreenSize = get(0, 'ScreenSize'); % [1 1 1152 900] 
ScreenWidth = ScreenSize(3);
ScreenHeight = ScreenSize(4);
WidthPadding = 20;	TextCellWidth  = 150;
HeightPadding = 5;	TextCellHeight = 20;
WindowWidth = Numberoffields*TextCellWidth + (Numberoffields+1)*WidthPadding;
WindowHeight = 3*TextCellHeight + 5*HeightPadding;

BtnWidth = 60;
BtnHeight = 18;
BtnSpace = 8;

Position = round([	ScreenWidth/2-WindowWidth/2	...
			ScreenHeight/2-WindowHeight/2	...
			WindowWidth			...
			WindowHeight ]);

% Make the figure
DefUIBgColor = get(0,'DefaultUIControlBackgroundColor');

fig = figure(	'NumberTitle','off',	...
		'Name',DlgName,		...
		'Units','pixels',	...
		'Position', Position,	...
		'NextPlot','new',	...
		'MenuBar','none',	...
		'Color',DefUIBgColor,	...
		'Visible','off');

% Make the 2 frame uicontrols

UIPos = [2 2 WindowWidth-2 BtnHeight+2*BtnSpace-3];
BtnFrame =	uicontrol(fig,'Style','frame','Position',UIPos);
	
UIPos = [2 BtnHeight+2*BtnSpace-1		...
	WindowWidth-2 WindowHeight-(BtnHeight+2*BtnSpace+1)-1];
TextFrame =	uicontrol(fig,'Style','frame','Position',UIPos);


for i = 1:Numberoffields

Infofield(i) = uicontrol(fig,'Style', 'text',		...
		'String',mat2str(PromptString(i,:)),	...
		'BackgroundColor',DefUIBgColor,		...
		'Position',[WidthPadding+(i-1)*(TextCellWidth+WidthPadding) ...
				(TextCellHeight+BtnHeight+2*BtnSpace+1)+3 TextCellWidth TextCellHeight],	...
		'HorizontalAlignment','center');

Textfield(i) = uicontrol(fig,'Style', 'edit',							...
		'String',DefEditStr,								...
		'BackgroundColor','white',							...
		'Position',[WidthPadding+(i-1)*(TextCellWidth+WidthPadding)					...
				(BtnHeight+2*BtnSpace+1)+3 TextCellWidth TextCellHeight],	...
		'HorizontalAlignment','center');

end;
% Make the pushbuttons

OKBtn =	uicontrol(	fig,'Style','push',		...
			'String','OK',			...
			'Callback',AcceptCallback,	...
			'BackgroundColor','blue',	...
			'Position',[BtnSpace BtnSpace BtnWidth BtnHeight]);

CancelBtn = uicontrol(	fig,'Style','push',		...
			'String','Cancel',		...
			'Callback','delete(gcf)',	...
			'BackgroundColor', 'red',	...
			'Position',[WindowWidth-BtnWidth-BtnSpace BtnSpace BtnWidth BtnHeight]);

% Finally, make all the uicontrols normalized and the figure visible
set(get(fig,'Children'),'Unit','norm');
set(fig,'Visible','on');
set(fig,'Resize','off');

if no,
   Fig = fig;
end

% end inputdlg
