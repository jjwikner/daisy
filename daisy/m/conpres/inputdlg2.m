function Fig = inputdlg2(PromptString,DlgName,AcceptCallback,DefEditStr)
%INPUTDLG Creates and manages an input dialog box.
%       FIG = INPUTDLG(PromptString,DlgName,OKCallback,DefEditStr) creates an
%       input dialog box with name DlgName and prompt PromptString.  There
%       is a single line editable uicontrol for the user to type into and two
%       pushbuttons, OK and Cancel.  If OK is pressed, then OKCallback
%       is called.  If Cancel is pressed, the dialog is destroyed.
%
%       Note: The last part of a good OKCallback should either
%        1) Delete the input dialog figure
%        2) Leave the input dialog open, but pop an errordlg because of bad input
%
%       Example 1
%       inputdlg
%       A do nothing input dialog.
%
%       Example 2
%       OKCallback = 'disp(get(findobj(gcf,''Style'',''edit''),''String''));';
%       fig = inputdlg('My prompt','My input dialog', ...
%                      [OKCallback 'delete(gcf)'],'foobar')
%       This input dialog simply echos the edit string to the command line.
%
%       See also ERRORDLG, HELPDLG, WARNDLG, QUESTDLG

%	Author(s): A. Potvin, 10-17-94
%	Copyright (c) 1984-94 by The MathWorks, Inc.
%	$Revision: 1.5 $  $Date: 1995/01/17 16:04:01 $

ni = nargin;
no = nargout;
if ni<4,
   DefEditStr = '[ ]';
   if ni<3,
      AcceptCallback = '';
      if ni<2,
         DlgName = 'Input Dialog';
         if ni<1,
            PromptString = 'This is an example PromptString.';
         end
      end
   end
end

% Check if figure is already on screen
[flag,fig] = figflag(DlgName);
if  flag,
   % No need to create new dialog
   if no,
      Fig = fig;
   end
   return
end

TextSize = size(PromptString);

% Get layout parameters
layout
mLineHeight = mLineHeight+5;
BWH = [mStdButtonWidth mStdButtonHeight];

% Define default position
ScreenUnits = get(0,'Units');
set(0,'Unit','pixels');
ScreenPos = get(0,'ScreenSize');
set(0,'Unit',ScreenUnits);
mCharacterWidth = 7;
FigWH = fliplr(TextSize).*[mCharacterWidth mLineHeight] ...
        +[2*(mEdgeToFrame+mFrameToText) 3*mLineHeight+2*BWH(2)];
MinFigW = 2*(BWH(1)+mFrameToText) + ...
          2*(mEdgeToFrame+mFrameToText);
FigWH(1) = max([FigWH(1) MinFigW]);
FigWH = min(FigWH,ScreenPos(3:4)-50);
Position = [(ScreenPos(3:4)-FigWH)/2 FigWH];

% Make the figure
DefUIBgColor = get(0,'DefaultUIControlBackgroundColor');
fig = figure('NumberTitle','off','Name',DlgName,'Units','pixels', ...
 'Position',Position,'NextPlot','new','MenuBar','none', ...
 'Color',DefUIBgColor,'Visible','off');

% Make the 2 frame uicontrols
UIPos = mEdgeToFrame*[1 1 -2 -2] + [0 0 FigWH(1) BWH(2)+mLineHeight]
uicontrol(fig,'Style','frame','Position',UIPos);
UIPos = [UIPos(1:3)+[0 UIPos(4)+mEdgeToFrame 0] FigWH(2)-UIPos(4)-2*mEdgeToFrame];
uicontrol(fig,'Style','frame','Position',UIPos);

% Make the text uicontrol(s) and edit uicontrol
UIPos = [mEdgeToFrame+mFrameToText FigWH(2)-mLineHeight ...
 FigWH(1)-2*mEdgeToFrame-2*mFrameToText mLineHeight];
for i=1:size(PromptString,1),
   UIPos = UIPos - [0 mLineHeight 0 0];
   uicontrol(fig,'Style','text','String',PromptString(i,:),'Position',UIPos, ...
    'HorizontalAlignment','left')
end
uicontrol(fig,'Style','edit','String',DefEditStr,'BackgroundColor','white', ...
 'Position',[UIPos(1:3)-[0 BWH(2) 0] BWH(2)],'HorizontalAlignment','left')

% Make the pushbuttons
Hspace = (FigWH(1)-2*BWH(1))/3;
uicontrol(fig,'Style','push','String','OK','Callback',AcceptCallback, ...
 'Position',[Hspace mLineHeight/2 BWH]);
uicontrol(fig,'Style','push','String','Cancel','Callback','delete(gcf)', ...
 'Position',[2*Hspace+BWH(1) mLineHeight/2 BWH]);

% Finally, make all the uicontrols normalized and the figure visible
set(get(fig,'Children'),'Unit','norm');
set(fig,'Visible','on')

if no,
   Fig = fig;
end

% end inputdlg
