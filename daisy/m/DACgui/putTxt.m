function handle = putTxt(pos, string, parent);

%
%
% (c) jjwikner, MERC
%

% default values

txtfldwid = 0.2;
txtfldhgt = 0.1;
bkgclr = [0.6 0.6 0.9];

if length(pos) > 2
  txtfldwid = pos(3);
end;
if length(pos) > 3
  txtfldhgt = pos(4);
end;

if (nargin == 3)
  handle = uicontrol('Parent',parent, ...
      'Units','normalized', ...
      'BackgroundColor',bkgclr, ...
      'ListboxTop',0, ...
      'Position',[pos(1) pos(2) txtfldwid txtfldhgt], ...
      'Style','text', ...
      'String',string, ...
      'HorizontalAlignment','center', ...
      'Tag',string);
else
  handle = uicontrol('Units','normalized', ...
      'BackgroundColor',bkgclr, ...
      'ListboxTop',0, ...
      'Position',[pos(1) pos(2) txtfldwid txtfldhgt], ...
      'Style','text', ...
      'HorizontalAlignment','center', ...
      'String',string, ...
      'Tag',string);
end;











