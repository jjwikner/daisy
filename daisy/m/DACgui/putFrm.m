function handle = putFrm(pos, string, parent);

%
%
% (c) jjwikner, MERC
%

frmfldwid = 0.2;
frmfldhgt = 0.06;
bkgclr = [.8 .8 .8];
headercolor = [0 0 0];
headerweight = 'bold';
headersize = 12;

if length(pos) > 2
  frmfldwid = pos(3);
end;
if length(pos) > 3
  frmfldhgt = pos(4);
end;

handle = uicontrol('Units','normalized', ...
    'Units','normalized', ...
    'BackgroundColor', bkgclr, ...
    'ListboxTop',0, ...
    'Position', [pos(1) pos(2) frmfldwid frmfldhgt], ...
    'Style','frame');


txt = putTxt([pos(1) pos(2)+frmfldhgt frmfldwid .05],string,parent);
set(txt,'BackgroundColor', bkgclr);    
set(txt,'FontSize',headersize);
set(txt,'FontWeight',headerweight);
set(txt,'FontName','Times');
