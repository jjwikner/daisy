function handle = putAxs(pos,parent);

%
%
% (c) jjwikner, MERC
%

axsfldwid = 0.1;
axsfldhgt = 0.1;


if length(pos) > 2
  axsfldwid = pos(3);
end;
if length(pos) > 3
  axsfldhgt = pos(4);
end;


handle = axes('Parent',parent,'position', [pos(1) pos(2) axsfldwid axsfldhgt]);
