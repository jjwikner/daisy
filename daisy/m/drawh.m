function [xnew, ynew]=drawh(x,y,size)

%
%
%

xnew=[x+size x-size x+size x-size];
ynew=[y+size y+size y-size y-size];

line(xnew(1:2),ynew(1:2));
line(xnew(3:4),ynew(3:4));
line([x x],[y+size y-size]);

