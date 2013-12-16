function [kalle]  = drawDEMtree(arguments)
global NOB layers axs nobh layh

if (nargin == 0)
  close all;
  demDACgui;
  NOB = 14;
  layers = 5;
  arguments = 'update layers';
end;

if (strcmp(arguments,'update NOB'))
  disp('jahadu, det tycker du...');

elseif (strcmp(arguments,'update layers'))
 
  layers = str2num(get(layh,'string'));
%  figure(1);
  cla;
  hold on;
  if mod(layers+1,2)
    line([0 2],[0 0]);
    x=[0];y=[0];
    noh=layers/2;
    size=1;
  else
    line([0 0],[-1 0])
    x=[-1 1];y=[0 0];
    line(x,y);
    size=.5;
    noh=(layers-1)/2;
  end
  
  for i=1:noh
    a=[];b=[];
    for j=1:length(x)
      [xnew, ynew]=drawh(x(j),y(j),size);
      a=[a xnew];b=[b ynew];
    end
    size=size/2;
    x=a;y=b;
  end
  
  for i=1:length(x)
    drawDAC(x(i),y(i),size);
  end;
  axis equal;
  axis off;
  hold off;
  
end;

