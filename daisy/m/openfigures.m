function result =  daisyOpenFigs(M, direction);

%
% result =  openfigures(M);
%
% M         = number of windows
% direction = row or column alignment
%
% This function arranges the windows to cover the whole
% screen to avoid searching for windows.
%
% (c) JJW, MERC, Ericsson Microelectronics AB, 000107

if (nargin == 1)
    direction = 'rows';
end;

y = ceil(sqrt(M))+1;
x = ceil(sqrt(M))-1;
mvals = inf*[1 1];
for width = x:y
    for height = width:y
        if (width*height >= M)
            mvalsp = [(height-width) (height*width - M)];
            if (mvalsp(1)<mvals(1))&(mvalsp(2)<mvals(2))
                correct = [width height];
                mvals = mvalsp;
            end;
        end;
    end;
end;
cwidth = correct(2);
cheight = correct(1);


xlimits = [0.0 0.94];
ylimits = [0.01 1.00];
sorgx = 0.01;
sorgy = 0.08;

if strcmp(direction, 'rows')
    % ROW
    deltax = (max(xlimits)-min(xlimits))/cwidth;
    deltay = (max(ylimits)-min(ylimits))/cheight;
    for row = 1:cheight
        for col = 1:cwidth
            m = col + (row-1)*cwidth;
            if (m <= M)
                l(m) = figure(m);
                x = min(xlimits) + (col-1)*deltax;
                y = max(ylimits) - (row)*deltay;
                set(l(m),'Units', 'Normalized', ...
                         'Position', [x y deltax-sorgx deltay-sorgy]);
            end;
        end;
    end;
else
    % COL
    deltax = (max(xlimits)-min(xlimits))/cheight;
    deltay = (max(ylimits)-min(ylimits))/cwidth;
    
    for col = 1:cheight
        for row = 1:cwidth
            m = row + (col-1)*cwidth;
            if (m <= M)
                l(m) = figure(m);
                x = min(xlimits) + (col-1)*deltax;
                y = max(ylimits) - (row)*deltay;
                set(l(m),'Units','Normalized', ...
                         'Position', [x y deltax-sorgx deltay-sorgy]);
            end;
        end;
    end;
end;
