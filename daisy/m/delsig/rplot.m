function [xr,yr]=rplot(x,y,yrange,fmt)
%function rplot(x,y,yrange,fmt)
%plot y vs x, restricted to a certain range.
n = length(x);
if nargin <4
    fmt = '-';
end

%deal with column vectors only
if(size(x,2)~=1)
    x=x';
end
if(size(y,2)~=1)
    y=y';
end

where = (y>yrange(2)) - (y<yrange(1));
above = 1;
inside = 0;
below = -1;

xr=[];	 yr=[];	 i=1;
while i<=n
    tmp = find(where(i:n)~=where(i));
    if isempty(tmp)
	tmp=n+1;
    else
	tmp = tmp(1)+i-1;
    end;
    if where(i)==inside		% copy the data over
	xr = [xr; x(i:tmp-1)];
	yr = [yr; y(i:tmp-1)];
	if( tmp<=n )		% add a point to the data
	    if(where(tmp) == above)
		xr = [xr; x(tmp) + (x(tmp)-x(tmp-1))/(y(tmp)-y(tmp-1))*(yrange(2)-y(tmp))];
		yr = [yr; yrange(2)];
	    else
		xr = [xr; x(tmp) + (x(tmp)-x(tmp-1))/(y(tmp)-y(tmp-1))*(yrange(1)-y(tmp))];
		yr = [yr; yrange(1)];
	    end
	end
    elseif where(i)==above
	if( tmp<=n )		% add a point to the data
	    xr = [xr; x(tmp) + (x(tmp)-x(tmp-1))/(y(tmp)-y(tmp-1))*(yrange(2)-y(tmp))];
	    yr = [yr; yrange(2)];
	    if( where(tmp)==below )	% add another point to the data
		xr = [xr; x(tmp) + (x(tmp)-x(tmp-1))/(y(tmp)-y(tmp-1))*(yrange(1)-y(tmp))];
		yr = [yr; yrange(1)];
	    end
	end
    else
	if( tmp<=n )
	    xr = [xr; x(tmp) + (x(tmp)-x(tmp-1))/(y(tmp)-y(tmp-1))*(yrange(1)-y(tmp))];
	    yr = [yr; yrange(1)];
	    if( where(tmp)==below )	% add another point to the data
		xr = [xr; x(tmp) + (x(tmp)-x(tmp-1))/(y(tmp)-y(tmp-1))*(yrange(2)-y(tmp))];
		yr = [yr; yrange(2)];
	    end
	end
    end
    i = tmp;
end
plot(xr,yr,fmt);
axis([min(x) max(x) yrange(1) yrange(2)]);
