function number = tme2num(time);
%
% tme2num(time)
%
% A time is expressed in a number.
% 9.36 corresponds to 9.6. 
%
% Unfortunately, we cannot use vectors. 
% 
% (c) jjwikner, MERC, 2000


if (size(time,1)>1) & (size(time,2)>1)
  warning('No vectors!');
else
  if (strcmp('',time))
    number = 0.00;
  else
    special = str2num(time);
    fraction = special-floor(special);
    number = floor(special) + fraction*100/60;
  end;
end;
