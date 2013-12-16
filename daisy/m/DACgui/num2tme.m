function time = num2tme(number);
%
% num2tme(number)
%
% A number is expressed in a string as time.
% 9.6 corresponds to 9.36. The result is 
% truncated to minutes.
%
% Unfortunately, we cannot use vectors. 
% 
% (c) jjwikner, MERC, 2000
%

if length(number) > 1
  warning('No vectors !');
else
  golvet = floor(number);
  if (golvet<0) 
    golvet = golvet + 1;
  end;
  fraction = 0.01*round(100*(number-golvet));
  number = golvet+fraction*0.6;
  time     = num2str(number,'%2.2f');
end;
