function [digval, tresh, delta] = adconvert(ftype,nos,sfreq,nob,ampmax,ampmin,ffreq,fampl,fphase,foffset);

K = 2^nob;

delta = (ampmax-ampmin)/(K - 1);

tresh = linspace(ampmin, ampmax, K);

if isstr(ftype)
  if strcmp(ftype,'sine')
    fvals = foffset + fampl*sin(fphase+(2*pi*ffreq/sfreq)*(1:nos));
  elseif strcmp(ftype,'ramp')

  elseif strcmp(ftype,'pulse')

  elseif strcmp(ftype,'sawtooth')

  elseif strcmp(ftype,'triangular')

  elseif strcmp(ftype,'???')

  end;

else
 
  fvals = ftype;

end;
 
digval = ones(size(fvals));

for i = 2:(K-1)
  L = find(fvals >= tresh(i));
  digval(L) = digval(L) + 1;
end;

digval = digval - (K/2);
