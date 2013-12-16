function inl = INL(analog_values,input_type,nob, vmax,vmin)


dnl = DNL(analog_values,input_type,nob, vmax,vmin);


N = size(dnl,2);

for i = 1:N
  inl(i) = sum(dnl(1:i));
end;

% approximera till en linje...
ij = (-(N-1)/2:(N-1)/2);

at = ij*inl'-sum(ij)*sum(inl)/N;
an = ij*ij'-sum(ij)*sum(ij)/N;
a = at/an;

bt = ((ij*ij')*sum(inl)-sum(i)*(ij*inl'))/N;
bn = ij*ij' - sum(ij)*sum(ij)/N;
b = bt/bn;

inl(:) = inl(:) - (a*ij(:) + b);

