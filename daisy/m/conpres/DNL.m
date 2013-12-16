function [dnl, p] = DNL(analog_values,input_type,nob,vmax,vmin);

% DNL is implemented for Sine waves only.

resolution = max(size(analog_values));
no_of_bits = nob;

m = 2^(no_of_bits-1);
l = m-1;
N = 2*l+1; %(m+l = N);
delta = 1/l;
k = (1:l-1);
p(N) = 0;

hg = histogram(analog_values, nob);

if (input_type == cFunctionnames(1)) % Sine Wave

  medel = vmax + (vmin-vmax)*cos(pi*hg(N))/(cos(pi*hg(N)) + cos(pi*hg(1)));
  ampl = (vmax - vmin) / (cos(pi*hg(N)) + cos(pi*hg(1)));
  faktor = (1/l)*(vmax/ampl);
  offset = medel/ampl;

  p(m) = (asin(0.5*faktor - offset)-asin(-0.5*faktor-offset));              % Nollkoden...

  p(m+1:N-1) = (asin((k(:)+0.5)*faktor-offset)-asin((k(:)-0.5)*faktor-offset));

  p(N) = (asin((l+0.5)*faktor-offset)-asin((l-0.5)*faktor-offset));

  for i = 1:l
	p(m-i) = p(m+i);
  end;

% Will be implemented in later versions

	elseif (input_type == cFunctionname(2))
	elseif (input_type == cFunctionname(3))
	elseif (input_type == cFunctionname(4))
	elseif (input_type == cFunctionname(5))
	elseif (input_type == cFunctionname(6))
	elseif (input_type == cFunctionname(7))

end;

p = p / sum(p);

for i = 1:N 
  if (imag(p(i)) == 0) 
    dnl(i) = hg(i)/p(i) - 1;
  else
    dnl(i) = 0;
  end;
end;

 dnl(1) = 0;
 dnl(N) = 0;





