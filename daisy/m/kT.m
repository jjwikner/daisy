 function  [outv] = kT(T);
if (nargin == 0)
  T = 300;
end;
outv = 1.38e-23 * T;
