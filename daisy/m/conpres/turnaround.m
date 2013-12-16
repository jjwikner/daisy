function newdig = turnaround(digval);

[r,n] = size(digval);

newdig(r,n) = 0;


for i = 1:r
  for j = 1:n
    newdig(i,n-j+1) = digval(i,j);
  end;
end;

