function idig = inv_dig(dig,MSBpos)

% Antag tillfalligt att den mest signifikanta biten ligger i den forsta sista biten...

n = size(dig,2);
idig = 1-dig;
carry = 1;

for j = 1:n
  if (MSBpos == 1) 
     jj = n-j+1;
  else 
     jj = j;
  end;
  idig(jj) = idig(jj) + carry;
  if (idig(jj) > 1) 
    idig(jj) = 0;
    carry = 1;
  else
    carry = 0;
  end;
end;
 
