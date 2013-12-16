function digital_values = AD_convert(ftype, fampl, ...
    NOP, NOS, NOB, MSB_pos, scaling)

k_values = (0:(NOS-1));
cFunctionnames(1)
cFunctionnames(2)

ftype
bit_constallation = (1:NOB); % Bit constallation is used to
% control addressing to according bits in data.
  
if (MSB_pos == 1) 		    % If the MSB is in first position, 
  % bit_constallation should be reversed.
  bit_constallation = NOB + 1 - bit_constallation;
end;

if ~isstr(ftype)
  NOS = max(size(ftype));
  s_values = ftype;
  two_values = (0:(NOB-1));
  for i = 1:NOB 
    two_values(i) = 2^(two_values(i));
  end;
  two_values(NOB) = -two_values(NOB);
  % The digital code
  digital_code(NOB) = 0;
  treshold = 0.5;
  for i = 1:NOS
    negvalue = (s_values(i) < 0);
    sv = abs(s_values(i));
    if (sv <= treshold)
      digital_code(1:NOB) = zeros(size(digital_code));
    else
      for j = 1:(NOB-1)
	jj = NOB-j;
	if (sv > (two_values(jj)-treshold))
	  digital_code(bit_constallation(jj)) = 1;
	  sv = sv - two_values(jj);
	else
	  digital_code(bit_constallation(jj)) = 0;
	end;
	digital_code(bit_constallation(NOB)) = 0;
      end;
      if (negvalue) 
	digital_code = inv_dig(digital_code,1);
      end; 
    end;
    digital_values(i,1:NOB) = digital_code(1:NOB);
  end;
else

  if strcmp(ftype, cFunctionnames(1)) % 'sine'
    s_values = fampl*sin((2*pi/NOS)*k_values);  
   hej = 2 
  elseif strcmp(ftype,cFunctionnames(2)) % 'ramp'
    s_values = k_values*fampl/NOS;	
    
  elseif strcmp(ftype, cFunctionnames(3)) % 'sawt'
    s_values(1:(NOS-1)/2) = ...
	k_values(1:(NOS-1)/2)*2*fampl/NOS;
    s_values(NOS/2:NOS) = 2*fampl*(k_values(NOS/2:NOS)/NOS-1);
    
  elseif strcmp(ftype, cFunctionnames(5)) % 'sqR0'
    s_values(1:(NOS-1)/2) = fampl*ones(size(k_values(1:(NOS-1)/2)));
    s_values(NOS/2:NOS) = zeros(size(k_values(NOS/2:NOS)));
    
  elseif strcmp(ftype, cFunctionnames(6)) % 'sq+-'
    s_values(1:(NOS-1)/2) = fampl*ones(size(k_values(1:(NOS-1)/2)));
    s_values(NOS/2:NOS) = -fampl*ones(size(k_values(NOS/2:NOS)));
    
  elseif strcmp(ftype, cFunctionnames(4)) % 'tria'
    s_values(1:(NOS-1)/2) =  k_values(1:(NOS-1)/2)*2*fampl/NOS;
    s_values(NOS/2:NOS) = 2*fampl*(1-k_values(NOS/2:NOS)/NOS);   
    
  else
    digital_values = NaN;
  end;
  % s_values are scaled
  
  s_values = s_values/scaling;
  
  % two_values contains the weigths
  two_values = (0:(NOB-1));
  for i = 1:NOB 
    two_values(i) = 2^(two_values(i));
  end;
  two_values(NOB) = -two_values(NOB);
  
  % The digital code
  digital_code(NOB) = 0;
  
  treshold = 0.5;

  for i = 1:NOS
    negvalue = (s_values(i) < 0);
    sv = abs(s_values(i));
    
    if (sv <= treshold)
      digital_code(1:NOB) = zeros(size(digital_code));
    else
      for j = 1:(NOB-1)
	jj = NOB-j;
	if (sv > (two_values(jj)-treshold))
	  digital_code(bit_constallation(jj)) = 1;
	  sv = sv - two_values(jj);
	else
	  digital_code(bit_constallation(jj)) = 0;
	end;
	digital_code(bit_constallation(NOB)) = 0;
      end;
      if (negvalue) 
	digital_code = inv_dig(digital_code,1);
      end; 
    end;
    digital_values(i,1:NOB) = digital_code(1:NOB);
  end;
  
  % To create the correct number of periods.
  
  for i = 1:(NOP-1)
    digital_values((1:NOS)+NOS*i,1:NOB) = ...
	digital_values(1:NOS,1:NOB);
  end;
end;














