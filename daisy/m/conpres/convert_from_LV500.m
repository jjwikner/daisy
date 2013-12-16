function digital_data = convert_from_LV500(filename,blocklength,starting_points,control_bits, control_pattern, patternname);

% CONVERT_FROM_LV500	convert_from_LV500(filename, blocklength, starting_points, control_bits, control_pattern, patternname);
%
%			The function takes as input:
%
%			filename	: A LV500 result data file
%			blocklength	: The Length of the datablocks to extract.
%			starting_points	: Starting points of the different blocks.
%			control_bits	: Special control bits which control interresting blocks.
%			control_pattern	: The control bits pattern
%			patternname	: The name of the LV500 pattern
%
% EXAMPLE:		convert_from_LV500('adtnum2',10,[6 16],[4 5],[1 0], '"test_of_ad:s"');
%			 
if ~isstr(filename)
  error('FILENAME must be a string.');
end;

if ~isstr(patternname)
  error('PATTERNNAME must be a string.');
end;

fid = fopen(filename,'r');

F = fread(fid);

[rows cols] = size(F);

pattern = [10 112 97 116 116 101 114 110 32 10];	% Pattern = 'pattern'
pattern_found = 0;
sp = size(pattern,2)-1;
for i = 1:rows-sp 
  if (pattern_found == 0) 
    pattern_found = all(F(i:i+sp) == pattern');
    line_number = i+sp+1;   
  end;
end;

ddata = F(line_number:rows);
	
[rows cols] = size(ddata);

j = 1;
k = 1;

sp = size(patternname,2)-1;
no_numb_in_the_name = 0;

correct_row_found = 0;
for i = 1:rows-sp

  if (correct_row_found == 0)
    correct_row_found = all(ddata(i:i+sp) == abs(patternname)');
    if correct_row_found == 1
      no_numb_in_the_name = i+sp;
   end;
  end;

  if (correct_row_found == 1) & (i > no_numb_in_the_name)
    if (ddata(i) == 48)
      ddata2(k,j) = 0;
      j = j + 1;
    end;
    if (ddata(i) == 49) 
      ddata2(k,j) = 1;
      j = j + 1;
    end;
  end;

  if (ddata(i) == 10)
    k = k + 1;
    j = 1;
    correct_row_found = 0;
    no_numb_in_the_name = 0;
  end;

end;

[rows cols] = size(ddata2);	% Size of the input
ndbs = size(starting_points,2);	% Number of datablocks
ncbs = size(control_bits,2);	% Number of control bits
allno = 0;			% Number of "allowed" datablocks
for i = 1:rows
    if all(ddata2(i,control_bits)==control_pattern) == 1
       for j = 1:ndbs
         digital_data(j+ndbs*allno,1:blocklength) = ddata2(i,starting_points(j):starting_points(j)+blocklength-1);
       end
       allno = allno + 1;
    end;
end;










