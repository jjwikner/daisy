function LV500_format = convert_to_LV500(filename, pattern_string, digital_data);

[rows cols] = size(digital_data);
count = 1;
every_second = 1;
		
if ~isstr(filename)
  error('FILENAME must be a string.');
end;

mellanslag = -1;

% Tillfallig struktur, skall goras mer allman senare...

digital_data
for i = 1:rows

 LV500_format(i,1:3) = [1 0 0];  % (Klocka och extra styrsignaler)
 LV500_format(i,4) = mellanslag;

 if (every_second == 1)
   LV500_format(i,5:6) = [1 1];  % (Dataready signaler, i detta fall insignal)
   LV500_format(i,7) = mellanslag;
   LV500_format(i,8:8+cols-1) = digital_data(i,1:cols);
   LV500_format(i,8+cols) = mellanslag;
   LV500_format(i,8+cols+1:8+cols+cols) = digital_data(i+1,1:cols);
   LV500_format(i,8+1+cols+cols) = mellanslag;
   LV500_format(i,8+1+2*cols+1:8+1+3*cols) = digital_data(i,1:cols);
   LV500_format(i,8+1+1+3*cols) = mellanslag;
   LV500_format(i,8+1+1+1+3*cols:8+1+1+4*cols) = digital_data(i+1,1:cols);
 else
   LV500_format(i,5:50) = [0 0 mellanslag 0 0 0 0 0 0 0 0 0 0 mellanslag 0 0 0 0 0 0 0 0 0 0 mellanslag 0 0 0 0 0 0 0 0 0 0 mellanslag 0 0 0 0 0 0 0 0 0 0];
 end;
  every_second = every_second*(-1);
end;

[rows cols] = size(LV500_format)

fid = fopen(filename,'wb');

for i = 1:rows
  fwrite(fid,9,'char');
  fwrite(fid,pattern_string,'char');
  fwrite(fid,9,'char');
  for j = 1:cols
      if (LV500_format(i,j) ~= mellanslag)
        str = num2str(LV500_format(i,j));
        fwrite(fid,str,'uchar');
      else
        fwrite(fid,' ','char');
      end;
  end;
  fwrite(fid, ';','char');
  fwrite(fid, 10, 'char'); % this may \r\n for DOS 
end;
    
fclose(fid);







