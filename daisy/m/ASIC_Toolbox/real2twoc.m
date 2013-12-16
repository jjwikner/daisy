function a_vec = real2twoc(number_vec, n);
% Converts real numbers between -1 and 1 to two's-complement numbers of wordlength n.
% Works properly only when number can be described exactly with n bits, i.e. no rounding has been implemented.
% 
% Example
% >> real2twoc(0.75, 5)
% ans =
%      0     1     1     0     0

if ~isnumeric(number_vec)
   error('The input must be in numeric format!');
end

if min(size(number_vec)) > 1
   error('The input must be a vector!');
end

if nargin < 2
   error('The wordlength must be specified!');
end

if round(n) ~= n
   error('The wordlength must be an integer!');
end

id = find((number_vec >= 1) | (number_vec < -1));

if length(id)
   warning('The input will be shifted!');
end

for i = 1:length(id)
   while (number_vec(id(i)) >= 1) | (number_vec(id(i)) < -1)
      number_vec(id(i)) = number_vec(id(i))*2^-1;
   end
end

% Convert each number
a_vec = zeros(length(number_vec), n);
for k = 1:length(number_vec)
   number = number_vec(k);
   
   posnum = abs(number);
   a = 0;
   for i = 1:n - 1
      bit = floor(2*posnum);
      posnum = rem(2*posnum, 1);
      a(i + 1) = bit;
   end
   
   if number < 0
      j = n;
      pos = 0;
      while ~pos & j
         if a(j) == 1
            pos = j - 1;
         else
            j = j - 1;
         end
      end
      for i = 1:pos
         a(i) = 1 - a(i);
      end
      % Special case for x = -1...
      if a(2) == 2
         a(2) = 0;
         a(1) = 1;
      end
   end
   
   a_vec(k, :) = a;
end
