function maxlength = getmaxlength(sfg);

% number, innodes, outnodes, operanddata

maxlength = zeros(1, 4);
d = 0; g = 1; s = 2;
for op_number = 1:size(sfg, 1)
   operand = sfg(op_number, :);
   argformat = getargformat(operand(1));
   numinout = getnumofinout(operand(1));
   str = '';
   index = 1;
   for argid = 1:length(argformat)
      if argformat(argid) ~= s
         tab = 1;
         if argid == 1
            new_str = 'id: ';
         elseif numinout(1) & (argid == 2)
            new_str = 'in: ';
         elseif numinout(2) & (argid == 2 + numinout(1))
            new_str = 'out: ';
         elseif (argformat(argid) == d) & (argid == 2 + sum(numinout))
            new_str = 'bits: ';
         elseif (argformat(argid) == g) & (argid == 2 + sum(numinout))
            new_str = 'coeff: ';
         else
            str = [str, ', '];
            tab = 0;
         end
         if tab
            if length(str)
               if length(str) > maxlength(index)
                  maxlength(index) = length(str);
               end
               index = index + 1;
            end
            str = new_str;
         end
         if argformat(argid) == d
            str = sprintf('%s%d', str, operand(argid + 1));
         elseif argformat(argid) == g
            str = sprintf('%s%0.3g', str, operand(argid + 1));
         end
      end
   end
   if index < 5
      if length(str) > maxlength(index)
         maxlength(index) = length(str);
      end
   end
end

