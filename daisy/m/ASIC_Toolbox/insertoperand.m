function newsfg = insertoperand(sfg, op_str, number, node, op_data, type_str);
% This function adds an operand to a signal flow graph
% 
% Inputs: sfg - signal flow graph
%         op_str - operand string
%         number - node to insert at
%         node - node to insert at
%         op_data - operand arguments
%         type_str - operand type
% Output: newsfg - signal flow graph

if nargin < 4
   error('Not enough input arguments');
end
if nargin > 6
   error('Too many input arguments');
end

% Load operand mapping
operandmapping;

type = 0;
switch lower(op_str)
case 'mult',
   op = op_mult; numarg = 4;
   type = op_data; 
case 'delay',
   op = op_delay; numarg = 3;
case 'invert',
   op = op_invert; numarg = 3;
case 'shift',
   op = op_shift; numarg = 4;
   switch lower(type_str)
       case 'left', type = shift_left;
       case 'right', type = shift_right;
       otherwise, error('Unknown shift type');
   end
   type = [op_data, type];
case 'quant',
   op = op_quant; numarg = 4;
   switch lower(type_str)
        case 'truncation', type = quant_truncation;
        case 'rounding', type = quant_rounding;
        case 'magnitudetruncation', type = quant_magnitudetruncation;
        otherwise, error('Unknown quantization type');
    end
    type = [op_data, type];
case 'overflow',
   op = op_overflow; numarg = 3;
   switch lower(type_str)
        case 'twosc', type = overflow_twosc;
        case 'saturation', type = overflow_saturation;
        otherwise, error('Unknown overflow type');
    end
otherwise,
    error('Can only insert operands with one input and one output');
end

newnode = getfreenode(sfg);

op_arg = [number, newnode, node];
if type
    op_arg = [op_arg, type];
end

% Find operation driving node and change to newnode
for row = 1:size(sfg,1)
   switch sfg(row,1)
      case op_in,    % One outout
         if sfg(row,3) == node  
            sfg(row,3) = newnode;
         end
      case {op_mult, op_delay, op_invert, op_shift, op_overflow, op_division, op_quant},  % One input, one output
         if sfg(row,4) == node
            sfg(row,4) = newnode;
         end
      case {op_add, op_sub, op_mac},   % Two inputs, one output
         if sfg(row,5) == node
            sfg(row,5) = newnode;
         end
      case {op_twoport, op_butterfly, op_demux},   % Two inputs, two outputs
         if sfg(row,5) == node
            sfg(row,5) = newnode;
         end
         if sfg(row,6) == node
            sfg(row,6) = newnode;
         end
      case {op_threeport},   % Three inputs, three outputs
         if sfg(row,6) == node
            sfg(row,6) = newnode;
         end
         if sfg(row,7) == node
            sfg(row,7) = newnode;
         end
         if sfg(row,8) == node
            sfg(row,8) = newnode;
         end
      case {op_fourport},   % Four inputs, four outputs
         if sfg(row,7) == node
            sfg(row,7) = newnode;
         end
         if sfg(row,8) == node
            sfg(row,8) = newnode;
         end
         if sfg(row,9) == node
            sfg(row,9) = newnode;
         end         
         if sfg(row,10) == node
            sfg(row,10) = newnode;
         end
      case {op_mux},   % Three inputs, one output
         if sfg(row,6) == node
            sfg(row,6) = newnode;
         end
         
   end
end

newsfg = addrow(sfg, [op, op_arg]);

