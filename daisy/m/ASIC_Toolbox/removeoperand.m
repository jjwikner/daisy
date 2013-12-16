function sfg = removeoperand(sfg, operandname, number);
% This function is used to remove an operand from a signal flow graph.
% The operand is simply removed from the signal flow graph without any changes of other operands.
% 
% Inputs: sfg - signal flow graph
%         operandname - operand name as a string
%         number - operand ID number
% Output: sfg - updated signal flow graph


if nargin < 3
   error('Not enough input arguments');
end
if nargin > 3
   error('Too many input arguments');
end

% Load operand mapping
operandmapping;

% Find operation
switch lower(operandname)
   case 'in', op = op_in;
   case 'out', op = op_out;
   case 'add', op = op_add;
   case 'sub', op = op_sub;
   case 'mult', op = op_mult;
   case 'division', op = op_division;
   case 'delay', op = op_delay;
   case 'invert', op = op_invert;
   case 'shift', op = op_shift;
   case 'twoport', op = op_twoport;
   case 'threeport', op = op_threeport;
   case 'fourport', op = op_fourport;
   case 'butterfly', op = op_butterfly;
   case 'quant', op = op_quant;
   case 'overflow', op = op_overflow;
   case 'mux', op = op_mux;
   case 'demux', op = op_demux;
   case 'mac', op = op_mac;
   otherwise, error('Unknown operand');
end

% Find and remove the row from the sfg
id_op = find(op == sfg(:, 1));
id_no = find(number == sfg(:, 2));
index = 1;
while ~length(find(id_op(index) == id_no))
   index = index + 1;
   if index > length(id_op)
      error('The operand could not be found!');
   end
end
row_op = id_op(index);
sfg(row_op, :) = [];

