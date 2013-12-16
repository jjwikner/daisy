function sfg = deleteoperand(sfg, operandname, number);
% This function is used to delete an operand from a signal flow graph.
% The input and output of the deleted operand are connected.
% The function only works on operands with one input and one output.
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
   case 'mult', op = op_mult;
   case 'division', op = op_division;
   case 'delay', op = op_delay;
   case 'invert', op = op_invert;
   case 'shift', op = op_shift;
   case 'quant', op = op_quant;
   case 'overflow', op = op_overflow;
   otherwise, error('Can only insert operands with one input and one output');
end

% Find row in sfg
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

% Update nodes
[innode_op, outnode_op] = getinoutnodes_op(sfg(row_op, :));
sfg(row_op, :) = [];
for row = 1:size(sfg, 1)
   [innodes, outnodes] = getinoutnodes_op(sfg(row, :));
   innodes(find(innodes == outnode_op)) = innode_op;
   sfg(row, 3:2 + length(innodes)) = innodes;
end

