function sfg = cascadesfg(sfg1, sfg2);
% The output of sfg1 is connected with the input of sfg2

% Check input arguments
if nargin ~= 2
    error('Not correct number of input arguments');
end

% Chech sfg:s
errorlist1 = checknodes(sfg1, 1);
if length(errorlist1)
   error('The first signal flow graph is not correct!');
end
errorlist2 = checknodes(sfg1, 1);
if length(errorlist2)
   error('The second signal flow graph is not correct!');
end

% Load operand mapping
operandmapping;

% Check output of sfg1 and input of sfg2
outputs1 = find(sfg1(:, 1) == op_out);
if length(outputs1) > 1
   error('The first signal flow graph has too many outputs!');
end
inputs2 = find(sfg2(:, 1) == op_in);
if length(inputs2) > 1
   error('The second signal flow graph has too many inputs!');
end

% Change all nodes in sfg2 that occur in sfg1
[innodes1, outnodes1] = getinoutnodes(sfg1);
nodes1 = sort([innodes1, outnodes1]);
nodes1(find(~diff(nodes1))) = [];
[innodes2, outnodes2] = getinoutnodes(sfg2);
nodes2 = sort([innodes2, outnodes2]);
nodes2(find(~diff(nodes2))) = [];
% numofnewnodes = 0;
% oldnodes = [];
% for i = 1:length(nodes2)
%    if length(find(nodes2(i) == nodes1))
%       oldnodes = [oldnodes, nodes2(i)];
%       numofnewnodes = numofnewnodes + 1;
%    end
% end

numofnewnodes = length(nodes2);
oldnodes = nodes2;

% Get new nodes
newnodes = getfreenode(sfg1, numofnewnodes);

% Replace the old input of sfg2 with the output of sfg1
if length(find(sfg2(inputs2,  3) == oldnodes))
   newnodes(find(sfg2(inputs2,  3) == oldnodes)) = sfg1(outputs1, 3);
else
   oldnodes = [oldnodes, sfg2(inputs2,  3)];
   newnodes = [newnodes, sfg1(outputs1, 3)];
end

% Update nodes in sfg2
for row = 1:size(sfg2, 1)
   [innodes_op, outnodes_op] = getinoutnodes_op(sfg2(row, :));
   nodes_op = [innodes_op, outnodes_op];
   for i = 1:length(nodes_op)
      id = find(nodes_op(i) == oldnodes);
      if length(id)
         nodes_op(i) = newnodes(id);
      end
   end
   sfg2(row, 3:2 + length(nodes_op)) = nodes_op;
end

% Remove the old the output of sfg1 and input of sfg2
sfg1 = removeoperand(sfg1, 'out', sfg1(outputs1, 2));
sfg2 = removeoperand(sfg2, 'in',  sfg2(inputs2,  2));

% Update operand id:s in sfg2
op_max = max(sfg2(:, 1));
for op = 1:op_max
   rows2 = find(op == sfg2(:, 1));
   if length(rows2)
      rows1 = find(op == sfg1(:, 1));
      if length(rows1)
         minid = max(sfg1(rows1, 2)) + 1;
      else
         minid = 1;
      end
      sfg2(rows2, 2) = (minid:minid + length(rows2) - 1)';
   end
end

% Concatenate the two sfg:s
col1 = size(sfg1, 2);
col2 = size(sfg2, 2);
if col1 < col2
   sfg1 = [sfg1, NaN*ones(size(sfg1, 1), col2 - col1)];
elseif col2 < col1
   sfg2 = [sfg2, NaN*ones(size(sfg2, 1), col1 - col2)];
end
sfg = [sfg1; sfg2];

