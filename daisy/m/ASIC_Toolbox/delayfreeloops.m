function [delayfree, loops, operands] = delayfreeloops(sfg);
% This function finds all delay free loops in a signal flow graph.
% Each row in 'delayfree' corresponds to one delay free loop and contain all nodes in the loop.

% Load operand mapping
operandmapping;

[loops, operands] = findloops(sfg);
opsize = size(operands);
if opsize(2) < op_delay
    delayfree = loops;
else
    delayfree = [];
    rows = find(~operands(:, op_delay));
    for i = 1:length(rows)
        nodes = sum(~isnan(loops(rows(i), :)));
        delayfree = addrow(delayfree, loops(rows(i), 1:nodes));
    end
end
