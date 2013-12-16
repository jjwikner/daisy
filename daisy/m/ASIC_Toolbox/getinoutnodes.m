function [innodes, outnodes] = getinoutnodes(sfg);
% This function extract all in and out nodes of a signal flow graph.
% Note that each node can occur more than once.

innodes = [];
outnodes = [];
sfgsize = size(sfg);
for operand = 1:sfgsize(1)
    op = sfg(operand, :);
    numinout = getnumofinout(op(1));
    innodes = [innodes, op(3:2 + numinout(1))];
    outnodes = [outnodes, op(3 + numinout(1):2 + sum(numinout))];
end

outnodes = sort(outnodes);
innodes = sort(innodes);
