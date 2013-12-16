function [innodes, outnodes] = getinoutnodes_op(op);
% This function extract all in and out nodes of an operand, i.e., one row of a signal flow graph.

numinout = getnumofinout(op(1));
innodes = op(3:2 + numinout(1));
outnodes = op(3 + numinout(1):2 + sum(numinout));
