function nl = getfreenode(sfg, nodes)
% GETFREENODE Get the smallest node number not used in an SFG
% NODENUMBER = GETFREENODE(SFG) returns a node number to be
%    used for an additional node in SFG
%
% SFG - Signal flow graph
% NODENUMBER - Resulting node number
%
% NODELIST = GETFREENODE(SFG, N) returns N nodes to be used for 
% additional nodes in SFG

% Copyright 2006 Electronics Systems, Dept of EE,
%                Linkoping University, SE-581 83 Linkoping
%                Sweden
% Generated from $Id: getfreenode.m,v 1.1 2006/01/24 14:30:48 oscarg Exp $

if nargin == 1
  nodes = 1;
end

nodelist = getnodelist(sfg);
possiblenodes = setxor(getnodelist(sfg), 1:(max(nodelist)+nodes));
nl=possiblenodes(1:nodes);
