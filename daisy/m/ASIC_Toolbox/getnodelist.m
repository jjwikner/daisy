function nodelist = getnodelist(sfg)
% GETNODELIST  Get a list of all used nodes in SFG
% NODELIST = GETNODELIST(SFG) returns a list of all nodes present
%    in the signal flow graph SFG
%
%    SFG - Signal flow graph
%    NODELIST - Resulting list of nodes
  
% Copyright 2004 Electronics Systems, Dept of EE,
%                Linkoping University, SE-581 83 Linkoping
%                Sweden
% Generated from $Id: getnodelist.m,v 1.4 2004/07/01 14:57:21 oscarg Exp $
  
% Load operand mapping
  operandmapping;
  
  sfgsize=size(sfg);
  
  nodelist=[];
  for operand = 1:sfgsize(1)
    switch sfg(operand,1)
     case {op_in, op_out},  % One operand
      nodelist=[nodelist sfg(operand,3)];
     case {op_mult, op_delay, op_invert, op_quant, op_overflow, ...
	   op_shift, op_division, op_quant2,op_quant_1bit}, % Two operands 
      nodelist=[nodelist sfg(operand,3:4)];
     case {op_add, op_sub, op_mac}, % Three operands
      nodelist=[nodelist sfg(operand,3:5)];
     case {op_twoport, op_butterfly, op_mux, op_demux}, % Four operands
      nodelist=[nodelist sfg(operand,3:6)];
     case {op_threeport}, % Six operands
      nodelist=[nodelist sfg(operand,3:8)];
     case {op_fourport}, % Eight operands
      nodelist=[nodelist sfg(operand,3:10)];
     otherwise,
      error('Invalid or non-implemented operand')
    end
  end

  if(~isempty(nodelist))
    nodelist=sort(nodelist);
    nodelist(find(diff(nodelist)==0))=[];
  end
  nodelist=nodelist';


