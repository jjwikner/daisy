function [sfg]=sfg_firfilter(h,type)
% SFG_FIRFILTER   Signal flow graph for FIR filter
%    SFG=SFG_FIRFILTER(H, TYPE) returns the signal flow graph of a
%    FIR filter with impulse response H and type TYPE
%
%    H - impulse response
%    TYPE - 'direct' (default) or 'transposed'

% Copyright 2004 Electronics Systems, Dept of EE,
%                Linkoping University, SE-581 83 Linkoping
%                Sweden
% Generated from $Id: sfg_firfilter.m,v 1.3 2004/07/01 16:32:48 oscarg Exp $

    operandmapping;
    
    if nargin < 2
      type = 'direct';
    else
      if not(strcmp(type,'direct')) & not(strcmp(type, 'transposed'))
	error('Filter type must be direct or transposed')
      end
    end
    
    filterlength=length(h);
    if not(filterlength)
      error('The impulse response is empty')
    end
    
    sfg=[op_in 1 1 NaN NaN];
    
    if strcmp(type,'direct')
      % Multiplications
      for tap=1:filterlength
	sfg=[sfg;[op_mult tap tap tap+filterlength h(tap)]];
      end
      
      % Addition
      nodestobeadded = (1:filterlength)+filterlength;
      next_node=2*filterlength+1;
      adder_count=1;
      while length(nodestobeadded) > 1
	newnodestobeadded=[];
	for adder=1:2:(length(nodestobeadded)-1)
	  sfg=[sfg;[op_add adder_count nodestobeadded(adder) ...
		    nodestobeadded(adder+1) next_node]];
	  newnodestobeadded=[newnodestobeadded next_node];
	  next_node=next_node+1;
	  adder_count=adder_count+1;
	end
	if mod(length(nodestobeadded),2) % Odd input nodes
	  newnodestobeadded=[newnodestobeadded ...
			     nodestobeadded(length(nodestobeadded))];
	end
	nodestobeadded = newnodestobeadded;
	  
      end
      
      % Output
      sfg=[sfg;[op_out 1 next_node-1 NaN NaN]];
      

      % Delay elements
      for tap=filterlength:-1:2
	sfg=[sfg;[op_delay tap-1 tap-1 tap NaN]];
      end
    else % Transposed direct form
      sfg=[sfg;[op_mult 1 1 2 h(1)]];
      
      for tap=2:filterlength
	sfg=[sfg;[op_mult tap 1 tap*3-2 h(tap)]];
	sfg=[sfg;[op_add tap tap*3-3 tap*3-2 tap*3-1]];
      end
      sfg=[sfg;[op_out 1 filterlength*3-1 NaN NaN]];
      for tap=filterlength-1:-1:1
	sfg=[sfg;[op_delay tap tap*3-1 tap*3 NaN]];
      end
      
    end
