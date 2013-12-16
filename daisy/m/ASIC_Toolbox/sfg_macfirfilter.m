function [sfg]=sfg_macfirfilter(h,type)
% SFG_FIRFILTER   Signal flow graph for FIR filter based on MAC
%    SFG=SFG_FIRFILTER(H, TYPE) returns the signal flow graph of a
%    FIR filter with impulse response H and type TYPE based on
%    multiply-accumulate (MAC) operations.
%
%    H - impulse response
%    TYPE - 'direct' (default) or 'transposed'

% Copyright 2004 Electronics Systems, Dept of EE,
%                Linkoping University, SE-581 83 Linkoping
%                Sweden
% Generated from $Id: sfg_macfirfilter.m,v 1.1 2004/07/01 15:13:34 oscarg Exp $

    operandmapping;
    
    if nargin < 2
      type = 'direct'
    else
      if not(strcmp(type,'direct')) & not(strcmp(type, 'transposed'))
	error('Filter type must be direct or transposed')
      end
    end
    
    filterlength=length(h);
    if not(filterlength)
      error('The impulse response is empty')
    end
    
    sfg=[op_in 1 1 NaN NaN NaN];
    
    if strcmp(type,'direct')
    % Direct form FIR filter
      sfg=[sfg;[op_mult 1 1 1+filterlength h(1) NaN]];
      
      % MAC
      for tap=2:filterlength
	sfg=[sfg;[op_mac tap tap tap+filterlength-1 tap+filterlength h(tap)]];
      end
      
      
      % Output
      sfg=[sfg;[op_out 1 2*filterlength NaN NaN NaN]];
      

      % Delay elements
      for tap=filterlength:-1:2
	sfg=[sfg;[op_delay tap-1 tap-1 tap NaN NaN]];
      end
      
    else 
    % Transposed direct form
      sfg=[sfg;[op_mult 1 1 2 h(1) NaN]];
      
      % MAC
      for tap=2:filterlength
	sfg=[sfg;[op_mac tap 1 tap*2-1 tap*2 h(tap)]];
      end
      
      % Output
      sfg=[sfg;[op_out 1 filterlength*2 NaN NaN NaN]];

      % Delay elements
      for tap=filterlength-1:-1:1
	sfg=[sfg;[op_delay tap tap*2 tap*2+1 NaN NaN]];
      end
      
    end
