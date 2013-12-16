function sfg=replacemac(sfg, mac_list)
% REPLACEMAC Replace MAC with multiply and add
% NEWSFG=REPLACEMAC(SFG, MACLIST) replaces the MAC operations in
% the SFG with a multiplication and an addition
%
%    SFG - Signal flow graph
%    MACLIST - List of MAC id:s (optional, all MACs if not given)
  
% Copyright 2004 Electronics Systems, Dept of EE,
%                Linkoping University, SE-581 83 Linkoping
%                Sweden
% Generated from $Id: replacemac.m,v 1.1 2004/07/01 16:10:34 oscarg Exp $
  
  operandmapping;
  if nargin < 2
    macrows=find(sfg(:,1)==op_mac);
    mac_list = sfg(macrows,2);
  end
  
  nodelist=getnodelist(sfg);
  maxnode=max(nodelist);
  
  sfgsize=size(sfg);
  
  rowstoremove=[]
  for op=1:sfgsize
    if sfg(op,1) == op_mac % Mac?
      if any(sfg(op,2) == mac_list) % Mac to be removed
	maxnode=maxnode+1;
	rowstoremove=[rowstoremove; op];
	tmp_mac=sfg(op,:);
	sfg=addoperand(sfg,'mult',[100+sfg(op,2) sfg(op,3) maxnode ...
		   sfg(op,6)]);
	sfg=addoperand(sfg,'add',[100+sfg(op,2) sfg(op,4) maxnode sfg(op,5)]);
      end
    end
    
  end
  sfg(rowstoremove,:)=[];
  