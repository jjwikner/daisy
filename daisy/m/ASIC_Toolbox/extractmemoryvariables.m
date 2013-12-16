function memoryvariables = extractmemoryvariables(schedule)
% EXTRACTMEMORYVARIABLES Extract memory variables from a schedule

% Copyright 2004-2006
%                Electronics Systems, Dept of EE,
%                Linkoping University, SE-581 83 Linkoping
%                Sweden
%

% Load operand mapping
operandmapping
scheduletime = schedule(1,1);
mv = scheduletime;
s=size(schedule);
for operand=1:(s(1)-1)
    if schedule(operand+1,1) ~= op_out
        [innodessource, outnodessource] = getinoutnodes_op(schedule(operand+1,:));
        [ub, ub_vec] = getupperbound(schedule, operand);
        starttime=mod(schedule(operand+1,s(2)-2)+schedule(operand+1,s(2)-1),scheduletime);
        sourceoperandnumber = schedule(operand+1,1:2);
        if sourceoperandnumber(1) ~= op_in
            ports=length(unique(ub_vec(:,2)));
            for port=1:ports  % One memory variable for each outport
               idx=find(ub_vec(:,2)==port)';
               endtime = mod(starttime + max(ub_vec(idx,1)),scheduletime);
               consumeinfo=[];
               for consumeidx = idx
                   consumeoperandnumber = schedule(ub_vec(consumeidx,3)+1,1:2);
                   if consumeoperandnumber(1) ~= op_out
                       consumetime = mod(starttime + ub_vec(consumeidx,1),scheduletime);
                       [innodes, outnodes] = getinoutnodes_op(schedule(ub_vec(consumeidx,3)+1,:));
                       consumeport = find(innodes == outnodessource(port));
                       consumeinfo = [consumeinfo consumetime consumeoperandnumber consumeport];
                   end
               end
               if ~isempty(consumeinfo)
                   variableinfo = [starttime endtime sourceoperandnumber port consumeinfo];
                   mv = addrow(mv, variableinfo);
               end
            end
        end
    end 
end
    


memoryvariables=mv;
