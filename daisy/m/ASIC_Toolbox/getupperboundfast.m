function [UB, UB_vec]=getupperboundfast(schedule,op_i);
% gets the Upper bound  on start-time

%   schedule=sch_trans_new; %(debugging only !)
%    op_i = 3;
%extract CFG
size_schedule=size(schedule);
cfg=zeros(size_schedule(1)-1,size_schedule(2));
sfg=zeros(size_schedule(1)-1,size_schedule(2)-5);
nl = zeros(size_schedule(1)-1,2);
st = zeros(size_schedule(1)-1,1);
et = zeros(size_schedule(1)-1,1);
cfg=schedule(2:size_schedule(1),1:size_schedule(2));
%sfg=schedule(2:size_schedule(1),1:size_schedule(2)-5);
sfg=schedule(2:size_schedule(1),:);

%time info computation
time_columns_index=size_schedule(2)-4;
timecol = [time_columns_index:1:size_schedule(2)];
scheduletime=schedule(1,1);
nl = cfg(:,timecol(1):timecol(1)+1); 
st = cfg(:,timecol(3));
et = st + cfg(:,timecol(4));
for ix = 1:length(et)
    if et(ix) > scheduletime
        et(ix)=mod(et(ix),scheduletime);
    end
end


%get the inout nodes of SFG to be used later
invec=-1*ones(length(sfg(:,1)),4);
outvec=-1*ones(length(sfg(:,1)),4);
for ix = 1:length(sfg(:,1))
    [inn,outn]=getinoutnodes_op(sfg(ix,:));
    
    if isempty(inn)
        invec(ix,1:4) = -1;
    else
        l=size(inn);
        invec(ix,1:l(2))=inn(1:l(2));
    end
    if isempty(outn)
        outvec(ix,1:4) = -1;
    else
        l=size(outn);
        outvec(ix,1:l(2))=outn(1:l(2));
    end
end
inv=invec;
outv=outvec;

%------------------------------------------
% UB - Logic discussed with Oscarg.
%-------------------------------------------
[innodes, outnodes] = getinoutnodes_op(sfg(op_i,:));
if (isempty(outnodes))
    UB = inf ;
    %UB is limited only by the output assignment can be done at inf
else
    UB_vec = [];
    UB_int = [];
    for ix = 1:length(outnodes)
        op_ind=[];
        [op_ind]=find(outnodes(ix) == inv);
        % this will give me all the instr nummers that are forward indexed
        for ik = 1:length(op_ind)
            l=length(sfg(:,1));
        if (op_ind(ik) > l)
            op_ind(ik) = mod(op_ind(ik),l);
            if (op_ind(ik) == 0)
                op_ind(ik) = l;
            end
        end
        end
       for iy = 1:length(op_ind)
           inn = []; outn = [];
           [inn outn] = getinoutnodes_op(sfg(op_ind(iy),:));
           nl_index=[];
           nl_index=find(outnodes(ix) == inn);
                if st(op_ind(iy)) >= et(op_i)
                UB_int = st(op_ind(iy))-et(op_i)+nl(op_ind(iy),nl_index)*scheduletime;
                else
                UB_int = st(op_ind(iy))-et(op_i)+scheduletime+nl(op_ind(iy),nl_index)*scheduletime;
                end
           % disp(UB_int) %for debugging 
            UB_vec=[UB_vec; [UB_int ix op_ind(iy)]];
       end
    end
UB = min(UB_vec(:,1));
end


    



