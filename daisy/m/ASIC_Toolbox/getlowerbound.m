function [LB]=getlowerbound(schedule,op_i);
% gets the Lower bound  on start-time

%schedule=sch_direct; (debug only!)
%extract CFG
size_schedule=size(schedule);
cfg=zeros(size_schedule(1)-1,size_schedule(2));
sfg=zeros(size_schedule(1)-1,size_schedule(2)-5);
nl = zeros(size_schedule(1)-1,2);
st = zeros(size_schedule(1)-1,1);
et = zeros(size_schedule(1)-1,1);
cfg=schedule(2:size_schedule(1),1:size_schedule(2));
sfg=schedule(2:size_schedule(1),1:size_schedule(2)-5);

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
% LB Logic discussed with oscarg
%-------------------------------------------

[innodes, outnodes] = getinoutnodes_op(sfg(op_i,:));
if (isempty(innodes))
    LB = 0 ;
else
        LB_int = 0;

    for ix = 1:length(innodes)
    %[op_ind,inp_ind]=get_opindices_backward(sfg,innodes(ix));
        [op_ind]=find(innodes(ix) == outv);
        if (op_ind > length(sfg(:,1)))
            op_ind = mod(op_ind,length(sfg(:,1)));
            if (op_ind == 0)
                op_ind = length(sfg(:,1));
            end
        end
            nl_index=find(innodes(ix) == inv(op_i,:));
                if st(op_i) >= et(op_ind)
                LB_int(ix) = -st(op_i)+et(op_ind)-nl(op_i,nl_index)*scheduletime;
                else
                LB_int(ix) = -st(op_i)+et(op_ind)-nl(op_i,nl_index)*scheduletime-scheduletime;
                
                end

    %disp(LB_int)
    end
LB = max(LB_int);
end


    



