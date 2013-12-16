function[newschedule]=changestarttimefast(schedule,operandname,number,timedifference);
% % Changes the starttime as starttime = startime + timedifference;

if(nargin > 4)
    error('too many inputs')
end
if(nargin < 4)
    error('too less inputs')
end

 % uncomment for debug only!
%   schedule=sch2; 
%   operandname='out';
%   number = 2;
%   timedifference = 2;
%-------------------------------------------------
% Extract SFG CFG -> timing info
%-------------------------------------------------
size_schedule=size(schedule);
sfg=zeros(size_schedule(1)-1,size_schedule(2)-5);
cfg=zeros(size_schedule(1)-1,size_schedule(2));
nl = zeros(size_schedule(1)-1,2);
st = zeros(size_schedule(1)-1,1);
et = zeros(size_schedule(1)-1,1);
newschedule=zeros(size_schedule(1),size_schedule(2));

cfg=schedule(2:size_schedule(1),1:size_schedule(2));
sfg=schedule(2:size_schedule(1),1:size_schedule(2)-5);

%time info computation
time_columns_index=size_schedule(2)-4;
timecol = [time_columns_index:1:size_schedule(2)];
scheduletime=schedule(1,1);
nl = cfg(:,timecol(1):timecol(1)+1); 
st = cfg(:,timecol(3));
et = st + cfg(:,timecol(4));
latency=cfg(:,timecol(4));
for ix = 1:length(latency)
    if et(ix) > scheduletime
        et(ix)=mod(et(ix),scheduletime);
    end
end

scheduletime = schedule(1,1);
%-------------------------------------------------
% Load operand mapping
%-------------------------------------------------
operandmapping;
%-------------------------------------------------
% Find operation
%-------------------------------------------------
switch lower(operandname)
    case 'in', op = op_in;
    case 'out', op = op_out;
    case 'add', op = op_add;
   case 'mult', op = op_mult;
   case 'division', op = op_division;
   case 'delay', op = op_delay;
   case 'invert', op = op_invert;
   case 'shift', op = op_shift;
   case 'quant', op = op_quant;
   case 'overflow', op = op_overflow;
    case 'twoport', op = op_twoport;
   otherwise, error('Unmapped Operation');
end
%--------------------------------------------------
% get the operation index
%--------------------------------------------------
same_ops_inst = find(op == sfg(:,1));
if isempty(same_ops_inst)
    error('Operand type not found');
end
same_ops_i = 0;
ik = length(same_ops_inst);
for ik = 1:length(same_ops_inst)
same_ops_i(ik) = sfg(same_ops_inst(ik),2);
end
index = find(number == same_ops_i);

if isempty(index)
error('operand number mistmatch');
end

op_i = same_ops_inst(index);

%-------------------------------------------------
if (sfg(op_i,1) == 6)
    error('delays cannot be moved')
end
%-------------------------------------------------
    
    inpmemtime=[];
    outmemtime=[];
    backop_i=[];
    backout_i=[];
    forwardops = [];
    forwardop_i = [];
    forwardin_i = [];
    forwardops_nl = [];
    
    [innodes,outnodes]=getinoutnodes_op(sfg(op_i,:));
    [ub]=getupperboundfast(schedule,op_i);
    [lb]=getlowerboundfast(schedule,op_i);
    
    if timedifference > ub
        error('upper bound error');
    end
    if timedifference < lb
        error('lower bound error');
    end
    
    if~(isempty(innodes))
    for ix = 1:length(innodes)
        [backop_i(ix),backout_i(ix)]=get_opindices_backward(sfg,innodes(ix));
        if st(op_i) >= et(backop_i(ix))
            inpmemtime(ix) = st(op_i)-et(backop_i(ix)) + nl(op_i,ix)*scheduletime;
        else
            inpmemtime(ix) = st(op_i)-et(backop_i(ix)) + nl(op_i,ix)*scheduletime+scheduletime;
        end
    end
    if timedifference  >= 0
            newinpmemtime  = inpmemtime + abs(timedifference);
        else
            newinpmemtime  = inpmemtime - abs(timedifference);
    end
    end

    if ~(isempty(outnodes))
    for io = 1:length(outnodes)
        [forwardop_i,forwardin_i]=get_opindices_forward(sfg,outnodes(io));
        forwardops=[forwardops forwardop_i];
        forwardops_nl=[forwardops_nl forwardin_i];
    end
    
    for ii = 1:length(forwardops)
        if st(forwardops(ii)) >= et(op_i) 
            outmemtime(ii) = st(forwardops(ii))-et(op_i)+nl(forwardops(ii),forwardops_nl(ii))*scheduletime;
        else
            outmemtime(ii) = st(forwardops(ii))-et(op_i)+nl(forwardops(ii),forwardops_nl(ii))*scheduletime+scheduletime;
        end
        
    end
    if timedifference  >= 0
            newoutmemtime  = outmemtime - abs(timedifference);
        else
            newoutmemtime  = outmemtime + abs(timedifference);
    end
    end

%--------------------------------------------------------------------------
% Find the direction of movement and update the outmem and inpmem
% accordingly
%--------------------------------------------------------------------------
delta = timedifference;
quotient =fix(delta/scheduletime);
remainder = delta + abs(quotient)*scheduletime;
stime = st(op_i) + remainder;
if stime < 0
    st(op_i) = stime + scheduletime;
elseif stime > scheduletime
    st(op_i) = mod(stime,scheduletime);
else
    if latency(op_i) == 0
    st(op_i)=stime;
    else
    st(op_i)=mod(stime,scheduletime);
    end
end
% Update the number of laps carefully
% Present instr
if ~(isempty(innodes))
inpmemtime_nl = fix(newinpmemtime./scheduletime);
    for ij = 1:length(backop_i)
        nl(op_i,ij)=inpmemtime_nl(ij);
    end
end

% Forward instr
if ~(isempty(outnodes))
outmemtime_nl = fix(newoutmemtime./scheduletime);
for ik = 1:length(forwardops)
    nl(forwardops(ik),forwardops_nl(ik))=outmemtime_nl(ik);
end
end
%end
%--------------------------------------------------------------------------
%form newschedule
%--------------------------------------------------------------------------
newschedule = schedule;
newschedule(2:size_schedule(1),timecol(1):timecol(1)+1)=nl;
newschedule(2:size_schedule(1),timecol(3))=st;
%--------------------------------------------------------------------------


