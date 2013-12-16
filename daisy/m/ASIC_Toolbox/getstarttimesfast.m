function [starttime,upperbound,lowerbound]=getstarttimesfast(schedule,operandname,number);
% gets the upper and lower bounds on the starttime

%schedule = scha; debug only
%operandname = 'add'; debug only
%number = 2; debug only

%-------------------------------------------------
% get the operation index
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
    case 'sub', op = op_sub;
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
%-------------------------------------------------
% Extract SFG 
%-------------------------------------------------
size_schedule=size(schedule);
sfg=zeros(size_schedule(1)-1,size_schedule(2)-5);
cfg=zeros(size_schedule(1)-1,size_schedule(2));
sfg=schedule(2:size_schedule(1),1:size_schedule(2)-5);
cfg=schedule(2:size_schedule(1),1:size_schedule(2));

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

time_columns_index=size_schedule(2)-4;
timecol = [time_columns_index:1:size_schedule(2)];
st = schedule(2:size_schedule(1),timecol(3));
starttime=st(op_i);

%-------------------------------------------------
% get the UB
%-------------------------------------------------
[UB]=getupperboundfast(schedule,op_i);
upperbound=UB;
%-------------------------------------------------
% get the LB
%-------------------------------------------------
[LB]=getlowerboundfast(schedule,op_i);
lowerbound=LB;
%-------------------------------------------------