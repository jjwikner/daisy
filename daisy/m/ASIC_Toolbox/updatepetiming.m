function schedule = updatepetiming(schedule, timing, operandname, operandnumber)
%schedule = updatepetiming(schedule, timing, operandname, operandnumber)

if nargin < 3
    error('updatepetiming: not enough input arguments')
end

operandmapping;

if nargin < 4
    allids = 1;
else
    allids = 0;
end

switch lower(operandname)
   case 'add',
      op = op_add;
   case 'sub',
      op = op_sub;
   case 'mult',
      op = op_mult;
   case 'division',
      op = op_division;
   case 'invert',
      op = op_invert;
   case 'shift',
      op = op_shift;
   case 'twoport',
      op = op_twoport;
      adaptor = 1;
   case 'threeport',
      op = op_threeport;
      adaptor = 1;
   case 'fourport',
      op = op_fourport;
      adaptor = 1;
   case 'butterfly',
      op = op_butterfly;
   case 'mux',
      op = op_mux;
   case 'demux',
      op = op_demux;
   case 'mac',
      op = op_mac;
   otherwise,
      error('Unknown operand');
end

size_schedule=size(schedule);
time_columns_index=size_schedule(2)-4;
timecol = [time_columns_index:1:size_schedule(2)];

new_latency = timing(1);
new_exectime = timing(2);

if allids
    ids = [false; schedule(2:end, 1) == op];
else
    ids = [false; (schedule(2:end, 1) == op) & ...
        (schedule(2:end, 2) == operandnumber)];
end

schedule(ids, timecol(4)) = new_latency;
schedule(ids, timecol(5)) = new_exectime;

