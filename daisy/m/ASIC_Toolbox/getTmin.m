function getTmin(sfg);
% This function prints the Tmin expression.
% 
% T_min = max{T_opi/N_i}
% where - T_opi is the latency of the operations in loop i
%       - N_i is the number of delay elements in loop i

[delayfree, loops, operands] = delayfreeloops(sfg);

if length(delayfree)
    error('The signal flow graph contain delay free loops');
end
numofloops = size(loops);
if ~numofloops(1)
    error('The signal flow graph does not contain any loops');
end

% Delete equal loops
delete_id = [];
for loop_no = 1:numofloops(1) - 1
    if isempty(delete_id) | ~length(find(loop_no == delete_id))
        for i = loop_no + 1:numofloops(1)
            if sum(operands(loop_no, :) == operands(i, :)) == length(operands(i, :))
                delete_id = [delete_id, i];
            end
        end
    end
end
loops(delete_id, :) = [];
operands(delete_id, :) = [];
numofloops = size(loops);

% Load operand mapping
operandmapping;

fid = 1;
fprintf(fid, '\tT_min = max{\n');
for loop_no = 1:numofloops(1)
    opid = find(operands(loop_no, :));
    opid(find(opid == op_delay)) = [];
    delay = operands(loop_no, op_delay);
    fprintf(fid, '\t\t\t');
    if delay > 1
        fprintf(fid, '( ');
    end
    for op_no = 1:length(opid)
        op = opid(op_no);
        switch op
        case op_in,        op_str = 'in';
        case op_out,       op_str = 'out';
        case op_add,       op_str = 'add';
        case op_sub,       op_str = 'sub';
        case op_mult,      op_str = 'mult';
        case op_division,  op_str = 'division';
        case op_delay,     op_str = 'delay';
        case op_invert,    op_str = 'invert';
        case op_shift,     op_str = 'shift';
        case op_twoport,   op_str = 'twoport';
        case op_threeport, op_str = 'threeport';
        case op_fourport,  op_str = 'fourport';
        case op_butterfly, op_str = 'butterfly';
        case op_quant,     op_str = 'quant';
        case op_overflow,  op_str = 'overflow ';
        case op_mux,       op_str = 'mux';
        case op_demux,     op_str = 'demux';
        case op_mac,       op_str = 'mac';
        otherwise, error('Unknown operand');
        end
        n = operands(loop_no, op);
        if n > 1
            fprintf(fid, '%d*', n);
        end
        fprintf(fid, 'T_%s', op_str);
        if op_no < length(opid)
            fprintf(fid, ' + ');
        elseif delay > 1
            fprintf(fid, ' ) / %d', delay);
        end
    end
    if loop_no < numofloops(1)
        fprintf(fid, ',\n');
    end
end
fprintf(fid, ' }\n');
