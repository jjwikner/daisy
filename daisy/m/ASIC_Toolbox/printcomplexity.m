function printcomplexity(sfg);
% This function prints the complexity of the signal flow graph.

% Load operand mapping
operandmapping;

fid = 1;
fprintf(fid, 'Complexity:\n-----------\n');
oplist = sfg(:, 1);
while ~isempty(oplist)
    op = min(oplist);
    id = find(op == oplist);
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
    fprintf(fid, '%s\t%7.5g\n', op_str, length(id));
    oplist(id) = [];
end
