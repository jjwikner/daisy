function numinout = getnumofinout(operand);
% This function returns the number of inputs and outputs of an operand

% Load operand mapping
operandmapping;

switch operand
    case op_in,
        numinout = [0 1]; % None input, one output
    case op_out,
        numinout = [1 0]; % One input, none output
    case {op_mult, op_invert, op_quant, op_overflow, op_shift, op_division, op_delay, op_quant2, op_quant_1bit},
        numinout = [1 1]; % One input, one output
    case {op_add, op_sub, op_mac},
        numinout = [2 1]; % Two inputs, one output
    case {op_twoport, op_butterfly, op_demux}, 
        numinout = [2 2]; % Two inputs, two outputs
    case op_mux,
        numinout = [3 1]; % Three inputs, one output
    case op_threeport,
        numinout = [3 3]; % Three inputs, three outputs
    case op_fourport,
        numinout = [4 4]; % Four inputs, four outputs
    otherwise,
        error('Unknown operand');
end

