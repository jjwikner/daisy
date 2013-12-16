function argformat = getargformat(operand);

% Check number of input arguments
if nargin < 1
   error('Not enough input arguments');
end
if nargin > 2
   error('Too many input arguments');
end

% Load operand mapping
operandmapping;

% Print the operand
d = 0; g = 1; s = 2;
switch operand(1)
    case op_in, argformat = [d d];
    case op_out, argformat = [d d];
    case op_add, argformat = [d d d d];
    case op_sub, argformat = [d d d d];
    case op_mult, argformat = [d d d g];
    case op_division, argformat = [d d d g];
    case op_delay, argformat = [d d d];
    case op_invert, argformat = [d d d];
    case op_shift, argformat = [d d d d s];
    case op_twoport, argformat = [d d d d d g s];
    case op_threeport, argformat = [d d d d d d d g g s];
    case op_fourport, argformat = [d d d d d d d d d g g g s];
    case op_butterfly, argformat = [d d d d d g s];
    case op_quant, argformat = [d d d d s];
    case op_overflow, argformat = [d d d s];
    case op_mux, argformat = [d d d d d];
    case op_demux, argformat = [d d d d d];
    case op_mac, argformat = [d d d d g];
    otherwise, error('Unknown operand');
end
