function coefficients = getcoefficients(sfgrow)
operandmapping
numinout = getnumofinout(sfgrow(1));
coeffstart=3+sum(numinout);
switch sfgrow(1)
    case {op_add, op_sub, op_quant, op_overflow, op_invert, op_shift, op_mux, op_demux},
        nocoeffs=0;
    case {op_mult, op_twoport, op_butterfly, op_division, op_mac},
        nocoeffs=1;
    case op_threeport,
        nocoeffs=2;
    case op_fourport,
        nocoeffs=3;
    otherwise,
        error('Unknown operand')
end
coefficients=sfgrow(coeffstart:(coeffstart+nocoeffs-1));
       