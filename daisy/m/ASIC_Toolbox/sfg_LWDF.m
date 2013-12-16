function sfg = sfg_LWDF(coeff);
% SFG_LWDF   Signal flow graph for LWDF
%    SFG=SFG_LWDF(COEFF) returns the signal flow graph of a
%    LWDF with the coefficients COEFF
%
%    COEFF - coefficients

% Check the filter order
N = length(coeff);
if N < 3
    error('The filter order must be at least 3');
end
if ~mod(N, 2)
    error('The filter order must be odd');
end

% Load operand mapping
operandmapping;

% Initialize the sfg
sfg = [];
sfg = addrow(sfg, [op_in 1 1]);
% Start with a first-order allpass section
sfg = addrow(sfg, [op_twoport 0 1 9 11 10 coeff(1) adaptor_sym]);
sfg = addrow(sfg, [op_delay 0 10 9]);
% Add the required number of second-order allpass sections
id = 0;
for i = 1:2:N - 2
    sfg = addrow(sfg, [op_twoport i (id + 1) (id + 2) (id + 21) (id + 3) coeff(i + 1) adaptor_sym]);
    sfg = addrow(sfg, [op_delay i (id + 6) (id + 2)]);
    sfg = addrow(sfg, [op_twoport (i + 1) (id + 3) (id + 4) (id + 6) (id + 5) coeff(i + 2) adaptor_sym]);
    sfg = addrow(sfg, [op_delay (i + 1) (id + 5) (id + 4)]);
    id = id + 10;
end
% Add the two branches
sfg = addrow(sfg, [op_add 1 (id + 1) (id + 11) (id + 20)]);
sfg = addrow(sfg, [op_mult 1 (id + 20) (id + 21) 0.5]);
sfg = addrow(sfg, [op_out 1 (id + 21)]);

% Sort the signal flow graph in computable order
sfg = sortsfg(sfg);
