function [outputs, outputids, registers, registerids, nodes, nodeids] ...
    = simulate(sfg, inputvalues, inputids, initialregisters, registerids, wordlength)
% SIMULATE  Simulate a signal flow graph
%    [OUTPUTS, OUTPUTIDS, REGISTERS, REGISTERIDS, NODES, NODEIDS]
%    = SIMULATE(SFG, INPUTVALUES, INPUTIDS, INITIALREGISTER, REGISTERIDS, WORDLENGTH)
%
%
%    SFG - Signal flow graph
%    INPUTVALUES - Vector or matrix with values to apply to the inputs
%    INPUTNODES - Vector with node numbers for the inputs
%    INITIALREGISTER - Vector with initial values for registers
%    REGISTERIDS - Ids for registers with initial values
%    WORDLENGTH - Optimal wordlength for finite wordlength simulation

% Copyright 2004-2006
%                Electronics Systems, Dept of EE,
%                Linkoping University, SE-581 83 Linkoping
%                Sweden
%

if (nargin < 2)
    error('Not enough input arguments')
end

% Load operand mapping
operandmapping;

% Computable order
sfg=sortsfg(sfg);

% Move registers first
regidx=find(sfg(:,1) == op_delay);
regsfg=sfg(regidx,:);
sfg(regidx,:)=[];
sfg=[regsfg;sfg];

% Check inputs
if(nargin < 3)
    inputids = sfg(find(sfg(:, 1) == op_in), 2);
    numin = length(inputids);
    if ~numin
        error('There is no input in the SFG');
    end
    if numin ~= 1
        error('More than one input available, please specify stimuli-input mapping')
    end
end

% Finite wordlength?
if(nargin == 6)
    if length(wordlength) == 1
        wordlength=[1 wordlength];
    else
        if length(wordlength) > 2
            error('You can only specify integer and fractional wordlengths')
        end
    end
else
    wordlength = 0;
end

% Check input values
stimulisize=size(inputvalues);
samples=stimulisize(2);
if (stimulisize(1) ~= length(inputids))
    error('Stimuli and ID-mapping inconsistent')
end

inputrows=find(sfg(:,1)==op_in);
if (stimulisize(1) ~= length(inputrows))
    error('Stimuli and signal flow graph inconsistent')
end

[inputs1, inidx1]=sort(sfg(inputrows,2));
[inputs2, inidx2]=sort(inputids);
if not(all(inputs1==inputs2))
    error('ID-mapping and signal flow graph inconsistent')
end

outputrows=find(sfg(:,1)==op_out);
outputids=sfg(outputrows,2);
outputcount=length(outputrows);
outputs=zeros(outputcount, samples);
intoutputs=zeros(outputcount, 1);


sfgsize=size(sfg);



% Nodes
nodelist=getnodelist(sfg);
nodeids=nodelist;
nodecount=length(nodelist);
intnodes(nodelist)=0;


if nargout > 4
    nodes=zeros(nodecount, samples);
end

% Registers
if nargin > 4
    intnodes(registerids)=initialregisters;
end

registerrows=find(sfg(:,1)==op_delay);
registerids=sfg(registerrows,2);
registernodes=sfg(registerrows,3);
registercount=length(registerids);
if not(registercount)
    registers=[];
end



% Simulation loop
for time = 1:samples
    % Operand list
    intinputs=inputvalues(:,time);
    for operand = 1:sfgsize(1)
        switch sfg(operand,1)
            % Input
            case op_in,
                intnodes(sfg(operand,3)) = intinputs(find(inputids==sfg(operand,2)));

                % Output
            case op_out,
                intoutputs(find(outputids==sfg(operand,2))) = intnodes(sfg(operand,3));

                % Addition
            case op_add,
                intnodes(sfg(operand,5)) = intnodes(sfg(operand,3)) + ...
                    intnodes(sfg(operand,4));

                % Subtraction
            case op_sub,
                intnodes(sfg(operand,5)) = intnodes(sfg(operand,3)) - ...
                    intnodes(sfg(operand,4));

                % Multiplication
            case op_mult,
                intnodes(sfg(operand,4)) = intnodes(sfg(operand,3)) * ...
                    sfg(operand,5);

                % Delay
            case op_delay,
                if time > 1
                    intnodes(sfg(operand,4)) = intnodes(sfg(operand,3));
                end
                % Quantization
            case op_quant,
                switch sfg(operand,6)
                    case quant_truncation,
                        intnodes(sfg(operand,4)) = floor((2^(sfg(operand,5)-1))*intnodes(sfg(operand,3)))/(2^(sfg(operand,5)-1));
                    case quant_rounding,
                        intnodes(sfg(operand,4)) = round((2^(sfg(operand,5)-1))*intnodes(sfg(operand,3)))/(2^(sfg(operand,5)-1));
                    case quant_magnitudetruncation,
                        intnodes(sfg(operand,4)) = fix((2^(sfg(operand,5)-1))*intnodes(sfg(operand,3)))/(2^(sfg(operand,5)-1));
                    otherwise,
                        error('Invalid or non-implemented quantization type')
                end

                % Overflow
            case op_overflow,
                switch sfg(operand,5)
                    case overflow_twosc,
                        intnodes(sfg(operand,4)) = twoscoverflow(intnodes(sfg(operand,3)),1);
                    case overflow_saturation,
                        saturation_value = intnodes(sfg(operand,3));
                        if saturation_value > 1
                            intnodes(sfg(operand,4)) = 1-eps;
                        elseif saturation_value < -1
                            intnodes(sfg(operand,4)) = -1;
                        else
                            intnodes(sfg(operand,4)) = intnodes(sfg(operand,3));
                        end
                    otherwise,
                        error('Invalid or non-implemented overflow type')
                end

                % Inversion (negation)
            case op_invert,
                intnodes(sfg(operand,4)) = -intnodes(sfg(operand,3));

                % Shift
            case op_shift,
                if (sfg(operand,6) == shift_left)
                    intnodes(sfg(operand,4)) = intnodes(sfg(operand,3))*(2^sfg(operand,5));
                else
                    intnodes(sfg(operand,4)) = intnodes(sfg(operand,3))*(2^(-sfg(operand,5)));
                end

                % Twoport adaptor
            case op_twoport,
                switch sfg(operand,8)
                    case adaptor_ser,
                        intnodes(sfg(operand,5)) = - intnodes(sfg(operand,3)) - ...
                            (sfg(operand,7))*(intnodes(sfg(operand,4)) + ...
                            intnodes(sfg(operand,3)));
                        intnodes(sfg(operand,6)) = -(intnodes(sfg(operand,4)) + ...
                            2*intnodes(sfg(operand,3))) ...
                            + (sfg(operand,7))*(intnodes(sfg(operand,4)) + ...
                            intnodes(sfg(operand,3)));
                    case adaptor_par,
                        intnodes(sfg(operand,5)) = 2*intnodes(sfg(operand,4)) - ...
                            intnodes(sfg(operand,3)) - (sfg(operand,7))* ...
                            (intnodes(sfg(operand,4)) - intnodes(sfg(operand,3)));
                        intnodes(sfg(operand,6)) = intnodes(sfg(operand,4)) - ...
                            (sfg(operand,7))*(intnodes(sfg(operand,4)) - ...
                            intnodes(sfg(operand,3)));
                    case adaptor_sym,
                        intnodes(sfg(operand,5)) = intnodes(sfg(operand,4)) + ...
                            (sfg(operand,7))*(intnodes(sfg(operand,4)) - ...
                            intnodes(sfg(operand,3)));
                        intnodes(sfg(operand,6)) = intnodes(sfg(operand,3)) + ...
                            (sfg(operand,7))*(intnodes(sfg(operand,4)) - ...
                            intnodes(sfg(operand,3)));
                    otherwise,
                        error('Invalid or non-implemented adaptor type')
                end

                % Division
            case op_division,
                intnodes(sfg(operand,4)) = intnodes(sfg(operand,3)) / ...
                    sfg(operand,5);

                % Multiplexer
            case op_mux,
                if sfg(operand,5)
                    intnodes(sfg(operand,6)) = intnodes(sfg(operand,4));
                else
                    intnodes(sfg(operand,6)) = intnodes(sfg(operand,3));
                end

                % Demultiplexer
            case op_demux,
                if sfg(operand,4)
                    intnodes(sfg(operand,6)) = intnodes(sfg(operand,3));
                else
                    intnodes(sfg(operand,5)) = intnodes(sfg(operand,3));
                end

                % Multiply-accumulate
            case op_mac,
                intnodes(sfg(operand,5)) = sfg(operand,6)*intnodes(sfg(operand,3)) + ...
                    intnodes(sfg(operand,4));
               
                % Quantization within a range symmetrical about 0
            case op_quant2
                tmp1 = intnodes(sfg(operand,3))/(sfg(operand,5)/2);
                tmp1(tmp1>1) = 1;
                tmp1(tmp1<-1) = -1;
                intnodes(sfg(operand,4)) = (sfg(operand,5)/2)*round((2^(sfg(operand,6)-1))*tmp1) / ...
                     (2^(sfg(operand,6)-1));
                 
                 
            case op_quant_1bit
               intnodes(sfg(operand,4)) = (intnodes(sfg(operand,3)) >=0)*2-1;
                % y = 2*(x>=0)-1;
                 
                % Other operations
            otherwise,
                error('Invalid or non-implemented operand')
        end
    end

    % Finite wordlength
    if any(wordlength)
        % Truncation
        intnodes = floor(((2^wordlength(2))*intnodes))/(2^wordlength(2));
        intoutputs = floor(((2^wordlength(2))*intoutputs))/(2^wordlength(2));
        % Overflow
        intnodes = twoscoverflow(intnodes, wordlength(1));
        intoutputs = twoscoverflow(intoutputs, wordlength(1));
    end

    if registercount & (nargout > 2)
        registers(1:registercount,time)=intnodes(registernodes)'; %'
    end

    outputs(:,time)=intoutputs;
    if nargout > 4
        nodes(:,time)=intnodes(nodelist)'; %'
    end

end



