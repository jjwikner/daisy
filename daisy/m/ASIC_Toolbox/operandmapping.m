% Operand mapping
%
% Digital Signal Processing Toolbox
% Version 0.1 

% Operand types
op_in=1;
op_out=2;
op_add=3; % Innode1 Innode2 Outnode
op_sub=4; % Innode1 (-)Innode2 Outnode
op_mult=5; % Innode Outnode Coeff
op_delay=6; % Innode Outnode
op_quant=7; % Innode Outnode Bits Type
op_overflow=8; % Innode Outnode Type
op_invert=9; % Innode Outnode
op_shift=10; % Innode Outnode Bits Type
op_twoport=11; % Innode1 Innode2 Outnode1 Outnode2 Coeff Type
op_threeport=12; % Innode1 Innode2 Innode3 Outnode1 Outnode2
                 % Outnode3 Coeff1 Coeff2 Type
op_fourport=13;  % Innode1 Innode2 Innode3 Innode 4 Outnode1
                 % Outnode2 Outnode3 Outnode4 Coeff1 Coeff2 Coeff3 Type
op_butterfly=14; % Innode1 Innode2 Outnode1 Outnode2 Coeff Type
op_division=15; % Innode Outnode Coeff
op_mux=16; % Innode1 Innode2 Controlnode Outnode
op_demux=17; % Innode1 Controlnode Outnode1 Outnode2
op_mac=18; % InnodeMult InnodeAdd Outnode Coeff
op_quant2=19; % Innode Outnode Range Bits 
op_quant_1bit=20;
% Overflow types
overflow_twosc=1; % Two's complement overflow
overflow_saturation=2; % Saturation overflow

% Quantization types
quant_truncation=1; % Two's complement Truncation
quant_rounding=2;   % Rounding
quant_magnitudetruncation=3;  % Magnitude truncation

% Shift types
shift_left=1;   % Shift left (multiply by 2^bits)
shift_right=2;  % Shift right (multiply by 2^-bits)

% Adaptor types
adaptor_ser=1;  % Series adaptor
adaptor_par=2;  % parallel adaptor
adaptor_sym=3;  % Symmetric adaptor

% Butterfly types
butterfly_dit=1;  % Decimation-in-time butterfly
butterfly_dif=2;  % Decimation-in-frequency butterfly

% Copyright 2004 Electronics Systems, Dept of EE,
%                Linkoping University, SE-581 83 Linkoping
%                Sweden
% Generated from $Id: operandmapping.m,v 1.5 2004/07/01 14:57:21 oscarg Exp $
