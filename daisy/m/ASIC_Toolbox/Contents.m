% Digital Signal Processing Toolbox
% Version 0.1 
%
% Signal flow graph manipulation
%   sortsfg        - Sort SFG in computational order
%   addoperand     - Add operand to SFG
%   insertoperand  - Inserts operand (with one input and one output) into SFG
%   replacemac     - Replace MAC with multiply and add
%   flattensfg     - Replace adaptors, MACs etc   
%
% Simulation
%   simulate       - Simulate SFG
%   impulseresponse - Simulate SFG with an input impulse
%   stepresponse   - Simulate SFG with an input step
%
% Display
%   printsfg       - Display SFG in more readable form
%   printprecedence - Display precedence relationship in readable form
%   printcomplexity - Display operation count
%   plotprecedence - Plot SFG in precedence form
%
% Built in signal flow graph generators
%   sfg_firfilter  - FIR filter SFG generator
%   sfg_macfirfilter  - FIR filter SFG generator using MACs
%   sfg_LWDF       - Lattice Wave Digital Filter generator
%
% Internal functions
%   getnodelist    - Get a list of all used nodes in SFG
%   operandmapping - Initiates internal variable names
%   getfreenode    - Get one (or more) unused node numbers

% Copyright 2004-2006 Electronics Systems, Dept of EE,
%                Linkoping University, SE-581 83 Linkoping
%                Sweden
% Generated from $Id: Contents.m,v 1.4 2006/01/24 15:21:06 oscarg Exp $

