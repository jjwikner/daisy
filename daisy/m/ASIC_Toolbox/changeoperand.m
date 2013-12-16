function sfg = changeoperand(sfg, operandname, number, operanddata, operandtype);
% This function change an operand in a signal flow graph.
% The properties that can be changed is the data and the type of the
% operand, i.e., input and output nodes can not be changed.
% If only the type is to be changed operanddata can either be an empty
% vector or simply be left out.
% 
% Inputs: sfg - signal flow graph
%         operandname - operand name as a string
%         number - operand ID number
%         operanddata - operand data in a row vector
%         operandtype - operand type as a string
% Output: sfg - updated signal flow graph

if nargin < 4
   error('Not enough input arguments');
end
if nargin > 5
   error('Too many input arguments');
end
if nargin == 4
   if ischar(operanddata)
      operandtype = operanddata;
      operanddata = [];
   else
      operandtype = [];
   end
end

% Load operand mapping
operandmapping;

% Find operation
switch lower(operandname)
   case 'in', op = 0; % op_in;
   case 'out', op = 0; % op_out;
   case 'add', op = 0; % op_add;
   case 'sub', op = 0; % op_sub;
   case 'mult', op = op_mult; % DATA
   case 'division', op = op_division; % DATA
   case 'delay', op = 0; % op_delay;
   case 'invert', op = 0; % op_invert;
   case 'shift', op = op_shift; % DATA, TYPE
   case 'twoport', op = op_twoport; % DATA, TYPE
   case 'threeport', op = op_threeport; % DATA, TYPE
   case 'fourport', op = op_fourport; % DATA, TYPE
   case 'butterfly', op = op_butterfly; % DATA, TYPE
   case 'quant', op = op_quant; % DATA, TYPE
   case 'overflow', op = op_overflow; % TYPE
   case 'mux', op = 0; % op_mux;
   case 'demux', op = 0; % op_demux;
   case 'mac', op = op_mac; % DATA
   otherwise, error('Unknown operand');
end
if ~op
   error('The %s operand can not be changed!', operandname);
end

% Find row in sfg
id_op = find(op == sfg(:, 1));
id_no = find(number == sfg(:, 2));
index = 1;
while ~length(find(id_op(index) == id_no))
   index = index + 1;
   if index > length(id_op)
      error('The operand could not be found!');
   end
end
row_op = id_op(index);

% Get format of arguments
d = 0; g = 1; s = 2;
argformat = getargformat(op);
id_arg = (3 + sum(getnumofinout(op))):(1 + length(argformat));
type_p = length(find(s == argformat));
data_p = length(id_arg) - type_p;

% Operand data
data = [];
if length(operanddata)
   if ~data_p
      warning('The data can not be changed for the %s operand!', operandname);
   else
      data = operanddata;
      if length(data) ~= data_p
         error('The input argument operanddata contain wrong number of elements!');
      end
   end
elseif data_p
   data = sfg(row_op, id_arg(1:end - type_p));
end

% Operand type
type = [];
if length(operandtype)
   if ~type_p
      warning('The type can not be changed for the %s operand!', operandname);
   else
      switch op
         case op_shift,
            switch lower(operandtype)
               case 'left', type = shift_left;
               case 'right', type = shift_right;
               otherwise, error('Unknown shift type');
            end
         case {op_twoport, op_threeport, op_fourport},
            switch lower(operandtype)
               case {'series', 'ser'}, type = adaptor_ser;
               case {'parallel', 'par'}, type = adaptor_par;
               case {'symmetric', 'sym'}, type = adaptor_sym;
               otherwise, error('Unknown adaptor type');
            end
         case op_butterfly,
            switch lower(operandtype)
               case 'dit', type = butterfly_dit;
               case 'dif', type = butterfly_dif;
               otherwise, error('Unknown butterfly type');
            end
         case op_quant,
            switch lower(operandtype)
               case {'truncation', 'trunc'}, type = quant_truncation;
               case {'rounding', 'round'}, type = quant_rounding;
               case {'magnitudetruncation', 'magtrunc'}, type = quant_magnitudetruncation;
               otherwise, error('Unknown quantization type');
            end
         case op_overflow,
            switch lower(operandtype)
               case 'twosc', type = overflow_twosc;
               case {'saturation', 'sat'}, type = overflow_saturation;
               otherwise, error('Unknown overflow type');
            end
         otherwise, error('Unknown operand');
      end
   end
elseif type_p
   type = sfg(row_op, id_arg(end));
end

% Update operand
sfg(row_op, id_arg) = [data, type];

