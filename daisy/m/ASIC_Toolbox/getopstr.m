function [op_str, type_str] = getopstr(operand);

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
adaptor = 0;
type_str = '';
switch operand(1)
   case op_in, op_str = 'in       ';
   case op_out, op_str = 'out      ';
   case op_add, op_str = 'add      ';
   case op_sub, op_str = 'sub      ';
   case op_mult, op_str = 'mult     ';
   case op_division, op_str = 'division ';
   case op_delay, op_str = 'delay    ';
   case op_invert, op_str = 'invert   ';
   case op_shift, op_str = 'shift    ';
      if nargout > 1
         switch operand(6)
            case shift_left, type_str = 'left';
            case shift_right, type_str = 'right';
            otherwise, error('Unknown shift type');
         end
      end
   case op_twoport, op_str = 'twoport  ';
      adaptor = 1;
   case op_threeport, op_str = 'threeport';
      adaptor = 1;
   case op_fourport, op_str = 'fourport ';
      adaptor = 1;
   case op_butterfly, op_str = 'butterfly';
      if nargout > 1
         switch operand(8)
            case butterfly_dit, type_str = 'dit';
            case butterfly_dif, type_str = 'dif';
            otherwise, error('Unknown butterfly type');
         end
      end
   case op_quant, op_str = 'quant    ';
      if nargout > 1
         switch operand(6)
            case quant_truncation, type_str = 'truncation';
            case quant_rounding, type_str = 'rounding';
            case quant_magnitudetruncation, type_str = 'magnitudetruncation';
            otherwise, error('Unknown quantization type');
         end
      end
   case op_overflow, op_str = 'overflow ';
      if nargout > 1
         switch operand(5)
            case overflow_twosc, type_str = 'twosc';
            case overflow_saturation, type_str = 'saturation';
            otherwise, error('Unknown overflow type');
         end
      end
   case op_mux, op_str = 'mux      ';
   case op_demux, op_str = 'demux    ';
   case op_mac, op_str = 'mac      ';
   otherwise, error('Unknown operand');
end

if nargout > 1
   if adaptor
      switch operand(1 + length(getargformat(operand)))
         case adaptor_ser, type_str = 'series';
         case adaptor_par, type_str = 'parallel';
         case adaptor_sym, type_str = 'symmetric';
         otherwise, error('Unknown adaptor type');
      end
   end
end
