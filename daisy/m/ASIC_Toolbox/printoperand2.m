function printoperand2(operand, fid);

% Check number of input arguments
if nargin < 1
   error('Not enough input arguments');
end
if nargin > 2
   error('Too many input arguments');
end

% If not defined, set a file identifier
if nargin < 2
   fid = 1;
end

% Load operand mapping
operandmapping;

% Print the operand
d = 0; g = 1; s = 2;
adaptor = 0;
switch operand(1)
   case op_in,
      op_str = 'in       '; argformat = [d d];
   case op_out,
      op_str = 'out      '; argformat = [d d];
   case op_add,
      op_str = 'add      '; argformat = [d d d d];
   case op_sub,
      op_str = 'sub      '; argformat = [d d d d];
   case op_mult,
      op_str = 'mult     '; argformat = [d d d g];
   case op_division,
      op_str = 'division '; argformat = [d d d g];
   case op_delay,
      op_str = 'delay    '; argformat = [d d d];
   case op_invert,
      op_str = 'invert   '; argformat = [d d d];
   case op_shift,
      op_str = 'shift    '; argformat = [d d d d s];
      switch operand(6)
         case shift_left, type_str = 'left';
         case shift_right, type_str = 'right';
         otherwise, error('Unknown shift type');
      end
   case op_twoport,
      op_str = 'twoport  '; argformat = [d d d d d g s];
      adaptor = 1;
   case op_threeport,
      op_str = 'threeport'; argformat = [d d d d d d d g g s];
      adaptor = 1;
   case op_fourport,
      op_str = 'fourport '; argformat = [d d d d d d d d d g g g s];
      adaptor = 1;
   case op_butterfly,
      op_str = 'butterfly'; argformat = [d d d d d g s];
      switch operand(8)
         case butterfly_dit, type_str = 'dit';
         case butterfly_dif, type_str = 'dif';
         otherwise, error('Unknown butterfly type');
      end
   case op_quant,
      op_str = 'quant    '; argformat = [d d d d s];
      switch operand(6)
         case quant_truncation, type_str = 'truncation';
         case quant_rounding, type_str = 'rounding';
         case quant_magnitudetruncation, type_str = 'magnitudetruncation';
         otherwise, error('Unknown quantization type');
      end
   case op_overflow,
      op_str = 'overflow '; argformat = [d d d s];
      switch operand(5)
         case overflow_twosc, type_str = 'twosc';
         case overflow_saturation, type_str = 'saturation';
         otherwise, error('Unknown overflow type');
      end
   case op_mux,
      op_str = 'mux      '; argformat = [d d d d d];
   case op_demux,
      op_str = 'demux    '; argformat = [d d d d d];
   case op_mac,
      op_str = 'mac      '; argformat = [d d d d g];
   otherwise,
      error('Unknown operand');
end

if adaptor
   switch operand(1 + length(argformat))
      case adaptor_ser, type_str = 'series';
      case adaptor_par, type_str = 'parallel';
      case adaptor_sym, type_str = 'symmetric';
      otherwise, error('Unknown adaptor type');
   end
end

numinout = getnumofinout(operand(1));

fprintf(fid, '%s', op_str);
for argid = 1:length(argformat)
   tab = 1;
   if argid == 1
      arg_str = 'id: ';
   elseif numinout(1) & (argid == 2)
      arg_str = 'in: ';
   elseif numinout(2) & (argid == 2 + numinout(1))
      arg_str = 'out: ';
   elseif (argformat(argid) == d) & (argid == 2 + sum(numinout))
      arg_str = 'bits: ';
   elseif (argformat(argid) == g) & (argid == 2 + sum(numinout))
      arg_str = 'coeff: ';
   elseif argformat(argid) == s
      arg_str = 'type: ';
   else
      arg_str = ', ';
      tab = 0;
   end
   if tab
      fprintf(fid, '\t')
   end
   if argformat(argid) == d
      fprintf(fid, '%s%d', arg_str, operand(argid + 1));
   elseif argformat(argid) == g
      fprintf(fid, '%s%0.3g', arg_str, operand(argid + 1));
   elseif argformat(argid) == s
      fprintf(fid, '%s%s', arg_str, type_str);
   end
end
fprintf(fid, '\n');
