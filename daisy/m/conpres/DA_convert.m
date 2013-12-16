function analog_values = DA_convert(digital_values, MSB_position, scaling, dead_time)

% DA_CONVERT	DA_convert(digital_values, MSB_position, scaling)
%	
%		The function takes as input:
%		digital_values  : Matrix with digital codes
%		MSB_position	: Indicates where the MSB is positioned. 
%				  A 1 indicates in the first bit, 
%				  a 0 indicates in the last bit.
%		scaling		: Scaling factor
%		dead_time	: Number of dead bits through the converter
%
%		As output a vector with the same length as the digital_values matrix.
%
%		The analog value of the digital code is given as: 
%		scaling * (1*LSB+2*(LSB+1)+...+(-2^(no_of_bits-1))*MSB) (MSB is a sign bit)
%
% EXAMPLE:	analog_values = DA_convert(vector_with_digital_codes,0,0.0004/511,50);
%
%		JJW Product, 960615
%


[rows, no_of_bits] = size(digital_values);

% A vector consisting of scaling * [1 2 4 8 ... -2^(no_of_bits-1)] is created.

for i = 1:no_of_bits
  two_values(MSB_position*(no_of_bits+1)-(2*MSB_position-1)*i) = scaling*(2^(i-1));
end;

k = no_of_bits*(1-MSB_position) + MSB_position;

two_values(k) = -two_values(k);

analog_values(1:rows-dead_time) = digital_values(dead_time+1:rows,1:no_of_bits)*two_values';









