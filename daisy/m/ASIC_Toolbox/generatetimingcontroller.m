function generatetimingcontroller(scheduleperiod, componentname)
% GENERATETIMINGCONTROLLER Write a timing controller to file
%
%

if nargin < 2
    componentname = 'timingcontroller';
end

fid = fopen([componentname '.vhdl'], 'w');

if ~fid
    error('Can not open file!')
end

  % Comments
  fprintf(fid,'-- Timing controller generated from the DSP toolbox\n');
  fprintf(fid,'-- Electronics Systems, http://www.es.isy.liu.se/\n\n');
  
  
  % Header files
  fprintf(fid,'library ieee;\nuse ieee.std_logic_1164.all;\n\n');
  
  % Entity
  fprintf(fid,'entity %s is\nport(\n',componentname);
  fprintf(fid,'clk, reset : in std_logic;\n');
  fprintf(fid,'state : inout integer range 0 to %d);\n',scheduleperiod-1);
  fprintf(fid,'end %s;\n\n',componentname);

  % Architecture
  fprintf(fid,'architecture generated of %s is\nbegin\n',componentname);
  fprintf(fid,'process(clk, reset)\nbegin\n');
  fprintf(fid,'if reset = ''1'' then \n state <= 0; \n');
  fprintf(fid,'elsif rising_edge(clk) then\n if state = %d then\n', scheduleperiod-1);
  fprintf(fid,'state <= 0;\nelse\n state <= state +1;\nend if;\nend if;\n');
  fprintf(fid, 'end process;\nend generated;\n');