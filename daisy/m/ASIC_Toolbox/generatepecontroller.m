function generatepecontroller(peassignment, schedule, number, bits)
% GENERATEPECONTROLLER Write a PE controller to file
%
%

if nargin < 3
    error('Must specify an identifier')
end

if nargin < 4
    bits = 10;
end

if iscell(peassignment)
    error('Input one peassignment, not a list')
end

% Extract information
petype=peassignment(1);
peids=peassignment(2:length(peassignment));
peschedule=[];
scheduletime=schedule(1,1);
coefficients=[];
starttimes=[];
for peid = peids
    perows=find(schedule(:,1)==petype);
    perow=find(schedule(perows,2)==peid);
    peschedulerow=schedule(perows(perow),:);
    peschedule=[peschedule;peschedulerow];
    starttime = peschedulerow(length(peschedulerow)-2);
    starttimes=[starttimes starttime];
    coefficients=[coefficients;getcoefficients(peschedulerow)];
end

if ~isempty(coefficients)
    coeffmatrix=real2twoc(coefficients,bits);
    si=size(coeffmatrix);
end

% Open file
componentname = sprintf('pecontroller%d',number);
fid = fopen([componentname '.vhdl'], 'w');

if fid == -1
    error('Can not open file!')
end

% Comments
fprintf(fid,'-- PE controller generated from the DSP toolbox\n');
fprintf(fid,'-- Electronics Systems, http://www.es.isy.liu.se/\n\n');


% Header files
fprintf(fid,'library ieee;\nuse ieee.std_logic_1164.all;\n\n');

% Entity
fprintf(fid,'entity %s is\nport(\n',componentname);
fprintf(fid,'state : in integer range 0 to %d;\n',scheduletime-1);
if ~isempty(coefficients)
    fprintf(fid,'coefficient : out std_logic_vector(0 to %d);\n',bits-1);
end
fprintf(fid, 'start : out std_logic);\n');
fprintf(fid,'end %s;\n\n',componentname);

% Architecture
fprintf(fid,'architecture generated of %s is\nbegin\n',componentname);
if ~isempty(coefficients)
    fprintf(fid,'with state select\ncoefficient <= \n');
    for row = 1:si(1)
        fprintf(fid,'\"');
        for bit = 1:si(2)
            fprintf(fid,'%d',coeffmatrix(row,bit));
        end
        fprintf(fid,'\" when %d,\n',starttimes(row));
    end

    fprintf(fid,'\"');
    for bit = 1:si(2)
        fprintf(fid,'-');
    end
    fprintf(fid,'\" when others;\n');
end

fprintf(fid,'with state select\nstart <= \n');
for k=starttimes
    fprintf(fid, '''1'' when %d,\n', k);
end
fprintf(fid,'''0'' when others;\n\n');

fprintf(fid, 'end generated;\n');