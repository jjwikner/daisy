function generatememorycontroller(cellassignment, number)
% GENERATEMEMORYCONTROLLER Write a memory controller to file
%
%

if nargin < 2
    error('Must specify an identifier')
end

% Extract information
accesslist=[];   % [time r=1w=0 cell]
for k = 1:length(cellassignment)  % For each cell
    mv=cellassignment{k};
    s = size(mv);
    for m = 2:s(1)   % For each row
        memrow = mv(m,:);
        % Write
        accesslist = [accesslist;[memrow(1) 0 k]];
        % Read
        for n = 6:4:s(2)
            if ~isnan(memrow(n)) % Check for NaN
                accesslist = [accesslist;[memrow(n) 1 k]];
            end
        end
    end
end

scheduletime=mv(1,1);

accesslist = sortrows(accesslist);
meminstr=size(accesslist,1);


% Open file
componentname = sprintf('memorycontroller%d',number);
fid = fopen([componentname '.vhdl'], 'w');

if fid == -1
    error('Can not open file!')
end

% Comments
fprintf(fid,'-- Memory controller generated from the DSP toolbox\n');
fprintf(fid,'-- Electronics Systems, http://www.es.isy.liu.se/\n\n');


% Header files
fprintf(fid,'library ieee;\nuse ieee.std_logic_1164.all;\n\n');

% Entity
fprintf(fid,'entity %s is\nport(\n',componentname);
fprintf(fid,'state : in integer range 0 to %d;\n',scheduletime-1);
fprintf(fid,'adress : out integer range 0 to %d;\n',length(cellassignment)-1);
fprintf(fid, 'enable, readwrite : out std_logic);\n');
fprintf(fid,'end %s;\n\n',componentname);

% Architecture
fprintf(fid,'architecture generated of %s is\nbegin\n',componentname);
fprintf(fid,'with state select\nadress <= \n');
for k=1:meminstr
    fprintf(fid, '%d when %d,\n', accesslist(k,3)-1, accesslist(k,1));
end
fprintf(fid,'0 when others;\n\n');

fprintf(fid,'with state select\nreadwrite <= \n');
for k=1:meminstr
    fprintf(fid, '''%d'' when %d,\n', accesslist(k,2), accesslist(k,1));
end
fprintf(fid,'''-'' when others;\n\n');

fprintf(fid,'with state select\nenable <= \n');
for k=1:meminstr
    fprintf(fid, '''%d'' when %d,\n', 1, accesslist(k,1));
end
fprintf(fid,'''0'' when others;\n\n');

fprintf(fid, 'end generated;\n');