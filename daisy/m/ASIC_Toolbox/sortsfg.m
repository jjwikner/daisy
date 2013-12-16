function [sortedsfg, precedenceform] = sortsfg(sfg);
% This function sort a signal flow graph in computable order
% 
% Input: sfg - signal flow graph
% Output: sortedsfg - signal flow graph
%         precedenceform - number of nodes in the sets of a sfg in precedence form

if nargin < 1
    error('Not enough input arguments');
end
if nargin > 1
    error('Too many input arguments');
end

% Load operand mapping
operandmapping;

% Initialize the sorted sfg with zeros
sfgsize = size(sfg);
sortedsfg = zeros(sfgsize);

% Store all available nodes in a vector
inputrows = find(sfg(:, 1) == op_in);
registerrows = find(sfg(:, 1) == op_delay);
registercount = length(registerrows);
available = [sfg(inputrows, 3); sfg(registerrows, 4)];
rowstoadd = 1:sfgsize(1);
rowstoadd([inputrows; registerrows]) = [];

% Start with all input operands
rowindex = length(inputrows);
sortedsfg(1:rowindex, :) = sfg(inputrows, :);
precedenceform = rowindex;

% Add operands that are possible to compute
prev_rowindex = -1;
while (rowindex + registercount < sfgsize(1)) & (prev_rowindex < rowindex)
    prev_rowindex = rowindex;
    rowstoremove = [];
    availtoadd = [];
    for rowid = 1:length(rowstoadd)
        operand = rowstoadd(rowid);
        numinout = getnumofinout(sfg(operand, 1));
        op_flag = 1;
        for inputid = 1:numinout(1)
            op_flag = op_flag & length(find(sfg(operand, inputid + 2) == available));
        end
        if op_flag
            rowindex = rowindex + 1;
            sortedsfg(rowindex, :) = sfg(operand, :);
            rowstoremove = [rowstoremove, rowid];
            if numinout(2)
                availtoadd = [availtoadd; sfg(operand, 3 + numinout(1): 2 + sum(numinout))'];
            end
        end
    end
    precedenceform = [precedenceform, length(rowstoremove)];
    rowstoadd(rowstoremove) = [];
    available = [available; availtoadd];
end

% Error check
if rowindex + registercount < sfgsize(1)
    error('The signal flow graph can not be sorted in computable order');
end

% End with all delay operands
prev_rowindex = -1;
while (rowindex < sfgsize(1)) & (prev_rowindex < rowindex)
    prev_rowindex = rowindex;
    rowstoremove = [];
    for rowid = 1:length(registerrows)
        operand = registerrows(rowid);
        if ~length(find(sfg(operand, 4) == sfg(registerrows, 3)))
        % True if the output is not input to a following (not yet added) delay element
            if length(find(sfg(operand, 3) == available))
                rowindex = rowindex + 1;
                sortedsfg(rowindex, :) = sfg(operand, :);
                rowstoremove = [rowstoremove, rowid];
            else
                error('The signal flow graph can not be sorted in computable order');
            end
        end
    end
    precedenceform = [precedenceform, length(rowstoremove)];
    registerrows(rowstoremove) = [];
end

% Error check
if rowindex < sfgsize(1)
    error('The signal flow graph can not be sorted in computable order');
end

