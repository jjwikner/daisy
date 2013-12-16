function newmat = addrow(oldmat, newrow);
% This function is used to add a row to a matrix.
% If the size of the matrix and the row does not agree, 
% the empty elements are filled with NaN.

matsize = size(oldmat);
col = length(newrow);
if matsize(2) < col
    oldmat = [oldmat, NaN*ones(matsize(1), col - matsize(2))];
elseif matsize(2) > col
    newrow = [newrow, NaN*ones(1, matsize(2) - col)];
end

newmat = [oldmat; newrow];
