function newmat = inc(oldmat, row, col);
% This function is used to increase an element of a matrix.
% If the matrix does not include the specified element, 
% the matrix is extended and the element is set to 1.

matsize = size(oldmat);
if (row > matsize(1)) | (col > matsize(2))
    oldmat(row, col) = 1;
else
    oldmat(row, col) = oldmat(row, col) + 1;
end

newmat = oldmat;
