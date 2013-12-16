function sfg = printprecedence(sfg, filename);
% This function is used to print the signal flow graph in precedence form.
% If no filename is given, the information is printed on the screen.

% Check number of input arguments
if nargin < 1
    error('Not enough input arguments');
end
if nargin > 2
    error('Too many input arguments');
end

% Set a file identifier
if nargin < 2
    fid = 1;
else
    if ~isstr(filename)
        error('The filename must be a string');
    end
    if exist(filename)
        warning(sprintf('Overwriting the file: %s', filename));
    end
    fid = fopen(filename, 'wt');
end

% Sort the signal flow graph in computable order
[sfg, precedenceform] = sortsfg(sfg);

% Loop over all operands
operand = 1;
maxlength = getmaxlength(sfg);
fprintf(fid, '-----------------------------------------------------\n');
for setid = 1:length(precedenceform)
    for opid = 1:precedenceform(setid)
        fprintf(fid, '%d.%d  ', setid, opid);
        printoperand(sfg(operand, :), fid, maxlength);
        operand = operand + 1;
    end
    fprintf(fid, '-----------------------------------------------------\n');
end

% Close the file
if fid ~= 1
    fclose(fid);
end
