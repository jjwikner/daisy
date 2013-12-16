function cellassignment = leftedgealgorithm(mv)
%cellassignment = leftedgealgorithm(mv)
%
%mv = [scheduletime; variableinfo1; variableinfo2; ...]

schtime = mv(1,1);

vars = mv(2:end, 1:2);
vars(:, 2) = mod(vars(:, 2) - vars(:, 1), schtime);

cells = leftedge(schtime, vars);

cellassignment = {};

for i = 1:length(cells)
    cellassignment{i} = [mv(1, :); mv(1 + cells{i}, :)];
end

