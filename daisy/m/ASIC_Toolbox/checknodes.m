function errorlist = checknodes(sfg, p_print);
% This function checks so that
% * outputs are not driven by more than one operand
% * there are no loose nodes
% 
% For a correct signal flow graph the result vector is empty

% Check input arguments
if nargin < 1
    error('Not enough input arguments');
end
if nargin < 2
    p_print = 1;
end

[innodes, outnodes] = getinoutnodes(sfg);

% Check that all outputs are not driven by more than one operand
id = find(~diff(outnodes));
e1 = outnodes(id);
e1(find(~diff(e1))) = [];
if p_print & length(e1)
    disp(sprintf('ERROR! Nodes driven by more than one operand: %s', int2str(e1)));
end

% Check that there are no loose nodes
outnodes(id) = [];
innodes(find(~diff(innodes))) = [];
e2 = [];
for i = 1:length(outnodes)
    if ~max(outnodes(i) == innodes)
        e2 = [e2, outnodes(i)];
    end
end
if p_print & length(e2)
    disp(sprintf('ERROR! Unused output nodes: %s', int2str(e2)));
end
e3 = [];
for i = 1:length(innodes)
    if ~max(innodes(i) == outnodes)
        e3 = [e3, innodes(i)];
    end
end
if p_print & length(e3)
    disp(sprintf('ERROR! Unavailable input nodes: %s', int2str(e3)));
end

errorlist = sort([e1, e2, e3]);
errorlist(find(~diff(errorlist))) = [];
