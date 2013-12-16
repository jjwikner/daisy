function sfg = plotprecedence(sfg, title_str);
% This function is used to plot the signal flow graph in precedence form.

% Check number of input arguments
if nargin < 1
    error('Not enough input arguments');
end
if nargin > 2
    error('Too many input arguments');
end
% Set a title
if nargin < 2
    title_str = 'SFG in precedence form';
else
    if ~isstr(title_str)
        error('The title must be a string');
    end
end

% Sort the signal flow graph in computable order
[sfg, precedenceform] = sortsfg(sfg);
% Load operand mapping
operandmapping;
% Constants
op_str = '..+x>ov^*<ppph*dds';
op_col = 'brmbrmbrmbrmbrmbrm';
dx = -0.1; dy = 0.2;
% Figure
figure; hold on;
title(title_str);

% Inputs and delays
inputrows = find(sfg(:, 1) == op_in);
registerrows = find(sfg(:, 1) == op_delay);
available = [sfg(inputrows, 3); sfg(registerrows, 4)];
numavail = length(available);
y_avail = 1:numavail;
for operand = 1:numavail
    if operand <= length(inputrows)
        op = op_in;
        id_str = strcat('x', int2str(sfg(inputrows(operand), 2)));
    else
        op = op_delay;
        id_str = int2str(available(operand));
    end
    plot(1, y_avail(operand), [op_col(op), op_str(op)]);
    text(1 + dx, y_avail(operand) + dy, id_str);
end
% Plot rectangle
plot([0.7, 1.3], [0.5, 0.5], 'k');
plot([0.7, 1.3], numavail + [0.5, 0.5], 'k');
plot([1.3, 1.3], [0.5, numavail + 0.5], 'k');
plot([0.7, 0.7], [0.5, numavail + 0.5], 'k');

% Operands
operand = length(inputrows) + 1;
% Node usage
nodelist = getnodelist(sfg);
[innodes, outnodes] = getinoutnodes(sfg);
M = length(nodelist);
nodeusage = zeros(M, 1);
for node = 1:M
    nodeusage(node) = length(find(nodelist(node) == innodes));
end
% Loop over all sets
for setid = 2:length(precedenceform)
    % Get info about the set
    numop = precedenceform(setid);
    numinout = zeros(numop, 2);
    oplist = zeros(numop, 1);
    removeid = [];
    for opid = 1:numop
        op = sfg(operand, 1);
        if (op ~= op_out) & (op ~= op_delay)
            oplist(opid) = operand;
            numinout(opid, :) = getnumofinout(op);
            for inid = 1:numinout(opid, 1)
                id = find(nodelist == sfg(operand, 2 + inid));
                nodeusage(id) = nodeusage(id) - 1;
            end
        else
            removeid = [removeid, opid];
        end
        operand = operand + 1;
    end
    numop = numop - length(removeid);
    numinout(removeid, :) = [];
    oplist(removeid) = [];
    % Check if empty set
    if numop
        % Sort the set
        y_in1 = zeros(1, numop);
        for opid = 1:numop
            id = [];
            for inid = 1:numinout(opid, 1)
                id = [id, find(available == sfg(oplist(opid), 2 + inid))];
            end
            y_in1(opid) = min(y_avail(id));
        end
        oplist_temp = oplist;
        numinout_temp = numinout;
        for opid = 1:numop
            [dummy, index] = min(y_in1);
            y_in1(index) = inf;
            oplist(opid) = oplist_temp(index);
            numinout(opid, :) = numinout_temp(index, :);
        end
        % X values
        x = 2*(setid - 1); x_in = x - 1; x_out = x + 1;
        % Bypass of nodes that are required later
        y_used = [];
        for id = 1:length(available)
            if nodeusage(find(nodelist == available(id)))
                plot([x_in, x_out], [y_avail(id), y_avail(id)], 'b--');
                y_used = [y_used, y_avail(id)];
            end
        end
        % Y values
        y_out = 1:(sum(numinout(:, 2)) + length(y_used));
        if length(y_used)
            y_out(y_used(find(y_used <= max(y_out)))) = [];
            y_out(sum(numinout(:, 2)) + 1:length(y_out)) = [];
        end
        y = y_out(1:numop);
        y_avail = [y_avail, y_out];
        % Plot the set
        yid = 1;
        for opid = 1:numop
            row = oplist(opid);
            op = sfg(row, 1);
            % Plot operand
            plot(x, y(opid), [op_col(op), op_str(op)]);
            text(x + dx, y(opid) + dy, int2str(sfg(row, 2)));
            % Plot inputs
            for inid = 1:numinout(opid, 1)
                id = find(available == sfg(row, 2 + inid));
                plot([x_in, x], [y_avail(id), y(opid)], op_col(op));
            end
            % Plot outputs
            for outid = 1:numinout(opid, 2)
                plot([x, x_out], [y(opid), y_out(yid)], op_col(op));
                plot(x_out, y_out(yid), [op_col(op), '.']);
                node = sfg(row, 2 + numinout(opid, 1) + outid);
                text(x_out + dx, y_out(yid) + dy, int2str(node));
                yid = yid + 1;
            end
            % Add to available
            available = [available; sfg(row, 3 + numinout(opid, 1):2 + sum(numinout(opid, :)))'];
        end
        % Plot rectangle
        y_max = max(y_out); y_min = min(y_out);
        plot(x_out + [-0.3, 0.3], y_min - [0.5, 0.5], 'k');
        plot(x_out + [-0.3, 0.3], y_max + [0.5, 0.5], 'k');
        plot(x_out + [0.3, 0.3], [y_min - 0.5, y_max + 0.5], 'k');
        plot(x_out - [0.3, 0.3], [y_min - 0.5, y_max + 0.5], 'k');
    end
end

% Outputs and delays
x = x + 2; x_in = x - 1; x_out = x + 1;
outputrows = find(sfg(:, 1) == op_out);
rows = [outputrows; registerrows];
y_used = [];
for operand = 1:length(rows)
    oprow = sfg(rows(operand), :);
    y_in = y_avail(find(available == oprow(3)));
    y = y_in;
    while length(y_used) & length(find(y_used == y))
        y = y + 1;
    end
    if operand <= length(outputrows)
        op = op_out; id_out = 3;
        id_str = strcat('y', int2str(oprow(2)));
    else
        op = op_delay; id_out = 4;
        id_str = int2str(oprow(id_out));
    end
    plot([x_in, x_out], [y_in, y], op_col(op));
    plot(x_out, y, [op_col(op), op_str(op)]);
    text(x_out + dx, y + dy, id_str);
    y_used = [y_used, y];
end
% Plot rectangle
y_max = max(y_used); y_min = min(y_used);
plot(x_out + [-0.3, 0.3], y_min - [0.5, 0.5], 'k');
plot(x_out + [-0.3, 0.3], y_max + [0.5, 0.5], 'k');
plot(x_out + [0.3, 0.3], [y_min - 0.5, y_max + 0.5], 'k');
plot(x_out - [0.3, 0.3], [y_min - 0.5, y_max + 0.5], 'k');

% Set axis scaling
axis([0, (x_out + 1), 0, (max([y_avail, y_max]) + 1)]);

