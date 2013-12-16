function [loops, operands] = findloops(sfg);
% This function is used to find all loops in the signal flow graph.
% Each row in 'loops' corresponds to one loop and contain all nodes in the loop.
% Each row in 'operands' contain the number of each operand in the loop.

MAXOUT = 4;

errorlist = checknodes(sfg, 1);
if length(errorlist)
    error('The signal flow graph is incorrect');
end

% Load operand mapping
operandmapping;

nodelist = getnodelist(sfg);
M = length(nodelist);
[innodes, outnodes] = getinoutnodes(sfg);

% For each node, search the sfg in a tree manner
% * if an output is reached, that path is not a loop
% * if the start node is reached, that path is a loop
loops = [];
operands = [];
for node_id = 1:M
    step = 1;
    opid = 1;
    node = NaN*ones(M, MAXOUT);
    node(1, 1) = nodelist(node_id);
    node_col = ones(M, 1);
    way = length(find(node(1, 1) == innodes));
    
    % Loop until all ways are explored
    while sum(way)
        % Find opid
        op = sfg(opid(step), :);
        numinout = getnumofinout(op(1));
        opin = op(3:2 + numinout(1));
        while ~length(find(node(step, node_col(step)) == opin))
            opid(step) = opid(step) + 1;
            op = sfg(opid(step), :);
            numinout = getnumofinout(op(1));
            opin = op(3:2 + numinout(1));
        end
        
        % Find outputs of the operand
        if ~numinout(2) % output reached
            opout = NaN;
            back_p = 1;
        else
            opout = op(3 + numinout(1):2 + sum(numinout));
            back_p = 0;
        end
        
        % Is it a loop?
        if length(find(node(1, 1) == opout))
            back_p = 1;
            % Find the nodes included in the loop
            loop_nodes = zeros(1, step);
            for i = 1:step
                loop_nodes(i) = node(i, node_col(i));
            end
            % Check if the loop is already found
            numofloops = size(loops);
            new_loop_p = 1;
            if numofloops(1) & (numofloops(2) >= step)
                sorted_loop = sort(loop_nodes);
                for i = 1:numofloops(1)
                    if new_loop_p & (sum(sorted_loop == sort(loops(i, 1:step))) == step)
                        new_loop_p = 0;
                    end
                end
            end
            % If it is a new loop, add to 'loops' and 'operands'
            if new_loop_p
                loops = addrow(loops, loop_nodes);
                for i = 1:step
                    operands = inc(operands, numofloops(1) + 1, sfg(opid(i), 1));
                end
            end
        end
        
        % Check for other loops
        if ~back_p
            for i = 1:length(opout)
                if length(find(node == opout(i)))
                    % Loop without including the first node
                    back_p = 1;
                end
            end
        end
        
        if back_p % Go back
            way(step) = way(step) - 1;
            % Go backwards until another path is found
            while ~way(step) & isnan(node(step, node_col(step) + 1)) & (step > 1)
                % Reset
                opid(step) = 1;
                node(step, :) = NaN;
                node_col(step) = 1;
                % Update
                step = step - 1;
                way(step) = way(step) - 1;
            end
            % Find the fulfilled condition
            if way(step)
                % Another path is possible
                opid(step) = opid(step) + 1;
            elseif ~isnan(node(step, node_col(step) + 1))
                % Another output from an operand possible
                opid(step) = 1;
                node_col(step) = node_col(step) + 1;
                way(step) = length(find(node(step, node_col(step)) == innodes));
            end
        else % Continue the path
            step = step + 1;
            opid(step) = 1;
            node(step, 1:length(opout)) = opout;
            node_col(step) = 1;
            way(step) = length(find(node(step, 1) == innodes));
        end
    end
end


