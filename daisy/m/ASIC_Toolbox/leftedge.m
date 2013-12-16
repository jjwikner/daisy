function cells = leftedge(schtime, variablelist)
%cells = leftedge(schtime, variablelist)
%
%variablelist = [starttime1 lifetime1; starttime2 lifetime2; ...]
%cells = {[variable1 variable2 ...] [variable3 ...] ...}

[slist, ind] = sortrows(variablelist, [1 -2]);

cell = 0;
cells = {};

while length(ind) > 0
    % Remaining variables, add a new cell
    cell = cell + 1;
    % Add the first variable in the sorted list
    cells{cell} = ind(1);
    first = slist(1,1);
    last = first + slist(1,2);
    ind(1) = [];
    slist(1,:) = [];

    while true
        % Consider variables with start time after the end time of the 
        % last added variable, and life time less than the available
        % time between the last added variable and the start time.
        choices = slist(:, 1) >= last;
        choices = choices .* (slist(:, 2) <= mod(first - slist(:, 1), schtime));

        t = find(choices == 1);
        if(isempty(t))
            % No available choices
            break;
        end

        choice = t(1);

        cells{cell} = [cells{cell} ind(choice)];
        last = slist(choice, 1) + slist(choice, 2);
        ind(choice) = [];
        slist(choice, :) = [];

        if(last >= schtime)
            % Wrap around, cell is full
            break;
        end
    end
end

