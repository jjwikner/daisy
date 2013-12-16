function peassignment = getpeassignment(schedule)
%peassignment = getpeassignment(schedule)

% Operand types that we don't assign to a PE
noassign = [1 2 6];

schtime = schedule(1,1);

% Remove types that shall not be assigned to a PE
processes = schedule(2:end, :);
for i = noassign
    processes(processes(:, 1) == i, :) = [];
end

% Sort according to operand type
processes = sortrows(processes, 1);

peassignment = {};

pes = 0;

% Assign the proccesses to PEs
while(~isempty(processes))
    petype = processes(1,1);

    % Extract the processes of this type
    peprocesses = processes(processes(:, 1) == petype, :);
    processes(processes(:, 1) == petype, :) = [];
    
    % Probably there is a better way to do this
    processidcol = 2;
    starttimecol = size(peprocesses, 2) - 2;
    exectimecol = size(peprocesses, 2);

    vars = peprocesses(:, [starttimecol exectimecol]);

    cells = leftedge(schtime, vars);

    for i = 1:length(cells)
        pes = pes + 1;
        peassignment{pes} = [petype peprocesses(cells{i}, processidcol)'];
    end
end

