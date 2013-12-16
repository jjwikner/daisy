function mvlist = memorypartitioning(mv, readports, writeports, concurrentports)
%mvlist = memorypartitioning(mv, readports, writeports, concurrentports)
%
%mvlist = {mv1, mv2, ...}
%mv = [scheduletime; variableinfo1; variableinfo2; ...]
%variableinfo = [starttime endtime sourceoperand number port consumetime1 
%  consumeoperand1 number1 port1 consumetime2 consumeoperand2 number2 ...]
%readports is the number of read ports of the memories
%writeports is the number of write ports of the memories
%concurrentports is the number of concurrent memory accesses, i.e. require
%  min(reads, writes) <= concurrentports for all time slots and each memory

schtime = mv(1,1);

nummems = 1;

% Contains the number of allocated reads and writes for each memory and time
% slot. The matrices contain one row for each memory.
readslots = zeros(1, schtime);
writeslots = zeros(1, schtime);

numvars = size(mv, 1) - 1;
varlist = 1:numvars;

memvars = {};

while ~isempty(varlist)
    worstmems = nummems + 1;
    
    for var = varlist
        % Extract the variable
        varinfo = mv(var+1, :);

        % Determine the write and read times
        writes = varinfo(1) + 1;
        reads = varinfo(6:4:length(varinfo)) + 1;
        % Remove NaNs
        reads(isnan(reads)) = [];
        
        foundmems = 0;
        foundmem = 0;
        
        % Try to fit the variable in the already allocated memories
        for mem = 1:nummems
            % Check the number of write accesses
            if writeslots(mem, writes) + 1 > writeports 
                continue;
            end
            % Check the number of read accesses
            if numel(find(readslots(mem, reads) + 1 > readports)) > 0
                continue;
            end

            % Check the number of concurrent accesses
            accesses = [readslots(mem, :); writeslots(mem, :)];
            accesses(1, writes) = accesses(1, writes) + 1;
            accesses(2, reads) = accesses(2, reads) + 1;
            accesses = min(accesses, [], 1);
            
            if numel(find(accesses > concurrentports)) > 0
                continue;
            end
            
            % No violations, mark as possible
            foundmems = foundmems + 1;

            if foundmem == 0
                foundmem = mem;
            end
        end

        if foundmems < worstmems
            choosevar = var;
            worstmems = foundmems;
            varmem = foundmem;
        end
    end

    varinfo = mv(choosevar+1, :);
    writes = varinfo(1) + 1;
    reads = varinfo(6:4:length(varinfo)) + 1;
    reads(isnan(reads)) = [];
    
    % Allocate choosevar to memory mem or a new memory if worstmems == 0
    if worstmems == 0
        %disp(sprintf('elected %d for new memory', choosevar));
        % Allocate a new memory
        nummems = nummems + 1;
        mem = nummems;
        readslots(mem, :) = zeros(1, schtime);
        writeslots(mem, :) = zeros(1, schtime);
            
        % Update the number of reads and writes
        readslots(mem, reads) = readslots(mem, reads) + 1;
        writeslots(mem, writes) = writeslots(mem, writes) + 1;

        % Check the number of write accesses
        if writeslots(mem, writes) > writeports 
            disp(sprintf('Variable %d requires too many write accesses', var));
            mvlist = {};
            return;
        end
        % Check the number of read accesses
        if numel(find(readslots(mem, reads) > readports)) > 0
            disp(sprintf('Variable %d requires too many read accesses', var));
            mvlist = {};
            return;
        end

        % Check the number of concurrent accesses
        accesses = [readslots(mem, :); writeslots(mem, :)];
        accesses(1, writes) = accesses(1, writes) + 1;
        accesses(2, reads) = accesses(2, reads) + 1;
        accesses = min(accesses, [], 1);
        
        if numel(find(accesses > concurrentports)) > 0
            disp(sprintf('Variable %d requires too many concurrent accesses', var));
            mvlist = {};
            return;
        end
    else
        %disp(sprintf('elected %d for memory %d, choices: %d', choosevar, varmem, worstmems));
        % Found a fit in one of the allocated memories, update it

        mem = varmem;

        readslots(mem, reads) = readslots(mem, reads) + 1;
        writeslots(mem, writes) = writeslots(mem, writes) + 1;
    end

    % Update the memory variables for the memory
    if length(memvars) < mem
        memvars{mem} = [];
    end
    memvars{mem} = [memvars{mem} choosevar];

    % Remove the variable from the list of remaining variables
    varlist(varlist == choosevar) = [];
end

% Create the output cell data
mvlist = cell(1, nummems);

for mem = 1:nummems
    mvpart = zeros(length(memvars{mem}) + 1, size(mv, 2));
    mvpart(2:end,:) = mv(memvars{mem} + 1, :);
    mvpart(1,1) = schtime;
    mvpart(1,2:end) = NaN;
    mvlist{mem} = mvpart;
end

