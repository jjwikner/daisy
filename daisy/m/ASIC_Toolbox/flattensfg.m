function sfgflat = flattensfg(sfg);
% This function flatten a signal flow graph.

% Initialize
operandmapping;
nodelist = getnodelist(sfg);
node = max(nodelist) + 1;
sfgflat = [];
sfgsize = size(sfg);

% Find start id's
addid = 1; subid = 1; multid = 1; invid = 1;
for operand = 1:sfgsize(1)
    id = sfg(operand, 2);
    switch sfg(operand, 1)
    case op_add,
        if id >= addid
            addid = id + 1;
        end
    case op_sub, 
        if id >= subid
            subid = id + 1;
        end
    case op_mult, 
        if id >= multid
            multid = id + 1;
        end
    case op_invert,
        if id >= invid
            invid = id + 1;
        end
    end
end

% Loop over all operands
for operand = 1:sfgsize(1)
    op = sfg(operand, :);
    switch op(1)
    case op_twoport,
        switch op(8)
        case adaptor_ser,
            sfgflat = addrow(sfgflat, [op_add, addid, op(3), op(4), node]);
            sfgflat = addrow(sfgflat, [op_mult, multid, node, node + 1, -op(7)]);
            sfgflat = addrow(sfgflat, [op_add, addid + 1, node + 1, op(3), op(5)]);
            sfgflat = addrow(sfgflat, [op_add, addid + 2, node, op(5), node + 2]);
            sfgflat = addrow(sfgflat, [op_invert, invid, node + 2, op(6)]);
            node = node + 3;
            addid = addid + 3; multid = multid + 1; invid = invid + 1;
        case adaptor_par,
            sfgflat = addrow(sfgflat, [op_sub, subid, op(4), op(3), node]);
            sfgflat = addrow(sfgflat, [op_mult, multid, node, node + 1, -op(7)]);
            sfgflat = addrow(sfgflat, [op_add, addid, node + 1, op(4), op(6)]);
            sfgflat = addrow(sfgflat, [op_add, addid + 1, node, op(6), op(5)]);
            node = node + 2;
            addid = addid + 2; subid = subid + 1; multid = multid + 1;
        case adaptor_sym,
            sfgflat = addrow(sfgflat, [op_sub, subid, op(4), op(3), node]);
            sfgflat = addrow(sfgflat, [op_mult, multid, node, node + 1, op(7)]);
            sfgflat = addrow(sfgflat, [op_add, addid, node + 1, op(3), op(6)]);
            sfgflat = addrow(sfgflat, [op_add, addid + 1, node + 1, op(4), op(5)]);
            node = node + 2;
            addid = addid + 2; subid = subid + 1; multid = multid + 1;
        end
    case op_threeport, sfgflat = addrow(sfgflat, op); % <FIXME>
        % Innode1 Innode2 Innode3 Outnode1 Outnode2 Outnode3 Coeff1 Coeff2 Type
    case op_fourport, sfgflat = addrow(sfgflat, op); % <FIXME>
        % Innode1 Innode2 Innode3 Innode 4 Outnode1
        % Outnode2 Outnode3 Outnode4 Coeff1 Coeff2 Coeff3 Type
    case op_butterfly, sfgflat = addrow(sfgflat, op); % <FIXME>
        % Innode1 Innode2 Outnode1 Outnode2 Coeff Type
    case op_mac,
        sfgflat = addrow(sfgflat, [op_mult, multid, op(3), node, op(6)]);
        sfgflat = addrow(sfgflat, [op_add, addid, node, op(4), op(5)]);
        node = node + 1;
        addid = addid + 1; multid = multid + 1;
    otherwise, sfgflat = addrow(sfgflat, op);
    end
end

