function values = getnodevalues(inputvalues, mapping, identifier)
% values = getnodevalues(inputvalues, mapping, identifier)

row = find(mapping == identifier);

switch length(row)
    case 0,
        error('Identifier not found')
    case 1,
        values=inputvalues(row,:);
    otherwise,
        error('Only one identifier can be used')
end