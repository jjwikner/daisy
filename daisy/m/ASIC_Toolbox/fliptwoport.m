function [newschedule] = fliptwoport(schedule, operandnumber)
% FLIPTWOPORT flip symmetric twoport adaptor
%
%   NEWSFG = FLIPTWOPORT(SFG,NUMBER)
%   NEWSCHEDULE = FLIPTWOPORT(SCHEDULE,NUMBER)
%
%   Change the order of the input and output ports for symmetric twoport
%   adaptors. Will also negate the coefficient to keep the same
%   functionality. NUMBER is one ot more operandnumbers.

operandmapping
newschedule=schedule;
for opid = operandnumber
    lineid = find(newschedule(:,1) == op_twoport);
    lineid = lineid(find(newschedule(lineid,2) == opid));

    % Should really test for symmetric adaptors as well...
    switch length(lineid)
        case 0, 
            error(sprintf('Can not find twoport number %d.', opid))
        case 1,
            newschedule(lineid,3:4) = fliplr(newschedule(lineid,3:4));
            newschedule(lineid,5:6) = fliplr(newschedule(lineid,5:6));
            newschedule(lineid,7) = -newschedule(lineid,7);
        otherwise,
            error(sprintf('Adaptor %d not uniquely defined.', opid))
    end
end
            