function newschedule = cleanupschedule(schedule)
% CLEANUPSCHEUDLE Remove delay elements from schedule
%   [CLEANSCHEDULE] = CLEANUPSCHEUDLE(SCHEDULE)
%
%   SCHEDULE - Schedule with delay elements remaining (from old version of
%   getinitialschedule)

% Copyright 2004-2006
%                Electronics Systems, Dept of EE,
%                Linkoping University, SE-581 83 Linkoping
%                Sweden
%

operandmapping
sfg=schedule;
sfg(1,:)=[];
delayidx = find(sfg(:,1) == op_delay)';

delayidx = fliplr(sort(delayidx));
for d = delayidx
    sfg = deleteoperand(sfg, 'delay', sfg(d,2));
end

newschedule=[schedule(1,:);sfg];

