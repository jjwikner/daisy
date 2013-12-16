function[newschedule]=setscheduletime(schedule,scheduletime);
% Sets the schedule time

% Ugly fix - Oscar 2007-02-15
newschedule=schedule;
newschedule(1,1)=scheduletime;

%To be ensured that schedule time is greater than 
%max start time
%schedule = sch;
% 
% size_schedule=size(schedule);
% st = zeros(size_schedule(1)-1,1);
% time_columns_index=size_schedule(2)-4;
% timecol = [time_columns_index:1:size_schedule(2)];
% st = schedule(2:size_schedule(1),timecol(3));
% 
% % Get the starttimes of only arith ops.
% for i=2:size_schedule(1)
%     if ((schedule(i,1) == 6)|(schedule(i,1) == 2)) 
%         st_arithops(i-1) = 0;  
%     else
%         st_arithops(i-1)=st(i);
%     end
% end
% 

% if scheduletime <= max(
%     error('There are instr which start after the scheduletime')
% else
%     newschedule = schedule;
%     newschedule(1,1)=scheduletime;
% end

