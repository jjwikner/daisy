function [newschedule]=updatetimescale(schedule,factor);
% Multiplies the time-scales in schedule by factor.

if factor <= 0
    error('Factor must be positive');
end

%get the timing info

size_schedule=size(schedule);
st = zeros(size_schedule(1)-1,1);
et = zeros(size_schedule(1)-1,1);
cfg=zeros(size_schedule(1)-1,size_schedule(2));

%time info computation
cfg=schedule(2:size_schedule(1),1:size_schedule(2));
time_columns_index=size_schedule(2)-4;
timecol = [time_columns_index:1:size_schedule(2)];
scheduletime=schedule(1,1);
st = cfg(:,timecol(3));
latency =cfg(:,timecol(4));
exectime=cfg(:,timecol(5));

% Multiply the time-scale by factor
new_scheduletime = floor(scheduletime*factor);
new_starttime=floor(st*factor);
new_latency=floor(latency*factor);
new_exectime=floor(exectime*factor);
cfg(:,timecol(3))=new_starttime;
cfg(:,timecol(4))=new_latency;
cfg(:,timecol(5))=new_exectime;

%Form the new schedule
newschedule=zeros(size_schedule(1),size_schedule(2));
newschedule(1,1)=new_scheduletime;
newschedule(2:size_schedule(1),1:size_schedule(2))=cfg;
