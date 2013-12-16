function [delay_count,pn]=get_delaycount(sfg,nodenummer);

delay_count = 0;
[pn,df]= get_nodeorigin(sfg,nodenummer);
while pn ~= nodenummer
    nodenummer = pn;
    delay_count=delay_count +1;
    [pn,ds]= get_nodeorigin(sfg,nodenummer);
end
