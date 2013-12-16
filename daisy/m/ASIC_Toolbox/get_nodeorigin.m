function [pnode,delay_flag]=get_nodeorigin(sfg,nodenummer);
% Function returns the input pnode of a delay element
% whose output is nodenummer. If nodenummer is a
% input node or output of a arithmetic op then 
% pnode = nodenummer.

% first sort the SFG 
%[sfg,pr]=sortsfg(sfg);

%In the sorted SFG, the delay elements are grouped
%together and are placed at the bottom. So get the indices
% of instrs which are delay ops (op nummer = 6).

k = find(sfg(:,1) == 6) ;

% may be empty or a vec

if isempty(k) 
    disp('This SFG has no delay elements');
    pnode = nodenummer;
    delay_flag = 0 ;
else
    % get inout nodes of all delay elements in SFG
    for ix = 1:length(k)
        [inn(ix),outn(ix)]=getinoutnodes_op(sfg(k(ix),:));
    end
    % Check whether the nodenummer is a outnode of a delay element
    % if not then it must be either a ipnode or arithoutnode
    l = find(nodenummer == outn);
    if isempty(l)
        delay_flag = 0;
        pnode = nodenummer;
    else
        %if not then return pnode = inputnode of the delay element
        delay_flag = 1;
        pnode = inn(l);
    end
end

        
   
