function[op_indices,inp_index]=get_opindices_forward(sfg,nodenummer);
% gets the operation nummer and input index number of
% instrs whose input is the nodenummer. Performs delay tunnelling.

%nodenummer = 9;
%%sfg=sfg_recur;

%Code logic is divided into two parts
%In the first part nodenummer is searched for in the outnode list
%And the corresponding instr nummer is returned.
% Second part the nodes whose parentnode is nodenummer and corresponding
% instr nummer are sought. This involves delay tunneling.

%sort the SFG and get the inout nodes list
%[sfg,pr]=sortsfg(sfg);
[inv,outv]=get_inoutnodelist(sfg);

%Initialize the output vecs
op_indices=[];
inp_index = [];

% First part - search in present schedule
% find the instr nummer whose inp = nodenummer
ka = find(nodenummer == inv);
if ~isempty(ka)
for ak = 1:length(ka)
    ka(ak) = mod(ka(ak),length(sfg(:,1)));
    % if ka(ak) = lenght or multiples of length then the mod will be zero
    % thus correct the value if necessary
    if ka(ak) == 0
        ka(ak) = length(sfg(:,1));
    end
    % We aren't bothered about delay instrs
    if sfg(ka(ak),1) ~= 6
        % The input position can be 1 or 2 only !!
    kb = find(nodenummer == inv(ka(ak),:));
    
    op_indices = [op_indices ka(ak)];
    inp_index = [inp_index kb];
    end
end


% Second Part
% Find all the delay instr in SFG
ki=find(sfg(:,1) == 6);

if ~isempty(ki)
for ik = 1:length(ki)
    [inn_d(ik),outn_d(ik)]=getinoutnodes_op(sfg(ki(ik),:));
    [dc,pn(ik)]=get_delaycount(sfg,outn_d(ik));
end
% Pick only those delay instr whose pnode = nodenummer
kj = find(nodenummer == pn);
    
if ~isempty(kj)
for ij = 1:length(kj)
    op_i=0; % intiliase the loop variables
    op_i=find(outn_d(kj(ij)) == inv); % Search all instr whose inp = out of delay 
    % get the instr nummer and correct if necessary
    for ik = 1:length(op_i)
    op_i(ik)=mod(op_i(ik),length(sfg(:,1)));
    if op_i(ik) == 0 
        op_i(ik) = length(sfg(:,1));
    end
    % Dont bother about delay instrs
    if sfg(op_i(ik),1) ~= 6
    op_indices = [op_indices op_i(ik)];
    % find the index of delay out in the input vec for matching instrs
    kc = find(outn_d(kj(ij)) == inv(op_i(ik),:));
    % can be either 1 or 2 only !
    inp_index = [inp_index kc];
    end
    end
    
end
end
end
else
    disp('Node not found in the SFG');
end


