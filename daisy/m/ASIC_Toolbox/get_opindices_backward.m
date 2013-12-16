function[op_indices,inp_index]=get_opindices_backward(sfg,nodenummer);
% gets the operation nummer and input index number of
% instrs whose input is the nodenummer. Performs delay tunnelling.

%nodenummer = 9;
%%sfg=sfg_recur;

%Code logic is divided into two parts
%In the first part nodenummer is searched for in the outnode list
%And the corresponding instr nummer is returned.
% Second part the  parentnodes are sought and corresponding
% instr nummer are sought. This involves delay tunneling.

%sort the SFG and get the inout nodes list
%[sfg,pr]=sortsfg(sfg);
[inv,outv]=get_inoutnodelist(sfg);

%Initialize the output vecs
op_indices=[];
inp_index = [];

ki=find(sfg(:,1) == 6);

if ~isempty(ki)
for ik = 1:length(ki)
    [inn_d(ik),outn_d(ik)]=getinoutnodes_op(sfg(ki(ik),:));
    [dc,pn(ik)]=get_delaycount(sfg,outn_d(ik));
end
end


% find the node as a output of an op - delay,arith or inp
ka = find(nodenummer == outv);
% check whether it exists in SFG
if ~isempty(ka)

    ka = mod(ka,length(sfg(:,1)));
    % if ka = lenght or multiples of length then the mod will be zero
    % thus correct the value if necessary
    if ka == 0
        ka = length(sfg(:,1));
    end
    % is it an inp or arith op ?
    if sfg(ka,1) ~= 6
        % The input position can be 1 or 2 only !!
    kb = find(nodenummer == outv(ka,:));
    op_indices = [op_indices ka];
    inp_index = [inp_index kb];
    else
        op_i=0;
        kj = find(nodenummer == outn_d);
        op_i=find(pn(kj) == outv);
        op_i=mod(op_i,length(sfg(:,1)));
            if op_i == 0 
            op_i = length(sfg(:,1));
            end
        op_indices = [op_indices op_i];
        kc = (find(pn(kj) == outv(op_i,:)));
        inp_index = [inp_index kc];
    end


else
    disp ('Node not found in SFG');
end



