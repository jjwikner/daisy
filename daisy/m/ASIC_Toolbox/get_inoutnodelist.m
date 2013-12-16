function[invec,outvec]=get_inoutnodelist(sfg);


invec=-1*ones(length(sfg(:,1)),4);
outvec=-1*ones(length(sfg(:,1)),4);
for ix = 1:length(sfg(:,1))
    [inn,outn]=getinoutnodes_op(sfg(ix,:));
    
    if isempty(inn)
        invec(ix,1:4) = -1;
    else
        l=size(inn);
        invec(ix,1:l(2))=inn(1:l(2));
    end
    if isempty(outn)
        outvec(ix,1:4) = -1;
    else
        l=size(outn);
        outvec(ix,1:l(2))=outn(1:l(2));
    end
end


        
        