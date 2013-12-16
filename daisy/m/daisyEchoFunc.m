function retStr = daisyEchoFunc(stringData)


% User: jacobw
% Project name: bodyLink
% Project area: /site/edu/es/DAISY/daisy
%
% Department of Electrical Engineering
% Linkoping University 
%
% Mon Jan 14 15:13:21 CET 2013
%
    retStr = 'daisy';
    selfStruct_ = dbstack();
    for mm = 2:length(selfStruct_);
        retStr = [retStr '::' selfStruct_(mm).name];
    end;

end

