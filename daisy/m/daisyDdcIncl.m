function daisyDdcIncl(ddcName, local)
    
% The DDC ddcName is loaded if it is found in the $PROJAREA.
% for local set to nil (default) or in the $WORKAREA if is 
% set to true.
      
%
% User: jacobw
% Project name: marble
% Project area: /proj/es/marble
%
% Department of Electrical Engineering
% Linkoping University 
%
% Tue Apr 20 08:48:20 CEST 2010
%
if nargin == 1
    local = 0;
end;

% There should be an option to read from local domain only.
if local
    retStr = daisyUnixCmd('echo $WORKAREA','none');
else
    retStr = daisyUnixCmd('echo $PROJAREA','none');
end;

% First check if the m/daisy for the ddc exists

if exist([retStr '/' ddcName '/m/daisy.m'])
    run([retStr '/' ddcName '/m/daisy.m']);
end;
