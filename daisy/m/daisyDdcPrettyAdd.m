function daisyDdcPrettyAdd(currentModule, local)
global daisyUserArea_ daisyDaisyArea_ daisyProjArea_;
    
% The DDC path for currentModule is loaded if it is found 
% in the $PROJAREA. 
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
    retStr = daisyWorkArea_;
else
    retStr = daisyUnixCmd('echo $PROJAREA','none');
    retStr = daisyProjArea_;
end;

disp('============================================');
disp([' Loading (' currentModule ')']);
disp('============================================');

daisyDdcPath(currentModule);

