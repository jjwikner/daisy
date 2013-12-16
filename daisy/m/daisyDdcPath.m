function daisyDdcPath(currentModule, sub, local)

global daisyUserArea_ daisyDaisyArea_ daisyProjArea_;

% The DDC path for currentModule is loaded if it is found 
% in the $PROJAREA. 
% for local set to nil (default) or in the $WORKAREA if is 
% set to true.
% sub is a subdirectory

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

if nargin < 3
    local = 0;
end;

if nargin < 2
    sub = '';
end;

% There should be an option to read from local domain only.
if local
    retStr = daisyUnixCmd('echo $WORKAREA','none');
    retStr = daisyWorkArea_;
else
    retStr = daisyUnixCmd('echo $PROJAREA','none');
    retStr = daisyProjArea_;
end;
% [retStr '/' currentModule '/m/' sub]
% First check if the m/daisy for the ddc exists
addpath([retStr '/' currentModule '/m/' sub]);

disp('============================================');
disp([' Adding ' retStr '/' currentModule '/m/' sub ' to the path']);
disp('============================================');

