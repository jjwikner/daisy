disp('====================================');
disp(' Loading daisy flow for Matlab');
disp('====================================');

global daisyUserArea_ daisyDaisyArea_ daisyProjArea_;

if findstr('WIN',computer)

    % Use variables in windows instead...
    %daisyUserArea_  = 'K:\home\jacobw\bodyLink\work_jacobw';
    %daisyDaisyArea_ = 'K:\site\edu\es\DAISY\daisy';
    %daisyProjArea_  = 'K:\site\edu\es\EXJOBB\bodyLink';
    
    % Use variables in windows instead...    
    % An if-statement could be placed here and check if variables are
    % defined or not.
    daisyUserArea_  = getenv('USERAREA');
    daisyDaisyArea_ = getenv('DAISYAREA');
    daisyProjArea_  = getenv('PROJAREA');
    
    pathSep   = '\';

else
    
    daisyUserArea_ = getenv('USERAREA');
    daisyDaisyArea_ = getenv('DAISYAREA');
    daisyProjArea_ = getenv('PROJAREA');
   
    pathSep = '/';

end;

% Adding the user's matlab area
addpath([daisyUserArea_ '/m/']);

% Adding the daisy MATLAB area
addpath([daisyDaisyArea_ '/m/']);

% Adding the analog filter toolbox
display('Adding Lars Wanhammars filter toolbox');
addpath([daisyDaisyArea_ '/m/AF_TOOLBOX/']);

display('Adding matlab2tikz');
addpath([daisyDaisyArea_ '/m/matlab2tikz/matlab2tikz-0.0.6/']);

display('Adding delsig');
addpath([daisyDaisyArea_ '/m/delsig/']);

display('Adding adcExtra');
addpath([daisyDaisyArea_ '/m/adcExtra/']);

display('Adding ASIC_Toolbox');
addpath([daisyDaisyArea_ '/m/ASIC_Toolbox/']);

display('Adding esplot');
addpath([daisyDaisyArea_ '/m/esplot/latest/']);

% ==================================================
% Default settings (that can be overridden below...)
% ==================================================

% Figure settings
fontSize = 14;
plotPath = [daisyUserArea_ '/doc/'];
set(0,'defaultlinelinewidth',3)
set(0,'defaultlineMarkersize',5)
set(0,'defaultaxesfontsize',fontSize);
set(0,'DefaultFigureColor','white')

columnWidth = 16.8; % 17 cm for DIN-A4
set(0, 'defaultfigurePaperUnits', 'centimeters');
set(0, 'defaultfigureUnits', 'centimeters');
set(0, 'defaultfigurePaperType','A4');
set(0, 'defaultfigurePaperSize', ...
       [columnWidth 0.1*round(10*columnWidth/sqrt(2))]);
set(0, 'defaultfigurePaperOrientation', 'portrait');
set(0, 'defaultfigurePaperPositionMode','manual')
set(0, 'defaultfigurePaperPosition', ...
       [0 0 columnWidth 0.1*round(10*columnWidth/sqrt(2))]);
set(0, 'defaultfigureFilename',[plotPath '/defaultplot.fig']);

% warning off;

% Include the sub ddcs for a given daisy project

ddcList = {};

if exist([daisyProjArea_ '/daisyProjSetup/info/daisyDdcs.txt'])
    fid = fopen([daisyProjArea_ '/daisyProjSetup/info/daisyDdcs.txt'], 'r');
    ddcList =  textscan(fid, '%s');
    fclose(fid);
    
    for m = 1:length(ddcList{1})
        ddcName = ddcList{1}{m};
        % Add the new ddc to the path
        daisyDdcPrettyAdd(ddcName);
        % Load the local daisy folder
        daisyDdcIncl(ddcName);
    end;
end;

% Include project specific ddc and run the project specific file
% ==============================================================

daisyDdcIncl('daisyProjSetup');

% Change into work path
% ==============================================================

cd([daisyUserArea_ '/m/']);





