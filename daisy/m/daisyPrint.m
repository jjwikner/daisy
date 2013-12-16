function daisyPrint(fileName, fig2bePrinted, plotPath)

% Suggested to use it as daisyPrint(mfilename)    
%
% User: mreza
% Project name: stuck
% Project area: /proj/es/shekes/stuck
%
% Department of Electrical Engineering
% Linkoping University 
%
% Fri Oct 21 23:10:31 CEST 2011
%
% specify mfilename in your script for file name

    if nargin < 3
        [res, patu] = unix('echo $USERAREA');
        % user's area/doc as plotting path, if the path is not specified! 
        userarea = patu(1:(end-1));
        % checks if it doesn't exist, to create the default dir
        if exist([userarea '/doc/mfigs/'],'dir')
        else
            mkdir([userarea '/doc/mfigs/'])
        end
        plotPath = [userarea '/doc/mfigs/'];
    end

    if nargin < 2
        fig2bePrinted = gcf; % default figure
    end
    
    if nargin < 1
        disp('============================================');
        disp(' Error: Please specify output file name ');
        disp('============================================'); 
    end
    fh = fig2bePrinted;

    % prompt for overwriting
    if exist([plotPath strcat(fileName, '.eps')])
        reply = input(' File already exist. Do you want to overwrite it? Y/N [Y]: ', 's');
        if isempty(reply)
            reply = 'Y';
        end
        
        if reply ==  'Y'
            print( fh, '-depsc2','-r300', '-tiff', [plotPath strcat(fileName, '.eps')]);
            disp('============================================');
            disp([' Plotted to file ' plotPath strcat(fileName, '.eps')]);
            disp('============================================'); 
        else
            disp('============================================');
            disp(' No figure plotted! ');
            disp('============================================'); 
        end
        
    else
        print( fh, '-depsc2','-r300', '-tiff', [plotPath strcat(fileName, '.eps')]);
        disp('============================================');
        disp([' Plotted to file ' plotPath strcat(fileName, '.eps')]);
        disp('============================================'); 
    end
