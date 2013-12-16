function [readData, waveNames, waveUnits] = ...
        daisyReadOcnPrint(fileName, numberOfSignals);
%
% function [readData, waveNames, waveUnits] =  ...
%         daisyReadOcnPrint(filename, numberOfSignals);
% 
% Function that reads the specialized format from 
% Cadence ocnPrint dump.
%
% numberOfSignals : Expected number of signals in the 
%                   file. (this can be calculated 
%                   automatically, but left for future 
%                   improvement). Be precise though!
% fileName        : Name of the file from the ocnPrint 
%                   command.  
% 
% This is a function used for the daisy design flow.
%
    
% Created by J Jacob Wikner, Linköping University. 
% 2009-10-07
%
    
    fileId = fopen(fileName);
    
    if (fileId == -1)
        % File was not found
        warning(['Filename ', fileName, ' was not found!']);
        help readOcnPrint;
    else
        % First we tap of the headers, notice the +1, since the "time"
        % column will be there too.
        signalNamesAndUnits = textscan(fileId,'%s%s',numberOfSignals+1);
        waveNames = signalNamesAndUnits{1};
        waveUnits = signalNamesAndUnits{2};
        % Create a string-read pattern for all the waveforms 
        % (number of %s = number of columns in the file)        
        pattern = '%s';
        for m = 1:numberOfSignals
            pattern = [pattern '%s'];
        end;
        
        % Now read the rest of the enchilada
        
        dataValues = textscan(fileId, pattern);        
        readData = zeros(length(dataValues{1}), 1+numberOfSignals);
        
        % "Unfortunately" we now have to run through the data vectors and
        % replace the "n", "u", "m", "M", "p", "f", "a", etc., with e-9,
        % e-6, and so on.
        
        unitVector = ['a' 'p' 'n' 'u' 'm' 'N' 'k' 'M' 'G' 'T' 'P'];
        for m = 1:(1+numberOfSignals)
            dataCell = dataValues{m};
            for l = 1:length(dataCell)
                dataString = char(dataCell(l));
                k = find(dataString(end) == unitVector);                         
                if k 
                    dataString = [dataString(1:(end-1)) 'e' num2str(3*(k-6))]
                end;
                readData(l,m) = str2num(dataString);                   
            end;
        end;
        fclose(fileId);
    end;
   