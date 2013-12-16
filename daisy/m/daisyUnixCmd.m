function [retStr] = daisyUnixCmd(cmd, altCmd);
%  
% retStr = daisyUnixCmd(cmd, altCmd)
% 
% Takes a unix command and returns the result as a string,
% in the variable retStr.
%
% If the result is faulty it returns an empty string or
% the alternative altCmd.
%   
% cmd    : command in text, such as e.g. 'echo $USER'
% altCmd : string result, for example 'johndoe'
%
% altCmd is an optional parameter
%
    
%
% User: jacobw
% Project name: daisy
% Project area: /site/edu/es/DAISY/daisy/
%
% Department of Electrical Engineering
% Linkoping University 
%
% Mon Nov 30 09:17:14 CET 2009
%
    
    [res, pat] = unix(cmd);
    
    if res==0
        retStr = pat(1:(end-1));        
    else % no result
        if (nargin == 2)
            retStr = altCmd;
        else
            retStr = '';        
        end;
    end;
