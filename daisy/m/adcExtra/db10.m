function dbp = db10(x)
% 
% dB10.m
%
% dbp = dB10(x)
% 
% Returns 10*log10(abs(x)).
% Removes small values and zeroes before log10 operation
%
% Revision history:
% 050511 - Created by Martin Anderson
%

dbp = -Inf*ones(size(x));
if isempty(x)
    return
end
nonzero = x~=0;
dbp(nonzero) = 10*log10(abs(x(nonzero)));