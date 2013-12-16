function dbv = db20(x)

% 
% dB20.m
%
% dbv = dB20(x)
% 
% Returns 20*log10(abs(x)).
% Compensates small values and zeros.
%
% Revision history:
% 050511 - Created by Martin Anderson
%

dbv = -Inf*ones(size(x));
if isempty(x)
    return
end
nonzero = x~=0;
dbv(nonzero) = 20*log10(abs(x(nonzero)));


