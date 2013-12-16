function y=dbm(x)
% dbm(x) = 10*log10(x*1000)  The equivalent in dBm of a power x in W
y = -Inf*ones(size(x));
if isempty(x)
    return
end
nonzero = x~=0;
y(nonzero) = 10*log10(abs(x(nonzero)))+30;
