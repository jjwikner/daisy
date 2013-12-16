function coeff = getLWDFcoefficients(P);
% This function computes the adaptor coefficients for a LWDF given the poles.

N = length(P);
coeff = zeros(1, N);

% Find the phase angles
arg = abs(angle(P));

% Start with the real pole
id = find(~arg);
if length(id) ~= 1
    error('There should be ONE real pole');
end
coeff(1) = P(id);

% Loop over the remaining complex conjugated pole pairs
for i = 2:2:N - 1
    P(id) = []; arg(id) = [];
    id = find(arg == min(arg));
    if length(id) ~= 2
        error('The remaining poles should be complex conjugated pole pairs');
    end
    pole = P(id(1));
    coeff(i) = -abs(pole)^2;
    coeff(i + 1) = 2*real(pole)/(1 - coeff(i));
end
