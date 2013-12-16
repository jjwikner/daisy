function N = over(n,k)

%         n!       /n\
% N = ---------- = | |
%      k!(n-k)!    \k/

N = gamma(n+1)./(gamma(k+1).*gamma(n-k+1));
