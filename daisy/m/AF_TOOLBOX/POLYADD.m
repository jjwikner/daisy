function Poly = POLYADD(P1, P2)
%
% function Poly = POLYADD(P1, P2)
%
% Adds two polynomials on the form An*s^n + An-1*s^n-1+ ... + a0
%
% Toolbox for ANALOG FILTERS USING MATLAB, Springer, 2009

%
% Author:   Lars Wanhammar 1983-03-15
% Modified by:  LW 2007-07-12
% Copyright:  Divison of Electronics Systems
%             Dept. Electrical Engineering, Linkšping University, Sweden
% Version:  1  
% Known bugs:  None
% Report bugs to: larsw@isy.liu.se

    n1 = length(P1);
    n2 = length(P2);
    Poly = zeros(1, max(n1, n2));
    if n1 >= n2
        Poly = P1;
        for j = n2:-1:1
            Poly(n1-n2+j) = Poly(n1-n2+j) + P2(j);
        end
    else
        Poly = P2;
        for j = n1:-1:1
            Poly(n2-n1+j) = Poly(n2-n1+j) + P1(j);
        end
    end
