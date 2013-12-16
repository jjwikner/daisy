function [output]=volterraout(input, kernels, sco, sci, order, skip, memory)
% [output]=volterraout(input, kernels, sco, sci, order, skip, memory)
%
% Calculates the output for a nonlinear system represented with its discrete
% Volterra series. 'input' is the input signal, 'kernels' contain the
% discrete Volterra kernels, 'sco' and 'sci' are scale factors for the output
% and the input respectively. The order of the nonlinearity is given in
% 'order'. 'skip' contains exponents to skip, e.g., if order=5 and the
% system is perfectly differential we expect no even order terms => 
% skip=[2 4]. 'memory' is the memory depth of the system, memory=1 is
% equivalent to the static case. The kernels and scale factors can be
% estimated using the function volterraest. The input should be given with a
% zero dc-level.
%
% (c) Ola Andersson, MERC, Ericsson Microelectronics AB, July 2000
%

% convert input signal to column vector

si=size(input);
if si(1) < si(2)
  input=input';
end

% scale input signal
input=input/sci;


% construct matrix of shifted input signals
a=[];
for i=0:memory-1
  a=[a input(memory-i:length(input)-i)];
end

% construct matrix of all exponents of the above
A=[];B=ones(length(input)+1-memory,1);b=[1];
for i=1:order
  sb=size(B);
  C=[];c=[];
  for j=1:sb(2)
    for k=b(j):memory
      C=[C B(:,j).*a(:,k)];
      c=[c k];
    end
  end
  use=find(skip==i);
  if(isempty(use))
    A=[A C];
  end
  B=C;b=c;
end

output=sco*A*kernels;
