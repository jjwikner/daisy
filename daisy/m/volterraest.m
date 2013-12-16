function [kernels, sco, sci]=volterraest(input, output, order, skip, memory)
% [kernels, sco, sci]=volterraest(input, output, order, skip, memory)
%
% Estimates the discrete Volterra kernels of a nonlinear system with
% inputsignal 'input' and outputsignal 'output'. The order of the
% nonlinearity is given by 'order', and 'skip' contains any exponents to
% skip, e.g, if order=5 and we expect no even order terms skip=[2 4].
% The memory depth is given by 'memory', memory=1 is equivalent to the
% static case. The outputs sco and sci are scalefactors for the output and
% input signal respectively.
%
% (c) Ola Andersson, MERC, Ericsson Microelectronics AB, July 2000
%

% convert all signals to column vectors
si=size(input);
if si(1) < si(2)
  input=input';
end
so=size(output);
if so(1) < so(2)
  output=output';
end

% scale signals
input=input-mean(input);
sci=max(abs(input));
input=input/sci;

output=output-mean(output);
sco=max(abs(output));
output=output/sco;

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

kernels=A\output(memory:length(output));
