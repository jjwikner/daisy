function[H] = pz2frs(Z,P,K,W)% Per Lowenborg%% Department of Electrical Engineering% Linkoping University %% Mon Sep 20 17:08:21 CEST 2010%L=length(W);H=K*((1:L)-(0:L-1));zl=length(Z);pl=length(P);W=j*W;for a=1:zl  H=H.*(W-Z(a));    endfor b=1:pl  H=H./(W-P(b));    end