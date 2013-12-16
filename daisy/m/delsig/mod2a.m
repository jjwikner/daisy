% A script for setting up the matrices for the second-order
% modulator that is often used in SC circuits
A=[1 0; 0.5 1];
B=[0.5 -0.5; 0 -0.5];
C=[0 1];
D=[0 0];
ABCD=[A B; C D];
