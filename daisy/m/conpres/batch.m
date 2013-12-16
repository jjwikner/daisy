K = AD_convert('sine',0.0004,1,4096,8,1,0.0004/128);
two = [1 2 4 8 16 32 64 -128];
A= K*turnaround(two)';
plot(A);
[dnl, p] = DNL(K,'sine',0.0004,-0.0004);
plot(p);                                
h = histogram(K, 1,1);
plot(h);
figure(1);
plot(p);
figure(2);
plot(h);
[dnl, p] = DNL(K,'sine',0.0003, -0.0003);
figure(1);                               
plot(p);                                 
[dnl, p] = DNL(K,'sine',0.0004,-0.0004); 
plot(p);                                
plot(dnl);
 
