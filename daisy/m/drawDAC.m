function drawDAC(x,y,size);



xnew=[x+size x-size x-size x+size x+size];
ynew=[y+size y+size y-size y-size y+size];

color = ['y','m','c','r','g','b','w','k'];

%farg = color(floor(rand*7.9)+1);
farg = 'w';

fill(xnew, ynew, farg);



