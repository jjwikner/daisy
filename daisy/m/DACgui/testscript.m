M = figure(1);
clf;

s = putFrm([0.5 0.09 0.4 0.75],'Frame PELLEpitt',M);
o = putTxt([0.6 0.1],'Textruta', M);
p = putBtn([0.6 0.2],'Skriv något', 'disp(''hej'')', M);
k = putFld([0.6 0.3],'ENOB','get(k,''String'')', M);
m = putMnu([0.6 0.4],'Test|Om|Det|Fungerar','get(m,''Value'')', M);
l = putChk([0.6 0.5],'Checking?','get(l,''Value'')', M);
n = putRdo([0.6 0.6],'Radiokillen', ...
    'if (get(m,''Value'')), set(m,''Value'',0); end', M);
r = putRdo([0.6 0.7],'Radiomannen', ...
    'if (get(n,''Value'')), set(n,''Value'',0); end', M);

t = putAxs([.1 .6 .3 .3], M);
u = plot([1:10]);

zoom on;
