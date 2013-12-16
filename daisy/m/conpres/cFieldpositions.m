function fieldpositions = cFieldpositions(left, top, btnWid, btnHt, Numberoffields);

fieldpositions(Numberoffields,1:4) = [0 0 0 0];

for i = 1:2:Numberoffields;
  fieldpositions(i, 1:4)   = [left top-(i+1)*0.6*btnHt btnWid btnHt];
  fieldpositions(i+1, 1:4) = [left+1.05*btnWid top-(i+1)*0.6*btnHt btnWid btnHt];
end;

