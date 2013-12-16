function [fieldpositions,fieldBgcolor,fieldTxtcolor] = cFielddefinitions(left,top,btnWid, btnHt,BCOLOR, Numberoffields);

fieldpositions(Numberoffields,1:4) = [0 0 0 0];
fieldBgcolor(1:Numberoffields,1:3) = ones(Numberoffields, 1)*BCOLOR;
fieldTxtcolor(1:Numberoffields,1:3) = ones(Numberoffields, 3);

for i = 1:2:Numberoffields;
  fieldpositions(i, 1:4)   = [left top-(i+1)*0.6*btnHt btnWid btnHt];
  fieldpositions(i+1, 1:4) = [left+1.05*btnWid top-(i+1)*0.6*btnHt btnWid btnHt];
end;

for i = 5:4:Numberoffields;
  fieldBgcolor(i,:) = [1 1 1];
  fieldBgcolor(i+1,:) = [1 1 1];
  fieldTxtcolor(i,:) = [0 0 0];
  fieldTxtcolor(i+1,:) = [0 0 0];
end
