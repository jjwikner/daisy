function inls = inlstat(dnlv);

inls(1) = dnlv(1);

for i = 2:max(size(dnlv))
  inls(i) = inls(i-1)+dnlv(i);
end;

