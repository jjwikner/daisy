function dnls = dnldyn(values,nob,ampmax,ampmin, idnls,delay);

delta = (ampmax-ampmin)/(2^nob - 1);

delay = 1;
idnls = 0;

dnls = values((delay+1):max(size(values)))-values(1:(max(size(values))-delay))-delta;

dnls = (dnls-idnls)/delta;

