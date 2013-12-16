function dnls = dnlstat(values,nob,ampmax,ampmin, idnls,delay);

delta = (ampmax-ampmin)/(2^nob - 1);

delay = 1;
idnls = 0;

dnls = values(delay:N)-values(1:(N-delay))-delta;

dnls = (dnls-idnls)/delta;

