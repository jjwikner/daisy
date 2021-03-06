//Simulates integration of charge on a capacitor


// Q=i*t
// v=Q/C
// i is negative or positive depending on up or dwn

`timescale 1ns/1ps
module daisyChargePumpInt(out, up, dwn);
   output out;
   input  up,dwn;
   real   vacc, vp, dq, v, i, c, kp, tup, tdwn, tup_high, tdwn_high, scale_time;
   reg 	  out;
   integer outperiod;
   
   initial
     begin
	out = 1'b0;

	v = 0;
	vacc = 0;
	vp = 0;
	dq = 0;
	
	i = 5e-6;
	c = 1e-9;
	kp = 1e11;
	
	scale_time = 1e9;
	tup = 0;
	tdwn = 0;
	tup_high = 0;
	tdwn_high =0;

	outperiod = 20; //50MHz
     end

   always@(posedge up)
   begin
      tup = $realtime;
   end

   always@(posedge dwn)
   begin
      tdwn = $realtime;
   end

   always@(negedge up)
   begin
      tup_high = $realtime - tup;
   end

   always@(negedge dwn)
   begin
      tdwn_high = $realtime - tdwn;
   end

   always@(posedge up)
     begin
	dq = i*(tup_high-tdwn_high)/scale_time;
	vp = dq*kp;		// proportional constant [Ohm/s]
	
	vacc = vacc + dq/c;
	//limit vacc
	if (vacc < 0.01) v = 0.01;
	if (vacc > 0.99) v = 0.99;
	
	v = vacc + vp;
	//limit v
	if (v < 0.01) v = 0.01;
	if (v > 0.99) v = 0.99;
	
     end

   always
     begin
	out = #(v*outperiod) 1'b0;
	out = #(outperiod-v*outperiod) 1'b1;
     end

endmodule // daisyChargePumpIntegrator
