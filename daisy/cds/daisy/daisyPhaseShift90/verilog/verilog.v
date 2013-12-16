
module daisyPhaseShift90 (outI,outQ, in );
   output outI, outQ;
   reg 	  outI, outQ;
   input  in;

   integer period, tpos;

   initial
     begin
	outI = 1'b0;
	outQ = 1'b0;
	period = 10;
     end
   
   always@(posedge in)
     begin
	period <= $realtime - tpos;
	tpos <= $realtime;
     end
       
   always@(in)
     begin
	outI <= in;
	outQ <= #(period/4) in;
     end
   
endmodule
