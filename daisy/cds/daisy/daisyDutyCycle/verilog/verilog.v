
module daisyDutyCycle (out64, clk );
   input clk;
   output out64;

   wire [63:0] out64 = $realtobits(dutycycle);
   real       dutycycle, pos_time, period_time, last_time, up_time;
   
   
   initial
     begin
	pos_time=0;
	period_time=0;
	dutycycle=0.5;
	last_time=0;
	up_time=0;
     end
   
   always@(posedge clk)
     begin
	pos_time=$realtime;
	period_time=pos_time-last_time;
	dutycycle=up_time/period_time;
	last_time=pos_time;
     end

   always@(negedge clk)
     begin
	up_time=$realtime-pos_time;
     end
endmodule
