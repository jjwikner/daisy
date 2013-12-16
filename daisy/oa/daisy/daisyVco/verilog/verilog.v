
// Simulates the voltage controlled oscillator
// Control voltage is represented as a 64-bit input.
// Frequency limits is calculated corresponding to the value of D
// which determines the subrange.


`timescale 1ps/1ps
module daisyVco (foutp, foutn, D, vin, en);
   //Including enable (en)
   // setting output to Z if enable isn't high?
   
   input en;
   input [4:0] D;
   input vin;

   wire [63:0] vin;
   
   output foutp, foutn;
   
   real 	Fsub, Fmin, Fmax, Fmid, fcontrol, fout;
   integer 	vcoperiod;
   
   real 	scale_time;
   
   reg [4:0] 	sub_nr;
   reg  	foutp, foutn;
   
   parameter 	real Fmin0=3e9;
   parameter 	real Fmax0=3.8e9;
   parameter 	real Frange=3e8;
   
   parameter 	integer subs=32;


   always@(vin) 
     begin
	fcontrol = $bitstoreal(vin);
     end
 
   //Make frequency range dependent on 5-bit input 'D'

   //VCO-frequency will be proportional to 1/sqrt(C_total), so VCO-gain
   //tuning-range etc. will actually change with capacitance. We will
   //however approximate with equally (in frequency) large sub-ranges
   //with linear tuning. We divide overall tuning range evenly between
   //the subranges.

   //Subrange tuning-range is Fsub, subrange[i] is Fmin0+i*Fsub 
   //to Fmin0+(i+1)*Fsub starting at subrange[0].
   
   initial 
     begin	
	foutp = 1'b0;
	foutn = ~foutp;
	fcontrol = 0.5;	
	scale_time = 1e12; //time scale

	Fsub=(Fmax0-Fmin0)/subs; //calculation of subranges
        
	//Initial range calculation
	/* Since D=31 corresponds to subrange 0
	 *       D=30 corresponds to subrange 1
	 *        ...
	 *       D=1  corresponds to subrange 30
	 *       D=0  corresponds to subrange 31
	 * we should invert all bits of D before proceeding.
	 */
	sub_nr=$rtoi(~D); 	//Bitwise inversion of D here!!!
        if (sub_nr>(subs-1)) sub_nr=subs-1; //subs is maximum nof ranges
        Fmin=Fmin0+sub_nr*Fsub;
        Fmax=Fmin+Fsub;

	//adding half sub-range overlap
	Fmin=Fmin-Fsub/2;
	Fmax=Fmax+Fsub/2;
        Fmid=(Fmin+Fmax)/2;
	
	//calculate frequency output
	fout = Fmid + (fcontrol - 0.5)*Frange;
		//limit frequency
	if (fout < Fmin) fout = Fmin;
	if (fout > Fmax) fout = Fmax;
	vcoperiod=scale_time/fout;
     end


   //This block calculates minimum and maximum frequency
   always@(D or vin or en)   //triggered by change in HF-divider setting
       begin //HF_div ratios are 0=2,1=4,2=8,3=16,4=32,>4=32
	  /* Since D=31 corresponds to subrange 0
	   *       D=30 corresponds to subrange 1
	   *        ...
	   *       D=1  corresponds to subrange 30
	   *       D=0  corresponds to subrange 31
	   * we should invert all bits of D before proceeding.
	   */
	  //Bitwise inversion of D here!!!
	  sub_nr=$rtoi(~D);
          if (sub_nr>(subs-1)) sub_nr=subs-1; //subs is maximum nof ranges
          Fmin=Fmin0+sub_nr*Fsub;
          Fmax=Fmin+Fsub;
	  
	  //adding half sub-range overlap
	  Fmin=Fmin-Fsub/2;
	  Fmax=Fmax+Fsub/2;  //adding half sub-range overlap
          Fmid=(Fmin+Fmax)/2;

	  //calculate frequency output
	  fout = Fmid + (fcontrol - 0.5)*Frange;
	  
	  //limit frequency
	  if (fout < Fmin) fout = Fmin;
	  if (fout > Fmax) fout = Fmax;
	  vcoperiod=scale_time/fout;
       end
   
   //clock generator
   // generates a clock output based on the value of vcofreq
   always
     begin
	if(en)
	  begin
	     foutp = #(vcoperiod/2) 1'b0;
	     foutp = #(vcoperiod-vcoperiod/2) 1'b1;
	  end
	else
	  begin
	     foutp = #(vcoperiod/2) 1'b0;
	  end
     end

   always@(foutp) foutn <= ~foutp;

	     
   
endmodule 




