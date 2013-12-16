function[starttime,lat_vec,exe_vec,endtime,criticalpath,numberoflaps]=get_timinginfo(sfg,timinginfo);
% Computes the starttime,endtime and critical path given
% sfg = sfga;
% latency = 2;
% executiontime=2;
[sfg,pr]=sortsfg(sfg);
[inv,outv]=get_inoutnodelist(sfg);
starttime=zeros(1,length(sfg(:,1)));
time_array = 0;

latency=timinginfo(1);
executiontime=timinginfo(2);

for ix = 1:length(sfg(:,1))
    [inn,outn]=getinoutnodes_op(sfg(ix,:));
    
    if ~(isempty(inn))
        l=size(inn);
        start_variable = 0;
        
        for iy = 1 : l(2)
        op_i=(find(inn(iy) == outv));
        op_i=mod(op_i,length(sfg));
                if op_i == 0
                op_i =length(sfg);
                end
                if (sfg(op_i,1) == 1 | sfg(op_i,1) == 2 | sfg(op_i,1) == 6)
                    runtime = 0;
                else
                    runtime = latency;
                end
                start_variable(iy)=starttime(op_i)+runtime;
        end
        starttime(ix)=max(start_variable);
        
    end
end

%write code to put start time = NaN and latency = 0 for delay

for i = 1 : length(sfg(:,1))
    if (sfg(i,1) == 1 | sfg(i,1) == 2 | sfg(i,1) == 6)
        lat_vec(i) = 0;
        exe_vec(i) = 0;
    else
        lat_vec(i) = latency;
        exe_vec(i) = executiontime;
    end
  %  sprintf('Operation : %d latency : %d  Start time : %d',i,time_array(i),starttime(i))
end

endtime = starttime + lat_vec ;
% for im = length(endtime)
%     if(endtime(im) > scheduletime)
%         endtime(im) = mod(endtime(im),scheduletime);
%     end
% end

%------------------------------------------------------------------------
%Critical path Computation
% Remove the endtimes of all input,output and delay instr
% CP = maxendtime 

delay_ip_out_opindices = find(sfg(:,1) == 6 |sfg(:,1) == 1 |sfg(:,1) == 2 );

for  ij = 1:length(sfg(:,1))
    et_notarithflag = (find(ij == delay_ip_out_opindices));
    if ~(isempty(et_notarithflag))
        et_arith(ij) = 0;
    else
        et_arith(ij)=endtime(ij);
    end
end

criticalpath=max(et_arith);
%--------------------------------------------------------------------------
%----Number of laps

number_laps=zeros(length(sfg(:,1)),2);

for ix = 1:length(sfg(:,1))
     
    if (sfg(ix,1) == 1 | sfg(ix,1) == 2 |sfg(ix,1) == 6) 
    number_laps(ix,1) = 0;number_laps(ix,2) = NaN;
        
    else
        [inn,outn]=getinoutnodes_op(sfg(ix,:));
        l=size(inn);
        for iy = 1 : l(2)
        nn=inn(iy);
        [dc,pn]=get_delaycount(sfg,nn);
        if dc ~= 0
           op_i=(find(pn == outv));
           op_i=mod(op_i,length(sfg));
                if op_i == 0
                op_i =length(sfg);
                end
                if starttime(ix) < endtime(op_i)
%                     %special case 
%                     if (starttime(ix) == 0) & (endtime(op_i) == criticalpath)
%                         number_laps(ix,iy) = dc;
%                     else
                    number_laps(ix,iy) = dc - 1;
%                     end
                else
                    number_laps(ix,iy) = dc ;
                end
        else
            number_laps(ix,iy) = 0;
            
        end
        end
    
     end
   
end

numberoflaps = number_laps;
for ik = 1:length(numberoflaps(:,1))
    if (sfg(ik,1) == 5)
        numberoflaps(ik,2)=NaN;
    end
end

        

 %--------------------------------------------------------------------

                


   






  
  