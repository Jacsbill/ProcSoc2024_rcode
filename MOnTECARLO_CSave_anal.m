T=readtable('C:\levEEGanalysis\main\paper_results_code\DRAFTS\finalR_07072022\MOnTECARLO_CSave1.csv');
T2=readtable('C:\levEEGanalysis\main\paper_results_code\DRAFTS\finalR_07072022\MOnTECARLO_CSave1a.csv')

% Load the two outputs.both contain data with randomly samples CPP
% timelines. Two data sets add up to 1000 iterations. run separetly to
% prevent R crashing. 


T=vertcat(T,T2);
head(T)
size(T)


 %certL_p  73 4 0  certQ_p 99 4 0     corr_p   92 8 0 2   Cl_C_p   82 2
 %0    cL_C_p  82 5 0
 
 
T.cert_p_bin=T.cL_C_p ;
T.cert_p_bin(T.cert_p_bin>0.05)=10;
T.cert_p_bin(T.cert_p_bin<=0.05)=0;
T.cert_p_bin(T.cert_p_bin==0)=5;
T.cert_p_bin(T.cert_p_bin==10)=0;

 count=0;
 savec=[];
  count2=0;
 savec2=[];
  count3=0;
 savec3=[];
for i=1:length(T.cert_p_bin)-3;
    if (T.cert_p_bin(i)+ T.cert_p_bin(i+1))==10;
        count=count+1;
        savec=vertcat(savec,i);
    end
     if  (T.cert_p_bin(i)+ T.cert_p_bin(i+1)+T.cert_p_bin(i+2))==15;
        count2=count2+1;
        savec2=vertcat(savec2,i);
     end
     
     if  (T.cert_p_bin(i)+ T.cert_p_bin(i+1)+T.cert_p_bin(i+2)+T.cert_p_bin(i+3))==20;
        count3=count3+1;
        savec3=vertcat(savec3,i);
     end
end

[count count2 count3]


% for i=1:length(savec);
%     T.cert_p_bin(savec(i)-5:savec(i)+5)
%     i 
%     pause
%      
% end