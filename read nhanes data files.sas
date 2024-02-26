/*Read the data */

*Demographics file: age and gender;
libname xptfile xport "DEMO_C.XPT" access=readonly;                                               
proc copy inlib=xptfile outlib=lib; run;                                                                                                                                                                                                                            
proc sort data=lib.DEMO_C;                                                                                                                   
by seqn;                                                                                                                                
run;   

*Oral Health file: Toothwear;
libname xptfile xport "OHXADD_C.XPT" access=readonly;                                               
proc copy inlib=xptfile outlib=lib; run;                                                                                                                                                                                                                                  
proc sort data=lib.OHXADD_C;                                                                                                                   
by seqn;                                                                                                                                
run; 

* Asthma data file;
libname xptfile xport "MCQ_C.XPT" access=readonly;                                               
proc copy inlib=xptfile outlib=lib; run;                                                                                                                                                                                                                                  
proc sort data=lib.MCQ_C;                                                                                                                   
by seqn;                                                                                                                                
run;    

* Dental insurance;
libname xptfile xport "HIQ_C.XPT" access=readonly;                                               
proc copy inlib=xptfile outlib=lib;  run;                                                                                                                                                                                                                                    
proc sort data=lib.HIQ_C;                                                                                                                   
by seqn;                                                                                                                                
run; 


*merge all data files;
data lib.merge_C;                                                                                                                          
merge lib.DEMO_C lib.OHXADD_C lib.MCQ_C lib.HIQ_C;
by seqn; 
run;

