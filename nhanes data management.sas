
*read the merged nhanes SAS data from library;
data one;
set lib.merge_c;
run;


data two; *n=18401;
set one;
keep SEQN RIAGENDR RIDAGEYR HID040 /* age gender and dental insurance */
	 OHX08ESL--OHX19ESO /* toothwear variables */ 
	 MCQ010; /* asthma variable */ 
run;


*we removed irrelevant and missing data on toothwear;
data three; *n=2661;
set two;
array NumVar OHX08ESL--OHX19ESO;
	do over NumVar;
	  if NumVar=8 then delete; /* 8=toothwear due to trauma and not wear */
	  if NumVar=9 then delete; /*9=could not access */
	  if NumVar=. then delete; /* .=missing */
   end;
run;

* create outcome variable by counting the number of surfaces with toothwear;
data four;
set three;
array NumVar OHX08ESL--OHX19ESO;
	 count=0;
	 do over NumVar;
	  if NumVar>0 then count=count+1;
	  end;
drop OHX08ESL--OHX19ESO;
run;


data five;
set four;
if MCQ010=9 then MCQ010=2; /*set "don't know" responses as "no"*/
if HID040=9 then HID040=2; /*set "don't know" response to dental insurance as "no"*/
if HID040=7 then HID040=2; /*set "refused" responses to dental insurance as "no"*/
run;

*A graph of the toothwear variable;
ODS LISTING STYLE = STATISTICAL;
ods graphics / reset imagename = "Figure" outputfmt = jpg width = 6in;
proc freq data=five;
table count /plots=freqplot(type=bar scale=percent);
label count='Count of surfaces with tooth wear';
run;
ods graphics off;
ods listing close;


proc export data=five 
outfile="nhanes.ex1.csv" dbms=csv replace;
run;
