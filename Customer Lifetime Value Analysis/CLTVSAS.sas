libname  datapath "C:/Users/babycorn/Documents/Edupristine/CLTV/SM/" ; 
PROC IMPORT OUT= WORK.brijesh 
            DATAFILE= "C:\Users\babycorn\Documents\Edupristine\CLTV\SM\data.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data datapath.test_data1;
set work.brijesh;
run;

/*Run phreg proc with stepwise selection to find the step where we get minimum AIC*/
ods output ModelBuildingSummary=datapath.Summary;
ods output FitStatistics=datapath.Fit;

Proc Phreg data=datapath.test_data1;
Model Duration*Status(0)= 
gender	
age	
marital_status	
emp_status
Acquisition_cost	
Total_tran_6m	
Total_tran_12m	
Total_tran_18m	
Avg_Tran_6m	
Avg_Tran_12m	
Avg_Tran_18m		
no_of_dep	
FS_code	
no_complaints	
only_promo_buyer_flg	
value_cust_lvl	
high_freq_cust_flg
/ selection=stepwise slentry=0.99 slstay=0.9;
run;
/*Select all AIC in a table*/
data AIC;
set FIT;
if CRITERION = 'AIC ';
run; 

/*Find the best combination of 4-6(based on the min AIC at step 5 in previous run)variables that gives min AIC*/
ods output BestSubsets=datapath.Best_Subset;
proc phreg data=datapath.test_data1;
model Duration*Status(0)= 
gender	
age	
marital_status	
emp_status
Acquisition_cost	
Total_tran_6m	
Total_tran_12m	
Total_tran_18m	
Avg_Tran_6m	
Avg_Tran_12m	
Avg_Tran_18m		
no_of_dep	
FS_code	
no_complaints	
only_promo_buyer_flg	
value_cust_lvl	
high_freq_cust_flg/selection=score START= 4 STOP= 6 ; run;
/*Set the count _N_ to the number of obsrvations with all the combinations*/
data datapath.varlist ;
	set Best_Subset;
	if _N_ = 49;
	call symputx('list',variablesInModel);
run;

proc sort data=datapath.test_data1;
by status;
run;
/*training data*/
proc surveyselect data = datapath.test_data1
out = datapath.cust_training 
seed = 7 
method = srs n =(2500 2500); 
strata status ; 
run;
/*Validation sample*/
proc sql;
create table datapath.cust_validation as
select * from datapath.test_data1 where Cust_id not in (select Cust_id from datapath.cust_training);
run;

PROC LIFETEST DATA = datapath.test_Data1 OUTSURV =datapath.cust_outsurv
METHOD = LIFE PLOT = (S, H) WIDTH = 1 GRAPHICS;
TIME Duration*Status(0);
RUN;

/*training dataset with Use Variable*/
data datapath.cust_training;
set datapath.cust_training;
use = 0;
run;
/*validation dataset with Use variable*/
data datapath.cust_validation;
set datapath.cust_validation;
use = 1;
status = .;
Duration= .;
run;

/*append both*/
data datapath.appnd;
set datapath.cust_training
datapath.cust_validation;
run;

PROC LIFEREG DATA=datapath.appnd OUTEST=A;
MODEL Duration*Status(0) = age high_freq_cust_flg emp_status no_complaints marital_status Total_tran_18m/
DIST=Lnormal;
OUTPUT OUT=B XBETA=lp CONTROL =USE;
RUN;
/*Set the scale as generated in the above output*/
data datapath.predd;
set B;
_scale_=0.81691;
run;
/*log normal dist calculation for survival probability*/
/*Set the time period for which survival probabilityis required*/
data datapath.pred_prob;
set datapath.predd;
PROB=1-PROBNORM((LOG(18)-LP)/_SCALE_);
run;

data datapath.prob_at_time(Keep= cust_Id prob);
set datapath.pred_prob;
run;

