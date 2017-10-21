libname  datapath "C:/Users/babycorn/Documents/Edupristine/market mix model/" ; 
PROC IMPORT OUT= WORK.brijesh 
            DATAFILE= "C:\Users\babycorn\Documents\Edupristine\market mix model\MMM_ds_1.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data datapath.mmmds01;
set work.brijesh;
run;
proc print data = datapath.mmmds01;
run;
proc contents data = datapath.mmmds01;
/*Verification for missing value treatment*/
proc means data =datapath.mmmds01 n nmiss ;  
class ln_sales;  
run ;
/*multicollinearity fix*/
ods listing select FitStatistics ParameterEstimates anova;
proc reg data = datapath.mmmds01;
 model ln_sales = Cmpgn1
campgn2
campgn3
ln_P_A
ln_P_B
ln_P_C/vif tol;
ods output ParameterEstimates = VIF;
run;
quit;
/*The model is prepared by running the proc glm code*/
proc glm data = datapath.mmmds01;
class region_cd;
 model ln_sales = Cmpgn1
campgn2
campgn3
ln_P_A
ln_P_B
ln_P_C/ solution;
ods output solutionf = FXD_ln_sales;
run;
quit;

/*assigning the mean values*/
proc  means data = datapath.mmmds01;
class region_cd;
var ln_sales Cmpgn1 campgn2 campgn3 ln_P_A ln_P_B ln_P_C;
output out = datapath.regionLevelMean mean = mln_sales mCmpgn1 mcampgn2 mcampgn3 mln_P_A mln_P_B mln_P_C;
run;
proc print data = datapath.regionLevelMean;
run;
/*append means to the dataset*/
proc  sort data = datapath.mmmds01 out=datapath.meancnt;
by region_cd; run;
/*sort data to merge with previous dataset*/
proc  sort data = datapath.regionLevelMean;
by region_cd; run;
/*merge dataset with previous dataset*/
data datapath.mmmds01meancnt;
merge datapath.regionLevelMean(in=a) datapath.meancnt(in =b);
by region_cd; 
if a and b;
run;
/*print dataset to verify the new merged dataset*/
proc print data = datapath.mmmds01meancnt1;
run;
/*calculation based on the seasonality factor  1.05874 and extimates  as derived from model and taking mean centering in consideration*/
/*(keep = ln_sales Cmpgn1 campgn2 campgn3 ln_P_A ln_P_B ln_P_C mln_sales mCmpgn1 mcampgn2 mcampgn3 mln_P_A mln_P_B mln_P_C timeperiod region_cd pred sales res)*/
data datapath.mmmds01meancnt1; 
set datapath.mmmds01meancnt;
pred = 1.05874*exp(0.1356*(Cmpgn1-mCmpgn1)+ 0.2432*(campgn2-mcampgn2)+ 0.3532*(campgn3-mcampgn3)+ 0.03772*(ln_P_A-mln_P_A)-
0.3618*(ln_P_B-mln_P_B)+ 0.3792*(ln_P_C-mln_P_C)+ mln_sales);
res = pred - sales;
abs_res = abs(res);
mape = 100*abs_res/sales;
run;
quit;
/*validate the summary of output*/
proc summary data =datapath.mmmds01meancnt1 nmiss nway;
class timeperiod;
var pred sales;
output out=datapath.test10 sum=;
run;
quit;

proc print data = datapath.test10;
run;
/*preparing the contribution matrix */
data datapath.contribution (keep = ln_sales Cmpgn1 campgn2 campgn3 ln_P_A ln_P_B ln_P_C 
mln_sales mCmpgn1 mcampgn2 mcampgn3 mln_P_A mln_P_B mln_P_C 
timeperiod region_cd pred sales res
pred_Cmpgn1 pred_campgn2 pred_campgn3 pred_ln_P_A pred_ln_P_B pred_ln_P_C
contr_Cmpgn1 contr_campgn2 contr_campgn3 contr_ln_P_A contr_ln_P_B contr_ln_P_C);
set datapath.mmmds01meancnt;
pred = 1.05874*exp(0.1356*(Cmpgn1-mCmpgn1)+ 0.2432*(campgn2-mcampgn2)+ 0.3532*(campgn3-mcampgn3)+ 0.03772*(ln_P_A-mln_P_A)-
0.3618*(ln_P_B-mln_P_B)+ 0.3792*(ln_P_C-mln_P_C)+ mln_sales);
res = pred - sales;

pred_Cmpgn1 = 1.05874*exp(0.1356*(-mCmpgn1)+ 0.2432*(campgn2-mcampgn2)+ 0.3532*(campgn3-mcampgn3)+ 0.03772*(ln_P_A-mln_P_A)-
0.3618*(ln_P_B-mln_P_B)+ 0.3792*(ln_P_C-mln_P_C)+ mln_sales);
contr_Cmpgn1 = pred - pred_Cmpgn1;

pred_campgn2 = 1.05874*exp(0.1356*(Cmpgn1-mCmpgn1)+ 0.2432*(-mcampgn2)+ 0.3532*(campgn3-mcampgn3)+ 0.03772*(ln_P_A-mln_P_A)-
0.3618*(ln_P_B-mln_P_B)+ 0.3792*(ln_P_C-mln_P_C)+ mln_sales);
contr_campgn2 = pred - pred_campgn2;

pred_campgn3 = 1.05874*exp(0.1356*(Cmpgn1-mCmpgn1)+ 0.2432*(campgn2-mcampgn2)+ 0.3532*(-mcampgn3)+ 0.03772*(ln_P_A-mln_P_A)-
0.3618*(ln_P_B-mln_P_B)+ 0.3792*(ln_P_C-mln_P_C)+ mln_sales);
contr_campgn3 = pred - pred_campgn3;

pred_ln_P_A = 1.05874*exp(0.1356*(Cmpgn1-mCmpgn1)+ 0.2432*(campgn2-mcampgn2)+ 0.3532*(campgn3-mcampgn3)+ 0.03772*(-mln_P_A)-
0.3618*(ln_P_B-mln_P_B)+ 0.3792*(ln_P_C-mln_P_C)+ mln_sales);
contr_ln_P_A = pred - pred_ln_P_A;

pred_ln_P_B = 1.05874*exp(0.1356*(Cmpgn1-mCmpgn1)+ 0.2432*(campgn2-mcampgn2)+ 0.3532*(campgn3-mcampgn3)+ 0.03772*(ln_P_A-mln_P_A)-
0.3618*(-mln_P_B)+ 0.3792*(ln_P_C-mln_P_C)+ mln_sales);
contr_ln_P_B = pred - pred_ln_P_B;

pred_ln_P_C = 1.05874*exp(0.1356*(Cmpgn1-mCmpgn1)+ 0.2432*(campgn2-mcampgn2)+ 0.3532*(campgn3-mcampgn3)+ 0.03772*(ln_P_A-mln_P_A)-
0.3618*(ln_P_B-mln_P_B)+ 0.3792*(-mln_P_C)+ mln_sales);
contr_ln_P_C = pred - pred_ln_P_C;
run;

proc print data = datapath.contribution;
run;
/*Export the contribution matrix for further analysis*/
PROC EXPORT DATA= DATAPATH.CONTRIBUTION 
            OUTFILE= "C:\Users\babycorn\Documents\Edupristine\market mix
 model\contributionDatafile.xls" 
            DBMS=EXCEL2000 REPLACE;
     SHEET="ContributionDatafile"; 
RUN;





