libname  datapath "C:/Users/babycorn/Documents/Edupristine/case study for Xsell model/" ; 
PROC IMPORT OUT= WORK.brijesh 
            DATAFILE= "C:\Users\babycorn\Documents\Edupristine\case study for Xsell model\cust_data.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data datapath.custds01;
set work.brijesh;
run;
proc print data = datapath.custds01;
run;
proc contents data = datapath.custds01;
run;
proc univariate data = datapath.custds01;
var age;
run;
/*Verification for missing value treatment*/
 proc means data =datapath.custds01 n nmiss ;  
class responder;  
run ;
proc freq data =datapath.custds01;  
table Channel*responder/ norow nocol nopercent;  
run;  
/*Fixing Missing values*/
 data datapath.custds01;  
set datapath.custds01;
if  Channel = '' then Channel = 'Missing';  
if  age = . then age = 43; 
run;

proc sql;
select age, channel from datapath.custds01 where master_id = 13002;
run;

 data datapath.custds01;  
set datapath.custds01;
if  master_id = 13002 then age = 970;  
if  master_id = 13002 then channel = ''; 
run;

/*Fixing Missing values*/
 data datapath.custds01;  
 set datapath.custds01;  
if Channel= '' then Channel= 'Direct';  
if age = . then age = 43; 
run
; 
/*Outlier treatment*/
/*list the problematic variable*/
proc means data =datapath.custds01 p99 max; 
run; 
/*fix the problem variable*/
data datapath.custds01;  
set datapath.custds01;  
if age > 60 then age = 60; 
run; 
proc reg data = datapath.custds01 corr; 
model responder = 
Age
/*Channel*/
/*FS_code*/
/*Marital_status*/
/*Gender*/
/*Prosperity_Index*/
WSI
IncomeGrp
No_of_prod1
MSL_prod1
No_of_prod2
MSL_prod2
num_of_cars
Family_doctor
/vif collin; 
run; 
/*generating categories*/
proc freq data =datapath.custds01;  
table FS_code*responder
Marital_status*responder
Channel*responder
Marital_status*responder
Prosperity_Index*responder/ norow nocol nocum nopercent;  
run;
/*Final interactive binnig: Running code to create group variables*/
data datapath.custds02(compress = yes);  
set datapath.custds01;; 
length _UFormat $200;
 drop _UFormat;
_UFormat='';
*------------------------------------------------------------*;
* Variable: age;
*------------------------------------------------------------*;
LABEL GRP_age = 'Grouped: age';
if MISSING(age) then do;
GRP_age = .;
end;
else if NOT MISSING(age) then do;
if age < 25 then do;
GRP_age = 1;
 end;
else
if 25 <= age <50 then do;
GRP_age = 2;
end;
else do;
GRP_age = 3;
end;
end;

*------------------------------------------------------------*;
* Variable: channel;
*------------------------------------------------------------*;
LABEL GRP_channel = 'Grouped: Channel';
if MISSING(Channel) then do;
GRP_channel = .;
end;
else if NOT MISSING(Channel) then do;
if (Channel = "Direct") then do;
GRP_channel = 1;
end;
else
if (Channel = "Broker") then do;
GRP_channel = 2;
 end;
else do;
GRP_channel = 3;
end;
end;

*------------------------------------------------------------*;
* Variable: Gender;
*------------------------------------------------------------*;
LABEL GRP_Gender = 'Grouped: Gender';
if MISSING(Gender) then do;
GRP_Gender = .;
end;
else if NOT MISSING(Gender) then do;
if (Gender = "Male") then do;
GRP_Gender = 1;
 end;
else
if (Gender = "Female") then do;
GRP_Gender = 2;
end;
else do;
GRP_Gender = 3;
end;
end;
*------------------------------------------------------------*;
* Variable: Marital status;
*------------------------------------------------------------*;

LABEL GRP_Marital_status = 'Grouped: Marital_status';
if MISSING(Marital_status) then do;
GRP_Marital_status = .;
end;
else if NOT MISSING(Marital_status) then do;
if (Marital_status = "Yes") then do;
GRP_Marital_status = 1;
 end;
else
if (Marital_status = "No") then do;
GRP_Marital_status = 2;
end;
else do;
GRP_Marital_status = 3;
end;
end;

*------------------------------------------------------------*;
* Variable: FS_Code;
*------------------------------------------------------------*;
LABEL GRP_FS_Code = 'Grouped: FS_Code';
if MISSING(FS_Code) then do;
GRP_FS_Code = .;
end;
else if NOT MISSING(FS_Code) then do;
if (FS_Code = "A" ) then do;
GRP_FS_Code = 1;
 end;
else
if (FS_Code = 'B' OR FS_Code eq 'C' OR FS_Code eq 'D') then do;
GRP_FS_Code = 2;
end;
else
if (FS_Code = 'E') then do;
GRP_FS_Code = 3;
end;
else do;
GRP_FS_Code = 4;
end;
end;
*------------------------------------------------------------*;
* Variable: Prosperity_Index;
*------------------------------------------------------------*;
LABEL GRP_Prosperity_Index = 'Grouped: Prosperity_Index';
if MISSING(Prosperity_Index) then do;
GRP_Prosperity_Index = .;
end;
else if NOT MISSING(Prosperity_Index) then do;
if (Prosperity_Index = "High") then do;
GRP_Prosperity_Index = 1;
 end;
else
if (Prosperity_Index = "Medium") then do;
GRP_Prosperity_Index = 2;
end;
else do;
GRP_Prosperity_Index = 2;
end;
end;
*------------------------------------------------------------*;
* Variable: No of prod 1;
*------------------------------------------------------------*;
LABEL GRP_No_of_prod1 = 'Grouped: No_of_prod1';
if MISSING(No_of_prod1) then do;
GRP_No_of_prod1 = .;
end;
else if NOT MISSING(No_of_prod1) then do;
if (No_of_prod1 gt 5) then do;
GRP_No_of_prod1 = 1;
 end;
else
if (No_of_prod1 = 3 or No_of_prod1 = 4 or No_of_prod1 = 5 ) then do;
GRP_No_of_prod1 = 2;
end;
else do;
GRP_No_of_prod1 = 3;
end;
end;
*------------------------------------------------------------*;
* Variable: No of prod 2;
*------------------------------------------------------------*;
LABEL GRP_No_of_prod2 = 'Grouped: No_of_prod2';
if MISSING(No_of_prod2) then do;
GRP_No_of_prod2 = .;
end;
else if NOT MISSING(No_of_prod2) then do;
if (No_of_prod2 gt 5) then do;
GRP_No_of_prod2 = 1;
 end;
else
if (No_of_prod2 = 3 or No_of_prod2 = 4 or No_of_prod2 = 5 ) then do;
GRP_No_of_prod2 = 2;
end;
else do;
GRP_No_of_prod2 = 3;
end;
end;
*------------------------------------------------------------*;
* Variable: MSLprod1;
*------------------------------------------------------------*;
LABEL GRP_MSL_prod1 = 'Grouped: MSL_prod1';
if MISSING(MSL_prod1) then do;
GRP_MSL_prod1 = .;
end;
else if NOT MISSING(MSL_prod1) then do;
if (MSL_prod1 gt 24) then do;
GRP_MSL_prod1 = 1;
 end;
else
if (24 >= MSL_prod1 >= 12 ) then do;
GRP_MSL_prod1 = 2;
end;
else do;
GRP_MSL_prod1 = 1;
end;
end;
run;

proc print data = datapath.custds02training ;
run;

proc sort data = datapath.custds02;
by responder;
run;
/*selecting training sample data*/
proc surveyselect data = datapath.custds02
out = datapath.custds02training 
seed = 7 
method = srs n =(1922 213); 
strata responder ; 
run;
/*selecting validation sample data*/
proc sql;
create table datapath.custds02validation as
select * from datapath.custds02 where master_id not in (select master_id from datapath.custds02training);
run;
/*logistic regression modeling code*/
proc logistic data = datapath.custds02training descending; 
class
GRP_age
GRP_channel 
GRP_Gender
GRP_Marital_status 
/*GRP_FS_Code */
GRP_Prosperity_Index 
GRP_No_of_prod1 
GRP_No_of_prod2 
GRP_MSL_prod1
;
model  responder = 
GRP_age
GRP_channel 
GRP_Gender
GRP_Marital_status 
/*GRP_FS_Code */
GRP_Prosperity_Index 
GRP_No_of_prod1 
GRP_No_of_prod2 
GRP_MSL_prod1
/selection = stepwise sls = 0.005 sle = 0.05 ctable lackfit; 
output out = logistic p = pred_score ;
run;

proc logistic data = datapath.custds02validation descending; 
class
GRP_age
GRP_channel 
GRP_Gender
GRP_Marital_status 
/*GRP_FS_Code */
GRP_Prosperity_Index 
GRP_No_of_prod1 
GRP_No_of_prod2 
GRP_MSL_prod1
;
model  responder = 
GRP_age
GRP_channel 
GRP_Gender
GRP_Marital_status 
/*GRP_FS_Code */
GRP_Prosperity_Index 
GRP_No_of_prod1 
GRP_No_of_prod2 
GRP_MSL_prod1
/selection = stepwise sls = 0.005 sle = 0.05 ctable lackfit; 
output out = logistic p = pred_score ;
run; 




