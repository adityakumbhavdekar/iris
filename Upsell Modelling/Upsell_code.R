###
setwd("C:/Users/babycorn/Documents/SelfStudy/Telecom upsell Case")
library(VIM)
library(car)
library(glmnet)
library(SamplingStrata)
library(sampling)

cust_data<-read.csv("RawdatafileV0.0.csv")
### See the data summary (verify Data) and prepare univariate report

head(cust_data)
tail(cust_data)
summ_cust_data.csv<-summary(cust_data)
write.csv(summ_cust_data.csv, "summ_cust_data.csv.csv")


##Data treatment

cust_dat1<- cust_data[,-c(12,29)]

cust_dat1$Var1<-ifelse(cust_dat1$Var1=="", "North", 
                       ifelse(cust_dat1$Var1=="South", "South",
                              ifelse(cust_dat1$Var1=="Central", "Central", "North")))

cust_dat1$Var4<-ifelse(cust_dat1$Var4 >50, 50, 
                       ifelse(cust_dat1$Var4< 25, 20 , 30))

summ_cust_data2.csv<-summary(cust_dat1)
write.csv(summ_cust_data2.csv, "summ_cust_data2.csv")
summary(cust_dat1$Var1)

###Generate responder variable
cust_dat1$Responder<-ifelse(cust_dat1$Plan_Chg_Flag == "Yes", 1, 0)


##Find correlation matrix for multicollinearity check
Corr_data<- cust_dat1[,-c(1,2,3,4,5,7,11)]
corr_matrix.csv <- cor(Corr_data,Corr_data)
write.csv(corr_matrix.csv, "corr_matrix.csv")


vif_Cust_data.csv <- vif(lm(Responder ~ Var4+  Var6+   Var7+       Var8+    Var11+	    Var12+	    Var13+	    
                              Var14+	    Var15+	    Var16+	    Var17+	    Var18+	    Var19+	    Var20+	    Var21+
                              Var22+	    Var23+	    Var24+	    Var25+	    Var26
                            , data=cust_dat1))

write.csv(vif_Cust_data.csv, "vif_Cust_data.csv")
read(vif1)

##Drop redundant variable used to create responder varaible

cust_dat1<-cust_dat1[,c(-2)]

##Perform bi variate analysis
dat<-cust_dat1
head(dat)

## create table to identify the var type

bi_var<-apply(dat,2,typeof)

## add var name for the type and added the flag ..default set to 1 for all var

bi_var<-data.frame(colnames(dat),bi_var,flag=1)

## set the row names as numbers

row.names(bi_var)<-1:nrow(bi_var)

## set the column names

colnames(bi_var)<-c("variable","var_type","flag")

## get the position for variables to set the flag as 0

bi_var$flag[which( bi_var$variable %in% c("Cust_id","Responder"))]<-0

## remove those with flag as 0

bi_var<-bi_var[bi_var$flag==1,]

## created an object to get bi var analysis 

event_rate<-NULL

## loop in till all the var in the table

for ( i in 1:nrow(bi_var))
  
{
  
  ## get the freq table for each var..ensure that the deleted var and numbers 
  
  ## are in sequence..eg responder should be at 2nd position
  
  aa<-as.matrix(table(dat[,i+2],dat[,27]))
  cc<-aa
  
  ## append var name and the categories in that variable
  
  bb<-cbind(rep(as.character(bi_var$variable[i]),nrow(aa)),row.names(aa))
  
  ## merge the name, cat and freq table
  
  aa<-data.frame(cbind(bb,aa))
  
  ## calc for ER, NER, WOE, IV and cum IV
  aa[,5]<-as.numeric(cc[,1])/sum(as.numeric(cc[,1]))
  aa[,6]<-as.numeric(cc[,2])/sum(as.numeric(cc[,2]))
  
  aa[,7]<-log(aa[,5]/aa[,6])
  
  aa[,8]<-(aa[,5]-aa[,6])*aa[,7]
  
  aa[,9]<-sum(aa[,8])
  

  ## append everything in new dataset
  
  event_rate<-rbind(event_rate,aa)
  
}

## give the column names for data created above ..after the for loop

colnames(event_rate)<-c("variable","Factor","Res","Non-Res","ER","NER","WOE","IV","Cum_IV")

head(event_rate)  
write.csv(event_rate, "event_rate_IV.csv")

##Bining and group variable creation
cust_dat1$GRPVar1<-ifelse(cust_dat1$Var1=="North",1,ifelse(cust_dat1$Var1=="South",2,3))
cust_dat1$GRPVar2<-ifelse(cust_dat1$Var2=="Low",1,ifelse(cust_dat1$Var2=="Medium",2,3))
cust_dat1$GRPVar3<-ifelse(cust_dat1$Var3=="Unemployed",1,ifelse(cust_dat1$Var3=="Govt",2,3))
cust_dat1$GRPVar4<-ifelse(cust_dat1$Var4 < 25,1,ifelse(cust_dat1$Var4< 40,2,3))
cust_dat1$GRPVar5<-ifelse(cust_dat1$Var5=="Male",1,2)
cust_dat1$GRPVar6<-ifelse(cust_dat1$Var6 < 2,1,2)
cust_dat1$GRPVar7<-ifelse(cust_dat1$Var7 < 500,1,ifelse(cust_dat1$Var7< 1000,2,3))
cust_dat1$GRPVar8<-ifelse(cust_dat1$Var8 < 13,1,2)
cust_dat1$GRPVar9<-ifelse(cust_dat1$Var9=="Cash",1,2)
cust_dat1$GRPVar11<-ifelse(cust_dat1$Var11 < 2,1,2)
cust_dat1$GRPVar12<-ifelse(cust_dat1$Var12 < 2,1,2)
cust_dat1$GRPVar13<-ifelse(cust_dat1$Var13 < 500,1,2)
cust_dat1$GRPVar14<-ifelse(cust_dat1$Var14 < 500,1,2)
cust_dat1$GRPVar15<-ifelse(cust_dat1$Var15 < 500,1,2)
cust_dat1$GRPVar16<-ifelse(cust_dat1$Var16 < 200,1,2)
cust_dat1$GRPVar17<-ifelse(cust_dat1$Var17 < 20,1,2)
cust_dat1$GRPVar18<-ifelse(cust_dat1$Var18 < 100,1,2)
cust_dat1$GRPVar19<-ifelse(cust_dat1$Var19 < 1000,1,2)
cust_dat1$GRPVar20<-ifelse(cust_dat1$Var20 < 2,1,2)
cust_dat1$GRPVar21<-ifelse(cust_dat1$Var21 < 2,1,2)
cust_dat1$GRPVar22<-ifelse(cust_dat1$Var22 < 500,1,2)
cust_dat1$GRPVar23<-ifelse(cust_dat1$Var23 < 500,1,2)
cust_dat1$GRPVar24<-ifelse(cust_dat1$Var24 < 50,1,2)
cust_dat1$GRPVar25<-ifelse(cust_dat1$Var25 < 100,1,2)
cust_dat1$GRPVar26<-ifelse(cust_dat1$Var26 < 30,1,2)

colnames(cust_dat1)

cust_modeldata<-cust_dat1[,c(27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52)]
write.csv(cust_modeldata, "cust_modeldata.csv")

## sorting the data
cust_modeldata <- cust_modeldata[order(cust_modeldata$Responder,decreasing = TRUE),]
training_data =strata(cust_modeldata,c("Responder"),size=c(350,3150), method="srswor")
training_data<-getdata(cust_modeldata,training_data)
View(training_data)

##Run the code to generate model

fit <- glm(Responder ~ as.factor(GRPVar2)+as.factor(GRPVar7)+as.factor(GRPVar8)+as.factor(GRPVar9)+
             as.factor(GRPVar11)+as.factor(GRPVar12)+as.factor(GRPVar1)+as.factor(GRPVar14)+
             as.factor(GRPVar15)+as.factor(GRPVar16)+as.factor(GRPVar17)+as.factor(GRPVar18)+
             as.factor(GRPVar19)+as.factor(GRPVar20)+as.factor(GRPVar21)+as.factor(GRPVar22)+
             as.factor(GRPVar23)+as.factor(GRPVar24)+as.factor(GRPVar25), 
           family = binomial("logit"),data=training_data )


summary(fit) # display results
predict(fit, type="response") # predicted values
residuals(fit, type="deviance") # residuals

##SAve the summary of model output
summary_model.csv<-summary(fit)
write.csv(summary_model.csv, "summary_model.csv")

summary_residuals_model.csv<-residuals(fit, type="deviance")
write.csv(summary_residuals_model.csv, "summary_residuals_model.csv")


summary_pred_model.csv<-predict(fit, type="response")
write.csv(summary_pred_model.csv, "summary_pred_model.csv")


###############################################################################
### Hosmer lemeshow goodness of fit test
hosmerlem <- function (y, yhat, g = 12) 
  
{
  
  cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0,1, 1/g)), include.lowest = T)
  
  obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
  
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  
  chisq <- sum((obs - expect)^2/expect)
  
  P <- 1 - pchisq(chisq, g - 2)
  
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
  
}

R_hat<-as.vector(fitted(fit1))
yhat<-R_hat
y<-training_data$Responder

hosmerlem(y, yhat)

###############################################################
#### Preparing Gains/Lift chart
library(ROCR)
gain.chart <- function(y_hat,y) {
  plot(performance(prediction(y_hat,y), "tpr", "rpp"),lwd = 7, main = "Lift Chart")
  lines(ecdf((rank(-y_hat)[y == T]) / length(y)),verticals = T, do.points = F, col = "red", lwd = 3)
}

gain.chart(R_hat,training_data$Responder)

##############################################################

### Test for concordance -C value and Gini Coefficient
# Assuming the input is a stored binomial GLM object

outcome_and_fitted_col<-data.frame(training_data$Responder, R_hat)
colnames(outcome_and_fitted_col)<-c("Responder","fitted.values")
Concordance = function(outcome_and_fitted_col)
  
{
  
  #outcome_and_fitted_col = cbind(logistic1$Responder, logistic1$fitted.values)
  # get a subset of outcomes where the event actually happened
  
  ones = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 1,]
  
  # get a subset of outcomes where the event didn't actually happen
  
  zeros = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 0,]
  
  # Equate the length of the event and non-event tables
  
  if (length(ones[,1])>length(zeros[,1])) {ones = ones[1:length(zeros[,1]),]}
  
  else {zeros = zeros[1:length(ones[,1]),]}
  
  # Following will be c(ones_outcome, ones_fitted, zeros_outcome, zeros_fitted)
  
  ones_and_zeros = data.frame(ones, zeros)
  
  # initiate columns to store concordant, discordant, and tie pair evaluations
  
  conc = rep(NA, length(ones_and_zeros[,1]))
  disc = rep(NA, length(ones_and_zeros[,1]))
  ties = rep(NA, length(ones_and_zeros[,1]))
  
  for (i in 1:length(ones_and_zeros[,1])) {
    
    # This tests for concordance
    
    if (ones_and_zeros[i,2] > ones_and_zeros[i,4])
      
    {conc[i] = 1
     disc[i] = 0
     ties[i] = 0}
    
    # This tests for a tie
    
    else if (ones_and_zeros[i,2] == ones_and_zeros[i,4])
      
    {
      
      conc[i] = 0
      disc[i] = 0
      ties[i] = 1
      
    }
    
    # This should catch discordant pairs.
    
    else if (ones_and_zeros[i,2] < ones_and_zeros[i,4])
      
    {
      
      conc[i] = 0
      disc[i] = 1
      ties[i] = 0
      
    }
    
  }
  
  # Here we save the various rates
  
  conc_rate = mean(conc, na.rm=TRUE)
  disc_rate = mean(disc, na.rm=TRUE)
  tie_rate = mean(ties, na.rm=TRUE)
  
  return(list(concordance=conc_rate, num_concordant=sum(conc), discordance=disc_rate, num_discordant=sum(disc), tie_rate=tie_rate,num_tied=sum(ties)))
  
}

Concordance_test<-Concordance(outcome_and_fitted_col)
Concordance_test
