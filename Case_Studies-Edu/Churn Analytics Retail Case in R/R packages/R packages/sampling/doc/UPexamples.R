### R code from vignette source 'UPexamples.Snw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: UPexamples.Snw:21-25
###################################################
library(sampling)
ps.options(pointsize=12)
options(width=60)



###################################################
### code chunk number 2: up1
###################################################
b=data(belgianmunicipalities)
pik=inclusionprobabilities(belgianmunicipalities$Tot04,200)
N=length(pik)
n=sum(pik)



###################################################
### code chunk number 3: up2
###################################################
sim=10
ss=array(0,c(sim,8))




###################################################
### code chunk number 4: up3
###################################################
y=belgianmunicipalities$TaxableIncome



###################################################
### code chunk number 5: up4
###################################################
ht=numeric(8)
for(i in 1:sim)
{
cat("Step ",i,"\n")
s=UPpoisson(pik)
ht[1]=HTestimator(y[s==1],pik[s==1])
s=UPrandomsystematic(pik)
ht[2]=HTestimator(y[s==1],pik[s==1])
s=UPrandompivotal(pik)
ht[3]=HTestimator(y[s==1],pik[s==1])
s=UPtille(pik)
ht[4]=HTestimator(y[s==1],pik[s==1])
s=UPmidzuno(pik)
ht[5]=HTestimator(y[s==1],pik[s==1])
s=UPsystematic(pik)
ht[6]=HTestimator(y[s==1],pik[s==1])
s=UPpivotal(pik)
ht[7]=HTestimator(y[s==1],pik[s==1])
s=srswor(n,N)
ht[8]=HTestimator(y[s==1],rep(n/N,n))
ss[i,]=ht
}



###################################################
### code chunk number 6: up5
###################################################
colnames(ss) <- 
c("poisson","rsyst","rpivotal","tille","midzuno","syst","pivotal","srswor")
boxplot(data.frame(ss), las=3)






###################################################
### code chunk number 7: UPexamples.Snw:100-108 (eval = FALSE)
###################################################
## b=data(belgianmunicipalities)
## pik=inclusionprobabilities(belgianmunicipalities$Tot04,200)
## N=length(pik)
## n=sum(pik)
## 
## sim=10
## ss=array(0,c(sim,8))
## 
## 
## y=belgianmunicipalities$TaxableIncome
## 
## ht=numeric(8)
## for(i in 1:sim)
## {
## cat("Step ",i,"\n")
## s=UPpoisson(pik)
## ht[1]=HTestimator(y[s==1],pik[s==1])
## s=UPrandomsystematic(pik)
## ht[2]=HTestimator(y[s==1],pik[s==1])
## s=UPrandompivotal(pik)
## ht[3]=HTestimator(y[s==1],pik[s==1])
## s=UPtille(pik)
## ht[4]=HTestimator(y[s==1],pik[s==1])
## s=UPmidzuno(pik)
## ht[5]=HTestimator(y[s==1],pik[s==1])
## s=UPsystematic(pik)
## ht[6]=HTestimator(y[s==1],pik[s==1])
## s=UPpivotal(pik)
## ht[7]=HTestimator(y[s==1],pik[s==1])
## s=srswor(n,N)
## ht[8]=HTestimator(y[s==1],rep(n/N,n))
## ss[i,]=ht
## }
## 
## colnames(ss) <- 
## c("poisson","rsyst","rpivotal","tille","midzuno","syst","pivotal","srswor")
## boxplot(data.frame(ss), las=3)
## 
## 
## 
## 
## 
## sampling.newpage()
## 


