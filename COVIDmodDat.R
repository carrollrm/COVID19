setwd("C:/Users/carrollr/OneDrive - UNC-Wilmington/COVID19/COVIDmod")

library(gdata)
library(INLA)
library(rgdal)
library(SDMTools)


health=read.csv("data/UScountyORderPR1.csv")
regions <- readOGR("data/USregions.shp")#local

##############JHU and other data set up#############
UScntyPop=read.csv("https://raw.githubusercontent.com/carrollrm/UScntyPop/master/UScntyPop.csv", header=T)

dates=format(seq(as.Date("03-23-2020",format="%m-%d-%Y"),
                 as.Date(Sys.time(),tz="US/Eastern")-1, 
                 by="days"),format="%m-%d-%Y")
us.ts.c=us.ts.d=us.ts.r=us.ts.n=
  us.ts.c.pop=us.ts.d.pop=us.ts.r.pop=matrix(0,nrow=length(dates),ncol=3141)
for (i in 1:(length(dates))){
  nm=read.csv(paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",dates[i],".csv",sep=""), header=T)
  nmus=nm[which(nm$Country_Region=="US"),]
  if (is.null(nmus$FIPS)){
    nmus$FIPS=nmus[,1]
  }
  nmus=nmus[order(nmus$FIPS),]
  nmus=nmus[-which(nmus$FIPS==60000|nmus$FIPS==66000|
                     nmus$FIPS==69000|nmus$FIPS==78000|
                     nmus$FIPS>78999|nmus$FIPS<=1000|
                     nmus$FIPS==2016|is.na(nmus$FIPS)),]
  if(dim(nmus[which(nmus$FIPS==32007|nmus$FIPS==35013|nmus$FIPS==53071),])[1]==6){
    nmus=nmus[-c(1750,1804,2988),]  
  } else if (dim(nmus[which(nmus$FIPS==32007|nmus$FIPS==35013),])[1]==4){
    nmus=nmus[-c(1750,1804),]  
  } else if (dim(nmus[which(nmus$FIPS==35013),])[1]==2){
    nmus=nmus[-1803,]  
  } else if (dim(nmus[which(nmus$FIPS==49053),])[1]==2){
    nmus$Confirmed[which(nmus$FIPS==49053)[1]]=sum(nmus$Confirmed[which(nmus$FIPS==49053)])
    nmus=nmus[-which(nmus$FIPS==49053)[2],]  
  } else {nmus=nmus}
  if (dim(nmus)[1]<3141){
    nmus1=nmus
    nmus=nmus.old
    nmus$Confirmed[which(nmus$FIPS%in%nmus1$FIPS)]=nmus1$Confirmed
    nmus$Recovered[which(nmus$FIPS%in%nmus1$FIPS)]=nmus1$Recovered
    nmus$Deaths[which(nmus$FIPS%in%nmus1$FIPS)]=nmus1$Deaths
  }
  nmus$pop=UScntyPop$pop
  us.ts.c[i,]=(nmus$Confirmed)
  if (i==1){
    us.ts.n[i,]=us.ts.c[i,]
  } else {
    us.ts.n[i,]=us.ts.c[i,]-us.ts.c[i-1,]
  }
  us.ts.n[i,]=(nmus$Confirmed)
  us.ts.d[i,]=(nmus$Deaths)
  us.ts.r[i,]=(nmus$Recovered)
  
  if (i!=length(dates)){  
    nmus.old=nmus[,-13]
  }
}

###escambia co expl
# barplot(us.ts.n[-(1:11),335],main="Escambia County",xlab="Days since April 3, 2020",ylab="New Cases")
# plot(1:(length(dates)-1),us.ts.n[-1,335])
# day=(1:(length(dates)-1))
# summary(lm(us.ts.n[-(1:11),335]~day[-(1:10)]))
# confint(lm(us.ts.n[-(1:11),335]~day[-(1:10)]))
# abline(lm(us.ts.n[-(1:11),335]~day[-(1:10)]),col=2)
# 
# ###all fl expl
# FL=which(nmus$FIPS>12000&nmus$FIPS<12999)
# barplot(rowSums(us.ts.n[-(1:11),FL]),main="Florida State",xlab="Days since April 3, 2020",ylab="New Cases")
# plot(1:(length(dates)-1),rowSums(us.ts.n[-1,FL]),ylab="New Cases",xlab="Days since March 24, 2020")
# abline(lm(rowSums(us.ts.n[-1,FL])~day),col=2)
# abline(lm(rowSums(us.ts.n[-(1:10),FL])~day[-(1:9)]),col=3)
# summary(lm(rowSums(us.ts.n[-(1:10),FL])~day[-(1:9)]))
# abline(lm(rowSums(us.ts.n[-(1:11),FL])~day[-(1:10)]),col=4)
# summary(lm(rowSums(us.ts.n[-(1:11),FL])~day[-(1:10)]))
# confint(lm(rowSums(us.ts.n[-(1:11),FL])~day[-(1:10)]))


##################Regression############
###reg data
#preds
health=read.csv("data/UScountyORderPR1.csv")
nmus$HlthReg=nmus$FIPS
for (i in 1:3141){
  nmus$HlthReg[i]=health$orderReg[which(health$GEOID==nmus$FIPS[i])]
}
#s only preds
regpreds=read.csv("data/regpreds.csv")
intpassRegBin=regpreds$intpassRegBin
unemplyReg=regpreds$unemplyReg#march estimates
smokeReg=regpreds$smokeReg
ageGE65Reg=regpreds$ageGE65Reg
intMIGReg=regpreds$intMIGReg
AApopReg=regpreds$AApopReg
#t only pred
wkdys=as.character(weekdays(as.Date(dates,format="%m-%d-%Y")))
wkdysN=ifelse(wkdys=="Monday",7,ifelse(wkdys=="Tuesday",1,
        ifelse(wkdys=="Wednesday",2,ifelse(wkdys=="Thursday",3,
         ifelse(wkdys=="Friday",4,ifelse(wkdys=="Saturday",5,6))))))
wkdysBIN=ifelse(wkdys=="Thursday"|wkdys=="Friday"|wkdys=="Saturday",1,0)
#st preds
stay=read.csv("data/StayHome.csv")
stayGRD=read.csv("data/sds-v3-full-county.csv")
stayGRDdate=format(as.Date(stayGRD$date,format="%m/%d/%Y"),format="%m-%d-%Y")
stayGRD=stayGRD[-which(stayGRD$county_fips==2016),]
stayGRD=stayGRD[-which(stayGRDdate%in%dates==FALSE),]
stayGRDdate2=format(as.Date(stayGRD$date,format="%m/%d/%Y"),format="%m-%d-%Y")
stayGRDfips=unique(stayGRD$county_fips)
stayGRDreg=rep(0,length(stayGRDfips))
for (i in 1:length(stayGRDfips)){
  stayGRDreg[i]=nmus$HlthReg[which(nmus$FIPS==stayGRDfips[i])]
}
stayGRD$reg=rep.int(stayGRDreg,rep(length(unique(stayGRDdate2)),length(stayGRDfips)))
stayReg=staylagReg=stayGRDReg=rep(NA,389*length(dates))
for (i in 2:390){
  for (j in 1:length(dates)){
    stayReg[(i-1)+389*(j-1)]=stay[which(stay$FIPS==health$STATEFP[which(health$orderReg==i)[1]]),j+16]
    staylagReg[(i-1)+389*(j-1)]=stay[which(stay$FIPS==health$STATEFP[which(health$orderReg==i)[1]]),j+9]
    stayGRDReg[(i-1)+389*(j-1)]=mean(stayGRD$n_grade_total[which(stayGRD$reg==i&stayGRDdate2==dates[j])])
    }}
stayGRDcatReg=ifelse(stayGRDReg<1.5,"F",ifelse(stayGRDReg<2.5,"D",ifelse(stayGRDReg<3.5,"C",
                ifelse(stayGRDReg<4.5,"B","A"))))
#table(stayGRDcatReg)

#outcomes
ushr.ts.c=ushr.ts.n=ushr.ts.d=ushr.ts.a=ushr.ts.r=
  matrix(0,nrow=length(dates),ncol=389)
popReg=rep(0,389)
for (i in 1:389){
  cols=rep(NA,length(which(health$orderReg==i+1)))
  if (i==337){cols=cols[-length(cols)]}
  for (j in 1:length(cols)){
    cols[j]=which(nmus$FIPS==health$GEOID[which(health$orderReg==i+1)[j]])
  }
  if (length(cols)>1){
    ushr.ts.c[,i]=rowSums(us.ts.c[,cols])
    ushr.ts.d[,i]=rowSums(us.ts.d[,cols])
    ushr.ts.r[,i]=rowSums(us.ts.r[,cols])
  }
  else {
    ushr.ts.c[,i]=us.ts.c[,cols]
    ushr.ts.d[,i]=us.ts.d[,cols]
    ushr.ts.r[,i]=us.ts.r[,cols]
  }
  popReg[i]=sum(nmus$pop[cols])
}
ushr.ts.c.n=ushr.ts.a=ushr.ts.c
for (i in 2:length(dates)){
  ushr.ts.n[i,]=ushr.ts.c[i,]-ushr.ts.c[i-1,]
  ushr.ts.a[i,]=ushr.ts.c[i,]-ushr.ts.d[i,]-ushr.ts.r[i,]
}
ushr.ts.n=ifelse(ushr.ts.n<0,0,ushr.ts.n)
ushr.ts.a=ifelse(ushr.ts.a<0,0,ushr.ts.a)

#expected
#calc expect and sir
r.hr.c=rowSums(ushr.ts.c)/sum(popReg)
r.hr.n=rowSums(ushr.ts.n)/sum(popReg)
r.hr.d=rowSums(ushr.ts.d)/sum(popReg)
r.hr.a=rowSums(ushr.ts.a)/sum(popReg)
e.hr.c=e.hr.n=e.hr.d=e.hr.a=matrix(1,nrow=length(dates),ncol=dim(ushr.ts.c)[2])
for (i in 1:length(dates)){
  e.hr.c[i,]=r.hr.c[i]*popReg
  e.hr.n[i,]=r.hr.n[i]*popReg
  e.hr.d[i,]=r.hr.d[i]*popReg
  e.hr.a[i,]=r.hr.a[i]*popReg
}

t=Sys.time()
###regression models
f1 = y ~ as.factor(x1) +
  as.factor(x2) +
  as.factor(x3) +
  x4 +
  x5 +
  x6 +
  x7 +
  f(id,model="iid",param=c(2,1)) +
 # f(id3,model="iid",param=c(2,1))+
f(id2,model="rw1",param=c(2,1))
regSTfips=as.numeric(as.character(regions$STATEFP))

###us
d=data.frame(y=unmatrix(ushr.ts.c),
             id=rep(1:389,length(dates)),
             id2=rep(1:length(dates),each=389),
             id3=1:length(unmatrix(ushr.ts.c)),
             x1=rep(intpassRegBin,length(dates)),
             x2=rep(wkdysBIN,each=389),
             #    x2=rep(noncitzReg,length(dates)),
             x3=stayReg,x4=rep(unemplyReg,length(dates)),
             x5=rep(smokeReg,length(dates)),
             x6=rep(ageGE65Reg,length(dates)),
             x7=rep(AApopReg,length(dates)))
resUS.c=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.c),
             control.compute = list(dic=TRUE,waic=TRUE))
d=data.frame(y=unmatrix(ushr.ts.n),
             id=rep(1:389,length(dates)),
             id2=rep(1:length(dates),each=389),
             id3=1:length(unmatrix(ushr.ts.c)),
             x1=rep(intpassRegBin,length(dates)),
             x2=rep(wkdysBIN,each=389),
             #    x2=rep(noncitzReg,length(dates)),
             x3=stayReg,x4=rep(unemplyReg,length(dates)),
             x5=rep(smokeReg,length(dates)),
             x6=rep(ageGE65Reg,length(dates)),
             x7=rep(AApopReg,length(dates)))
resUS.n=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.n),
             control.compute = list(dic=TRUE,waic=TRUE))
d=data.frame(y=unmatrix(ushr.ts.a),
             id=rep(1:389,length(dates)),
             id2=rep(1:length(dates),each=389),
             id3=1:length(unmatrix(ushr.ts.c)),
             x1=rep(intpassRegBin,length(dates)),
             x2=rep(wkdysBIN,each=389),
             #    x2=rep(noncitzReg,length(dates)),
             x3=stayReg,x4=rep(unemplyReg,length(dates)),
             x5=rep(smokeReg,length(dates)),
             x6=rep(ageGE65Reg,length(dates)),
             x7=rep(AApopReg,length(dates)))
resUS.a=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.a),
             control.compute = list(dic=TRUE,waic=TRUE))
d=data.frame(y=unmatrix(ushr.ts.d),
             id=rep(1:389,length(dates)),
             id2=rep(1:length(dates),each=389),
             id3=1:length(unmatrix(ushr.ts.c)),
             x1=rep(intpassRegBin,length(dates)),
             x2=rep(wkdysBIN,each=389),
             #    x2=rep(noncitzReg,length(dates)),
             x3=stayReg,x4=rep(unemplyReg,length(dates)),
             x5=rep(smokeReg,length(dates)),
             x6=rep(ageGE65Reg,length(dates)),
             x7=rep(AApopReg,length(dates)))
resUS.d=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.d),
             control.compute = list(dic=TRUE,waic=TRUE))

#NE
NE=which(regSTfips==9|regSTfips==23|regSTfips==25|regSTfips==34|
           regSTfips==33|regSTfips==36|regSTfips==42|
           regSTfips==44|regSTfips==50)-1
regionsNE=regions[NE+1,]
stayKeep=rep(FALSE,389*length(dates))
for (i in 1:length(dates)){
  stayKeep[NE+389*(i-1)]=TRUE
}
r.hr.c=rowSums(ushr.ts.c[,NE])/sum(popReg[NE])
e.hr.c=matrix(1,nrow=length(dates),ncol=dim(ushr.ts.c[,NE])[2])
r.hr.n=rowSums(ushr.ts.n[,NE])/sum(popReg[NE])
e.hr.n=matrix(1,nrow=length(dates),ncol=dim(ushr.ts.n[,NE])[2])
r.hr.a=rowSums(ushr.ts.a[,NE])/sum(popReg[NE])
e.hr.a=matrix(1,nrow=length(dates),ncol=dim(ushr.ts.a[,NE])[2])
r.hr.d=rowSums(ushr.ts.d[,NE])/sum(popReg[NE])
e.hr.d=matrix(1,nrow=length(dates),ncol=dim(ushr.ts.d[,NE])[2])
for (i in 1:length(dates)){
  e.hr.c[i,]=r.hr.c[i]*popReg[NE]
  e.hr.n[i,]=r.hr.n[i]*popReg[NE]
  e.hr.a[i,]=r.hr.a[i]*popReg[NE]
  e.hr.d[i,]=r.hr.d[i]*popReg[NE]
}
d=data.frame(y=unmatrix(ushr.ts.c[,NE]),
             id=rep(1:length(regionsNE),length(dates)),
             id2=rep(1:length(dates),each=length(NE)),
             id3=1:length(unmatrix(ushr.ts.c[,NE])),
             x1=rep(intpassRegBin[NE],length(dates)),
             x2=rep(wkdysBIN,each=length(NE)),
             #     x2=rep(noncitzReg[NE],length(dates)),
             x3=stayReg[stayKeep],x4=rep(unemplyReg[NE],length(dates)),
             x5=rep(smokeReg[NE],length(dates)),
             x6=rep(ageGE65Reg[NE],length(dates)),
             x7=rep(AApopReg[NE],length(dates)))
resNE.c=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.c),
             control.compute = list(dic=TRUE,waic=TRUE))
d=data.frame(y=unmatrix(ushr.ts.n[,NE]),
             id=rep(1:length(regionsNE),length(dates)),
             id2=rep(1:length(dates),each=length(NE)),
             id3=1:length(unmatrix(ushr.ts.n[,NE])),
             x1=rep(intpassRegBin[NE],length(dates)),
             x2=rep(wkdysBIN,each=length(NE)),
             #     x2=rep(noncitzReg[NE],length(dates)),
             x3=stayReg[stayKeep],x4=rep(unemplyReg[NE],length(dates)),
             x5=rep(smokeReg[NE],length(dates)),
             x6=rep(ageGE65Reg[NE],length(dates)),
             x7=rep(AApopReg[NE],length(dates)))
resNE.n=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.n),
             control.compute = list(dic=TRUE,waic=TRUE))
d=data.frame(y=unmatrix(ushr.ts.a[,NE]),
             id=rep(1:length(regionsNE),length(dates)),
             id2=rep(1:length(dates),each=length(NE)),
             id3=1:length(unmatrix(ushr.ts.a[,NE])),
             x1=rep(intpassRegBin[NE],length(dates)),
             x2=rep(wkdysBIN,each=length(NE)),
             #     x2=rep(noncitzReg[NE],length(dates)),
             x3=stayReg[stayKeep],x4=rep(unemplyReg[NE],length(dates)),
             x5=rep(smokeReg[NE],length(dates)),
             x6=rep(ageGE65Reg[NE],length(dates)),
             x7=rep(AApopReg[NE],length(dates)))
resNE.a=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.a),
             control.compute = list(dic=TRUE,waic=TRUE))
d=data.frame(y=unmatrix(ushr.ts.d[,NE]),
             id=rep(1:length(regionsNE),length(dates)),
             id2=rep(1:length(dates),each=length(NE)),
             id3=1:length(unmatrix(ushr.ts.a[,NE])),
             x1=rep(intpassRegBin[NE],length(dates)),
             x2=rep(wkdysBIN,each=length(NE)),
             #     x2=rep(noncitzReg[NE],length(dates)),
             x3=stayReg[stayKeep],x4=rep(unemplyReg[NE],length(dates)),
             x5=rep(smokeReg[NE],length(dates)),
             x6=rep(ageGE65Reg[NE],length(dates)),
             x7=rep(AApopReg[NE],length(dates)))
resNE.d=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.d),
             control.compute = list(dic=TRUE,waic=TRUE))
#MW
#IA, IL, IN, KS, MN, MI, MO, ND, NE, OH, SD, WI
MW=which(regSTfips==19|regSTfips==17|regSTfips==18|regSTfips==20|
           regSTfips==27|regSTfips==29|regSTfips==26|
           regSTfips==38|regSTfips==31|regSTfips==39|
           regSTfips==46|regSTfips==55)-1
regionsMW=regions[MW+1,]
stayKeep=rep(FALSE,389*length(dates))
for (i in 1:length(dates)){
  stayKeep[MW+389*(i-1)]=TRUE
}
r.hr.c=rowSums(ushr.ts.c[,MW])/sum(popReg[MW])
e.hr.c=matrix(1,nrow=length(dates),ncol=dim(ushr.ts.c[,MW])[2])
r.hr.n=rowSums(ushr.ts.n[,MW])/sum(popReg[MW])
e.hr.n=matrix(1,nrow=length(dates),ncol=dim(ushr.ts.n[,MW])[2])
r.hr.a=rowSums(ushr.ts.a[,MW])/sum(popReg[MW])
e.hr.a=matrix(1,nrow=length(dates),ncol=dim(ushr.ts.a[,MW])[2])
r.hr.d=rowSums(ushr.ts.d[,MW])/sum(popReg[MW])
e.hr.d=matrix(1,nrow=length(dates),ncol=dim(ushr.ts.d[,MW])[2])
for (i in 1:length(dates)){
  e.hr.c[i,]=r.hr.c[i]*popReg[MW]
  e.hr.n[i,]=r.hr.n[i]*popReg[MW]
  e.hr.a[i,]=r.hr.a[i]*popReg[MW]
  e.hr.d[i,]=r.hr.d[i]*popReg[MW]
}
d=data.frame(y=unmatrix(ushr.ts.c[,MW]),
             id=rep(1:length(regionsMW),length(dates)),
             id2=rep(1:length(dates),each=length(MW)),
             id3=1:length(unmatrix(ushr.ts.a[,MW])),
             x1=rep(intpassRegBin[MW],length(dates)),
             x2=rep(wkdysBIN,each=length(MW)),
             #     x2=rep(noncitzReg[MW],length(dates)),
             x3=stayReg[stayKeep],x4=rep(unemplyReg[MW],length(dates)),
             x5=rep(smokeReg[MW],length(dates)),
             x6=rep(ageGE65Reg[MW],length(dates)),
             x7=rep(AApopReg[MW],length(dates)))
resMW.c=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.c),
             control.compute = list(dic=TRUE,waic=TRUE))
d=data.frame(y=unmatrix(ushr.ts.n[,MW]),
             id=rep(1:length(regionsMW),length(dates)),
             id2=rep(1:length(dates),each=length(MW)),
             id3=1:length(unmatrix(ushr.ts.a[,MW])),
             x1=rep(intpassRegBin[MW],length(dates)),
             x2=rep(wkdysBIN,each=length(MW)),
             #     x2=rep(noncitzReg[MW],length(dates)),
             x3=stayReg[stayKeep],x4=rep(unemplyReg[MW],length(dates)),
             x5=rep(smokeReg[MW],length(dates)),
             x6=rep(ageGE65Reg[MW],length(dates)),
             x7=rep(AApopReg[MW],length(dates)))
resMW.n=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.n),
             control.compute = list(dic=TRUE,waic=TRUE))
d=data.frame(y=unmatrix(ushr.ts.a[,MW]),
             id=rep(1:length(regionsMW),length(dates)),
             id2=rep(1:length(dates),each=length(MW)),
             id3=1:length(unmatrix(ushr.ts.a[,MW])),
             x1=rep(intpassRegBin[MW],length(dates)),
             #     x2=rep(noncitzReg[MW],length(dates)),
             x2=rep(wkdysBIN,each=length(MW)),
             x3=stayReg[stayKeep],x4=rep(unemplyReg[MW],length(dates)),
             x5=rep(smokeReg[MW],length(dates)),
             x6=rep(ageGE65Reg[MW],length(dates)),
             x7=rep(AApopReg[MW],length(dates)))
resMW.a=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.a),
             control.compute = list(dic=TRUE,waic=TRUE))
d=data.frame(y=unmatrix(ushr.ts.d[,MW]),
             id=rep(1:length(regionsMW),length(dates)),
             id2=rep(1:length(dates),each=length(MW)),
             id3=1:length(unmatrix(ushr.ts.a[,MW])),
             x1=rep(intpassRegBin[MW],length(dates)),
             #     x2=rep(noncitzReg[MW],length(dates)),
             x2=rep(wkdysBIN,each=length(MW)),
             x3=stayReg[stayKeep],x4=rep(unemplyReg[MW],length(dates)),
             x5=rep(smokeReg[MW],length(dates)),
             x6=rep(ageGE65Reg[MW],length(dates)),
             x7=rep(AApopReg[MW],length(dates)))
resMW.d=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.d),
             control.compute = list(dic=TRUE,waic=TRUE))
#S
#AL, AR, DC, DE, FL, GA, KY, LA, MD, MS, NC, OK, SC, TN, TX, VA, WV
S=which(regSTfips==1|regSTfips==5|regSTfips==10|regSTfips==11|
          regSTfips==12|regSTfips==13|regSTfips==21|
          regSTfips==22|regSTfips==24|regSTfips==28|
          regSTfips==37|regSTfips==40|regSTfips==45|
          regSTfips==47|regSTfips==48|regSTfips==51|
          regSTfips==54)-1
regionsS=regions[S+1,]
stayKeep=rep(FALSE,389*length(dates))
for (i in 1:length(dates)){
  stayKeep[S+389*(i-1)]=TRUE
}
r.hr.c=rowSums(ushr.ts.c[,S])/sum(popReg[S])
e.hr.c=matrix(1,nrow=length(dates),ncol=dim(ushr.ts.c[,S])[2])
r.hr.n=rowSums(ushr.ts.n[,S])/sum(popReg[S])
e.hr.n=matrix(1,nrow=length(dates),ncol=dim(ushr.ts.n[,S])[2])
r.hr.a=rowSums(ushr.ts.a[,S])/sum(popReg[S])
e.hr.a=matrix(1,nrow=length(dates),ncol=dim(ushr.ts.a[,S])[2])
r.hr.d=rowSums(ushr.ts.d[,S])/sum(popReg[S])
e.hr.d=matrix(1,nrow=length(dates),ncol=dim(ushr.ts.d[,S])[2])
for (i in 1:length(dates)){
  e.hr.c[i,]=r.hr.c[i]*popReg[S]
  e.hr.n[i,]=r.hr.n[i]*popReg[S]
  e.hr.a[i,]=r.hr.a[i]*popReg[S]
  e.hr.d[i,]=r.hr.d[i]*popReg[S]
}
d=data.frame(y=unmatrix(ushr.ts.c[,S]),
             id=rep(1:length(regionsS),length(dates)),
             id2=rep(1:length(dates),each=length(S)),
             id3=1:length(unmatrix(ushr.ts.a[,S])),
             x1=rep(intpassRegBin[S],length(dates)),
             #     x2=rep(noncitzReg[S],length(dates)),
             x2=rep(wkdysBIN,each=length(S)),
             x3=stayReg[stayKeep],x4=rep(unemplyReg[S],length(dates)),
             x5=rep(smokeReg[S],length(dates)),
             x6=rep(ageGE65Reg[S],length(dates)),
             x7=rep(AApopReg[S],length(dates)))
resS.c=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.c),
             control.compute = list(dic=TRUE,waic=TRUE))
d=data.frame(y=unmatrix(ushr.ts.n[,S]),
             id=rep(1:length(regionsS),length(dates)),
             id2=rep(1:length(dates),each=length(S)),
             id3=1:length(unmatrix(ushr.ts.a[,S])),
             x1=rep(intpassRegBin[S],length(dates)),
             x2=rep(wkdysBIN,each=length(S)),
             #     x2=rep(noncitzReg[S],length(dates)),
             x3=stayReg[stayKeep],x4=rep(unemplyReg[S],length(dates)),
             x5=rep(smokeReg[S],length(dates)),
             x6=rep(ageGE65Reg[S],length(dates)),
             x7=rep(AApopReg[S],length(dates)))
resS.n=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.n),
             control.compute = list(dic=TRUE,waic=TRUE))
d=data.frame(y=unmatrix(ushr.ts.a[,S]),
             id=rep(1:length(regionsS),length(dates)),
             id2=rep(1:length(dates),each=length(S)),
             id3=1:length(unmatrix(ushr.ts.a[,S])),
             x1=rep(intpassRegBin[S],length(dates)),
             x2=rep(wkdysBIN,each=length(S)),
             #     x2=rep(noncitzReg[S],length(dates)),
             x3=stayReg[stayKeep],x4=rep(unemplyReg[S],length(dates)),
             x5=rep(smokeReg[S],length(dates)),
             x6=rep(ageGE65Reg[S],length(dates)),
             x7=rep(AApopReg[S],length(dates)))
resS.a=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.a),
             control.compute = list(dic=TRUE,waic=TRUE))
d=data.frame(y=unmatrix(ushr.ts.d[,S]),
             id=rep(1:length(regionsS),length(dates)),
             id2=rep(1:length(dates),each=length(S)),
             id3=1:length(unmatrix(ushr.ts.a[,S])),
             x1=rep(intpassRegBin[S],length(dates)),
             x2=rep(wkdysBIN,each=length(S)),
             #     x2=rep(noncitzReg[S],length(dates)),
             x3=stayReg[stayKeep],x4=rep(unemplyReg[S],length(dates)),
             x5=rep(smokeReg[S],length(dates)),
             x6=rep(ageGE65Reg[S],length(dates)),
             x7=rep(AApopReg[S],length(dates)))
resS.d=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.d),
             control.compute = list(dic=TRUE,waic=TRUE))
#W
#AK, AZ, CA, CO, HI, ID, MT, NM, NV, OR, UT, WA, WY
W=which(regSTfips==2|regSTfips==4|regSTfips==6|regSTfips==8|
          regSTfips==15|regSTfips==16|regSTfips==30|
          regSTfips==35|regSTfips==32|regSTfips==41|
          regSTfips==49|regSTfips==53|regSTfips==56)-1
regionsW=regions[W+1,]
stayKeep=rep(FALSE,389*length(dates))
for (i in 1:length(dates)){
  stayKeep[W+389*(i-1)]=TRUE
}
r.hr.c=rowSums(ushr.ts.c[,W])/sum(popReg[W])
e.hr.c=matrix(1,nrow=length(dates),ncol=dim(ushr.ts.c[,W])[2])
r.hr.n=rowSums(ushr.ts.n[,W])/sum(popReg[W])
e.hr.n=matrix(1,nrow=length(dates),ncol=dim(ushr.ts.n[,W])[2])
r.hr.a=rowSums(ushr.ts.a[,W])/sum(popReg[W])
e.hr.a=matrix(1,nrow=length(dates),ncol=dim(ushr.ts.a[,W])[2])
r.hr.d=rowSums(ushr.ts.d[,W])/sum(popReg[W])
e.hr.d=matrix(1,nrow=length(dates),ncol=dim(ushr.ts.d[,W])[2])
for (i in 1:length(dates)){
  e.hr.c[i,]=r.hr.c[i]*popReg[W]
  e.hr.n[i,]=r.hr.n[i]*popReg[W]
  e.hr.a[i,]=r.hr.a[i]*popReg[W]
  e.hr.d[i,]=r.hr.d[i]*popReg[W]
}
d=data.frame(y=unmatrix(ushr.ts.c[,W]),
             id=rep(1:length(regionsW),length(dates)),
             id2=rep(1:length(dates),each=length(W)),
             id3=1:length(unmatrix(ushr.ts.a[,W])),
             x1=rep(intpassRegBin[W],length(dates)),
             x2=rep(wkdysBIN,each=length(W)),
             #     x2=rep(noncitzReg[W],length(dates)),
             x3=stayReg[stayKeep],x4=rep(unemplyReg[W],length(dates)),
             x5=rep(smokeReg[W],length(dates)),
             x6=rep(ageGE65Reg[W],length(dates)),
             x7=rep(AApopReg[W],length(dates)))
resW.c=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.c),
             control.compute = list(dic=TRUE,waic=TRUE))
d=data.frame(y=unmatrix(ushr.ts.n[,W]),
             id=rep(1:length(regionsW),length(dates)),
             id2=rep(1:length(dates),each=length(W)),
             id3=1:length(unmatrix(ushr.ts.a[,W])),
             x1=rep(intpassRegBin[W],length(dates)),
             x2=rep(wkdysBIN,each=length(W)),
             #     x2=rep(noncitzReg[W],length(dates)),
             x3=stayReg[stayKeep],x4=rep(unemplyReg[W],length(dates)),
             x5=rep(smokeReg[W],length(dates)),
             x6=rep(ageGE65Reg[W],length(dates)),
             x7=rep(AApopReg[W],length(dates)))
resW.n=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.n),
             control.compute = list(dic=TRUE,waic=TRUE))
d=data.frame(y=unmatrix(ushr.ts.a[,W]),
             id=rep(1:length(regionsW),length(dates)),
             id2=rep(1:length(dates),each=length(W)),
             id3=1:length(unmatrix(ushr.ts.a[,W])),
             x1=rep(intpassRegBin[W],length(dates)),
             x2=rep(wkdysBIN,each=length(W)),
             #     x2=rep(noncitzReg[W],length(dates)),
             x3=stayReg[stayKeep],x4=rep(unemplyReg[W],length(dates)),
             x5=rep(smokeReg[W],length(dates)),
             x6=rep(ageGE65Reg[W],length(dates)),
             x7=rep(AApopReg[W],length(dates)))
resW.a=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.a),
             control.compute = list(dic=TRUE,waic=TRUE))
d=data.frame(y=unmatrix(ushr.ts.d[,W]),
             id=rep(1:length(regionsW),length(dates)),
             id2=rep(1:length(dates),each=length(W)),
             id3=1:length(unmatrix(ushr.ts.a[,W])),
             x1=rep(intpassRegBin[W],length(dates)),
             x2=rep(wkdysBIN,each=length(W)),
             #     x2=rep(noncitzReg[W],length(dates)),
             x3=stayReg[stayKeep],x4=rep(unemplyReg[W],length(dates)),
             x5=rep(smokeReg[W],length(dates)),
             x6=rep(ageGE65Reg[W],length(dates)),
             x7=rep(AApopReg[W],length(dates)))
resW.d=inla(f1,data=d,family="poisson",E=unmatrix(e.hr.d),
             control.compute = list(dic=TRUE,waic=TRUE))

write.csv(exp(resUS.c$summary.fixed[-1,c(1,3,5)]),"data/resUSc.csv")
write.csv(exp(resUS.n$summary.fixed[-1,c(1,3,5)]),"data/resUSn.csv")
write.csv(exp(resUS.a$summary.fixed[-1,c(1,3,5)]),"data/resUSa.csv")
write.csv(exp(resUS.d$summary.fixed[-1,c(1,3,5)]),"data/resUSd.csv")
write.csv(exp(resNE.c$summary.fixed[-1,c(1,3,5)]),"data/resNEc.csv")
write.csv(exp(resNE.n$summary.fixed[-1,c(1,3,5)]),"data/resNEn.csv")
write.csv(exp(resNE.a$summary.fixed[-1,c(1,3,5)]),"data/resNEa.csv")
write.csv(exp(resNE.d$summary.fixed[-1,c(1,3,5)]),"data/resNEd.csv")
write.csv(exp(resMW.c$summary.fixed[-1,c(1,3,5)]),"data/resMWc.csv")
write.csv(exp(resMW.n$summary.fixed[-1,c(1,3,5)]),"data/resMWa.csv")
write.csv(exp(resMW.a$summary.fixed[-1,c(1,3,5)]),"data/resMWn.csv")
write.csv(exp(resMW.d$summary.fixed[-1,c(1,3,5)]),"data/resMWd.csv")
write.csv(exp(resW.c$summary.fixed[-1,c(1,3,5)]),"data/resWc.csv")
write.csv(exp(resW.n$summary.fixed[-1,c(1,3,5)]),"data/resWn.csv")
write.csv(exp(resW.a$summary.fixed[-1,c(1,3,5)]),"data/resWa.csv")
write.csv(exp(resW.d$summary.fixed[-1,c(1,3,5)]),"data/resWd.csv")
write.csv(exp(resS.c$summary.fixed[-1,c(1,3,5)]),"data/resSc.csv")
write.csv(exp(resS.n$summary.fixed[-1,c(1,3,5)]),"data/resSn.csv")
write.csv(exp(resS.a$summary.fixed[-1,c(1,3,5)]),"data/resSa.csv")
write.csv(exp(resS.d$summary.fixed[-1,c(1,3,5)]),"data/resSd.csv")
write.csv(resUS.c$summary.random$id[,2],"data/resUScSpt.csv")
write.csv(resUS.n$summary.random$id[,2],"data/resUSnSpt.csv")
write.csv(resUS.a$summary.random$id[,2],"data/resUSaSpt.csv")
write.csv(resUS.d$summary.random$id[,2],"data/resUSdSpt.csv")
write.csv(resNE.c$summary.random$id[,2],"data/resNEcSpt.csv")
write.csv(resNE.n$summary.random$id[,2],"data/resNEnSpt.csv")
write.csv(resNE.a$summary.random$id[,2],"data/resNEaSpt.csv")
write.csv(resNE.d$summary.random$id[,2],"data/resNEdSpt.csv")
write.csv(resMW.c$summary.random$id[,2],"data/resMWcSpt.csv")
write.csv(resMW.n$summary.random$id[,2],"data/resMWnSpt.csv")
write.csv(resMW.a$summary.random$id[,2],"data/resMWaSpt.csv")
write.csv(resMW.d$summary.random$id[,2],"data/resMWdSpt.csv")
write.csv(resW.c$summary.random$id[,2],"data/resWcSpt.csv")
write.csv(resW.n$summary.random$id[,2],"data/resWnSpt.csv")
write.csv(resW.a$summary.random$id[,2],"data/resWaSpt.csv")
write.csv(resW.d$summary.random$id[,2],"data/resWdSpt.csv")
write.csv(resS.c$summary.random$id[,2],"data/resScSpt.csv")
write.csv(resS.n$summary.random$id[,2],"data/resSnSpt.csv")
write.csv(resS.a$summary.random$id[,2],"data/resSaSpt.csv")
write.csv(resS.d$summary.random$id[,2],"data/resSdSpt.csv")
write.csv(resUS.c$summary.random$id2[,2],"data/resUScTemp.csv")
write.csv(resUS.n$summary.random$id2[,2],"data/resUSnTemp.csv")
write.csv(resUS.a$summary.random$id2[,2],"data/resUSaTemp.csv")
write.csv(resUS.d$summary.random$id2[,2],"data/resUSdTemp.csv")
write.csv(resNE.c$summary.random$id2[,2],"data/resNEcTemp.csv")
write.csv(resNE.n$summary.random$id2[,2],"data/resNEnTemp.csv")
write.csv(resNE.a$summary.random$id2[,2],"data/resNEaTemp.csv")
write.csv(resNE.d$summary.random$id2[,2],"data/resNEdTemp.csv")
write.csv(resMW.c$summary.random$id2[,2],"data/resMWcTemp.csv")
write.csv(resMW.n$summary.random$id2[,2],"data/resMWnTemp.csv")
write.csv(resMW.a$summary.random$id2[,2],"data/resMWaTemp.csv")
write.csv(resMW.d$summary.random$id2[,2],"data/resMWdTemp.csv")
write.csv(resW.c$summary.random$id2[,2],"data/resWcTemp.csv")
write.csv(resW.n$summary.random$id2[,2],"data/resWnTemp.csv")
write.csv(resW.a$summary.random$id2[,2],"data/resWaTemp.csv")
write.csv(resW.d$summary.random$id2[,2],"data/resWdTemp.csv")
write.csv(resS.c$summary.random$id2[,2],"data/resScTemp.csv")
write.csv(resS.n$summary.random$id2[,2],"data/resSnTemp.csv")
write.csv(resS.a$summary.random$id2[,2],"data/resSaTemp.csv")
write.csv(resS.d$summary.random$id2[,2],"data/resSdTemp.csv")
# 
# #for teams
# write.csv(data.frame(resUScSpt=resUS.c$summary.random$id[,2],
#                      resUSnSpt=resUS.n$summary.random$id[,2],
#                      resUSaSpt=resUS.a$summary.random$id[,2],
#                      resUSdSpt=resUS.d$summary.random$id[,2]),"C:/Users/carrollr/OneDrive - UNC-Wilmington/COVID19/SptRes2/resUSspt.csv")
# write.csv(data.frame(resNEcSpt=resNE.c$summary.random$id[,2],
#                      resNEnSpt=resNE.n$summary.random$id[,2],
#                      resNEaSpt=resNE.a$summary.random$id[,2],
#                      resNEdSpt=resNE.d$summary.random$id[,2]),"C:/Users/carrollr/OneDrive - UNC-Wilmington/COVID19/SptRes2/resNEspt.csv")
# write.csv(data.frame(resMWcSpt=resMW.c$summary.random$id[,2],
#                      resMWnSpt=resMW.n$summary.random$id[,2],
#                      resMWaSpt=resMW.a$summary.random$id[,2],
#                      resMWdSpt=resMW.d$summary.random$id[,2]),"C:/Users/carrollr/OneDrive - UNC-Wilmington/COVID19/SptRes2/resMWspt.csv")
# write.csv(data.frame(resScSpt=resS.c$summary.random$id[,2],
#                      resSnSpt=resS.n$summary.random$id[,2],
#                      resSaSpt=resS.a$summary.random$id[,2],
#                      resSdSpt=resS.d$summary.random$id[,2]),"C:/Users/carrollr/OneDrive - UNC-Wilmington/COVID19/SptRes2/resSspt.csv")
# write.csv(data.frame(resWcSpt=resW.c$summary.random$id[,2],
#                      resWnSpt=resW.n$summary.random$id[,2],
#                      resWaSpt=resW.a$summary.random$id[,2],
#                      resWdSpt=resW.d$summary.random$id[,2]),"C:/Users/carrollr/OneDrive - UNC-Wilmington/COVID19/SptRes2/resWspt.csv")


Sys.time()-t


