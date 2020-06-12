#libraries
library(shiny)
library(rgdal)
library(fillmap)
library(viridis)

###############maps and results################
regions <- readOGR("data/USregions.shp")#local
regions <- regions[-1,]
regSTfips=as.numeric(as.character(regions$STATEFP))
NE=which(regSTfips==9|regSTfips==23|regSTfips==25|regSTfips==34|
           regSTfips==33|regSTfips==36|regSTfips==42|
           regSTfips==44|regSTfips==50)
regionsNE=regions[NE,]
MW=which(regSTfips==19|regSTfips==17|regSTfips==18|regSTfips==20|
           regSTfips==27|regSTfips==29|regSTfips==26|
           regSTfips==38|regSTfips==31|regSTfips==39|
           regSTfips==46|regSTfips==55)
regionsMW=regions[MW,]
S=which(regSTfips==1|regSTfips==5|regSTfips==10|regSTfips==11|
          regSTfips==12|regSTfips==13|regSTfips==21|
          regSTfips==22|regSTfips==24|regSTfips==28|
          regSTfips==37|regSTfips==40|regSTfips==45|
          regSTfips==47|regSTfips==48|regSTfips==51|
          regSTfips==54)
regionsS=regions[S,]
W=which(regSTfips==2|regSTfips==4|regSTfips==6|regSTfips==8|
          regSTfips==15|regSTfips==16|regSTfips==30|
          regSTfips==35|regSTfips==32|regSTfips==41|
          regSTfips==49|regSTfips==53|regSTfips==56)
regionsW=regions[W,]


resUSc=read.csv("data/resUSc.csv")
resUSn=read.csv("data/resUSn.csv")
resUSa=read.csv("data/resUSa.csv")
resUSd=read.csv("data/resUSd.csv")
resNEc=read.csv("data/resNEc.csv")
resNEn=read.csv("data/resNEn.csv")
resNEa=read.csv("data/resNEa.csv")
resNEd=read.csv("data/resNEd.csv")
resMWc=read.csv("data/resMWc.csv")
resMWn=read.csv("data/resMWn.csv")
resMWa=read.csv("data/resMWa.csv")
resMWd=read.csv("data/resMWd.csv")
resWc=read.csv("data/resWc.csv")
resWn=read.csv("data/resWn.csv")
resWa=read.csv("data/resWa.csv")
resWd=read.csv("data/resWd.csv")
resSc=read.csv("data/resSc.csv")
resSn=read.csv("data/resSn.csv")
resSa=read.csv("data/resSa.csv")
resSd=read.csv("data/resSd.csv")
resUScSpt=read.csv("data/resUScSpt.csv")
resUSnSpt=read.csv("data/resUSnSpt.csv")
resUSaSpt=read.csv("data/resUSaSpt.csv")
resUSdSpt=read.csv("data/resUSdSpt.csv")
resNEcSpt=read.csv("data/resNEcSpt.csv")
resNEnSpt=read.csv("data/resNEnSpt.csv")
resNEaSpt=read.csv("data/resNEaSpt.csv")
resNEdSpt=read.csv("data/resNEdSpt.csv")
resMWcSpt=read.csv("data/resMWcSpt.csv")
resMWnSpt=read.csv("data/resMWnSpt.csv")
resMWaSpt=read.csv("data/resMWaSpt.csv")
resMWdSpt=read.csv("data/resMWdSpt.csv")
resWcSpt=read.csv("data/resWcSpt.csv")
resWnSpt=read.csv("data/resWnSpt.csv")
resWaSpt=read.csv("data/resWaSpt.csv")
resWdSpt=read.csv("data/resWdSpt.csv")
resScSpt=read.csv("data/resScSpt.csv")
resSnSpt=read.csv("data/resSnSpt.csv")
resSaSpt=read.csv("data/resSaSpt.csv")
resSdSpt=read.csv("data/resSdSpt.csv")
resUScTemp=read.csv("data/resUScTemp.csv")
resUSnTemp=read.csv("data/resUSnTemp.csv")
resUSaTemp=read.csv("data/resUSaTemp.csv")
resUSdTemp=read.csv("data/resUSdTemp.csv")
resNEcTemp=read.csv("data/resNEcTemp.csv")
resNEnTemp=read.csv("data/resNEnTemp.csv")
resNEaTemp=read.csv("data/resNEaTemp.csv")
resNEdTemp=read.csv("data/resNEdTemp.csv")
resMWcTemp=read.csv("data/resMWcTemp.csv")
resMWnTemp=read.csv("data/resMWnTemp.csv")
resMWaTemp=read.csv("data/resMWaTemp.csv")
resMWdTemp=read.csv("data/resMWdTemp.csv")
resWcTemp=read.csv("data/resWcTemp.csv")
resWnTemp=read.csv("data/resWnTemp.csv")
resWaTemp=read.csv("data/resWaTemp.csv")
resWdTemp=read.csv("data/resWdTemp.csv")
resScTemp=read.csv("data/resScTemp.csv")
resSnTemp=read.csv("data/resSnTemp.csv")
resSaTemp=read.csv("data/resSaTemp.csv")
resSdTemp=read.csv("data/resSdTemp.csv")
dates=format(seq(as.Date("03-23-2020",format="%m-%d-%Y"),
                 as.Date(Sys.time(),tz="US/Eastern")-1, 
                 by="days"),format="%m-%d-%Y")[1:length(resSdTemp[,2])]
regpreds=read.csv("data/regpreds.csv")
health=read.csv("data/UScountyOrderPR1.csv")
stay=read.csv("data/StayHome.csv")[,-(3:6)]
stayReg=rep(NA,389*length(dates))
for (i in 2:390){
  for (j in 1:length(dates))
    stayReg[(i-1)+389*(j-1)]=stay[which(stay$FIPS==health$STATEFP[which(health$orderReg==i)[1]]),j+2]
}



################main panel##############
shinyServer(function(input,output){
  output$text <- renderText({
    paste(paste0("These results were last updated with the data from the release on ",
                 max(dates),". The results were generated via a Bayesian spatio-temporal 
                 Poisson model for disease mapping. The temporal unit considered is days, 
                 and the spatial unit is health region. This is a lesser known spatial 
                 region that comprises one or more counties and was used to reduce the 
                 dimension of the model as well as reduce the number of zeros in the outcomes. 
                 The models take over an hour to run, and run times will only get 
                 longer as more days are included, so this dashboard displays saved 
                 estimates from those model runs."))
  })
 # if (input$pred==F){
    output$text1 <- renderText({
    paste("The table displays what we call fixed effect estimates. These are 
                 estimates related to differences in risk among the spatial regions considered
                 based on the predictors that are listed in the first column
                 (presence of an international airport in the region, a spatio-temporal 
                 indicator of state directed stay at home orders, the region's percent of 
                 unemployed individuals, and the region's percent of current smokers). 
                 When examining these estimates, the value 1.000 is an important cut off. 
                 Estimates greater than 1.000 indicate an increase in risk while estimates less than one 
                 indicate a decrease in risk, and these estimates are considered well 
                 estimated if 1.000 is not contained in the credible interval. The first two 
                 predictors are categorical, so the estimate suggests the change in 
                 risk for areas where the characteristic is present (so regions with airports 
                 that service international locations or regions/days where a stay at home order
                 was in effect) compared to the reference category (no international airport, 
no stay at home order in effect). The next three predictors are continuous measures of region 
                 percent presence of a characteristic in its population, so the estimates 
                 suggest the change in risk for a 1 percent increase in the given characteristic 
                 (percent of the population that is unemployed, smokes, or is age 65 and older).")
  })
  #fixed effects
    output$tab <- renderTable({
      if (input$data=="Confirmed Cases"){
        if(input$loc=="South"){
          mat=matrix(NA,nrow=dim(resSc[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resSc[1,-1],3))
          mat[4,]=as.numeric(round(resSc[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resSc[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           "State Issued Stay at Home - None",
                    #             "State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        } else if(input$loc=="West") {
          mat=matrix(NA,nrow=dim(resWc[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resWc[1,-1],3))
          mat[4,]=as.numeric(round(resWc[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resWc[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           "State Issued Stay at Home - None",
                                               "State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        }
        else if(input$loc=="Midwest") {
          mat=matrix(NA,nrow=dim(resMWc[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resMWc[1,-1],3))
          mat[4,]=as.numeric(round(resMWc[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resMWc[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           "State Issued Stay at Home - None",
                           #                     "State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        }
        else if(input$loc=="Northeast"){
          mat=matrix(NA,nrow=dim(resNEc[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resNEc[1,-1],3))
          mat[4,]=as.numeric(round(resNEc[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resNEc[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           #"State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        }
        else {
          mat=matrix(NA,nrow=dim(resUSc[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resUSc[1,-1],3))
          mat[4,]=as.numeric(round(resUSc[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resUSc[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           "State Issued Stay at Home - None",
                                         "State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        }
      } else if (input$data=="Deaths"){
        if(input$loc=="South"){
          mat=matrix(NA,nrow=dim(resSd[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resSd[1,-1],3))
          mat[4,]=as.numeric(round(resSd[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resSd[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           "State Issued Stay at Home - None",
             #                                   "State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        } else if(input$loc=="West") {
          mat=matrix(NA,nrow=dim(resWd[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resWd[1,-1],3))
          mat[4,]=as.numeric(round(resWd[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resWd[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           "State Issued Stay at Home - None",
                                          "State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        }
        else if(input$loc=="Midwest") {
          mat=matrix(NA,nrow=dim(resMWd[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resMWd[1,-1],3))
          mat[4,]=as.numeric(round(resMWd[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resMWd[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           "State Issued Stay at Home - None",
                           #                   "State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        }
        else if(input$loc=="Northeast"){
          mat=matrix(NA,nrow=dim(resNEd[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resNEd[1,-1],3))
          mat[4,]=as.numeric(round(resNEd[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resNEd[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           #                       "State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        }
        else {
          mat=matrix(NA,nrow=dim(resUSd[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resUSd[1,-1],3))
          mat[4,]=as.numeric(round(resUSd[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resUSd[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           "State Issued Stay at Home - None",
                                        "State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        }
      } else if (input$data=="New Cases"){
        if(input$loc=="South"){
          mat=matrix(NA,nrow=dim(resSn[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resSn[1,-1],3))
          mat[4,]=as.numeric(round(resSn[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resSn[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           "State Issued Stay at Home - None",
                    #                            "State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        } else if(input$loc=="West") {
          mat=matrix(NA,nrow=dim(resWn[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resWn[1,-1],3))
          mat[4,]=as.numeric(round(resWn[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resWn[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           "State Issued Stay at Home - None",
                                          "State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        }
        else if(input$loc=="Midwest") {
          mat=matrix(NA,nrow=dim(resMWn[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resMWn[1,-1],3))
          mat[4,]=as.numeric(round(resMWn[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resMWn[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           "State Issued Stay at Home - None",
                           #                     "State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        }
        else if(input$loc=="Northeast"){
          mat=matrix(NA,nrow=dim(resNEn[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resNEn[1,-1],3))
          mat[4,]=as.numeric(round(resNEn[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resNEn[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           #                    "State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        }
        else {
          mat=matrix(NA,nrow=dim(resUSn[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resUSn[1,-1],3))
          mat[4,]=as.numeric(round(resUSn[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resUSn[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           "State Issued Stay at Home - None",
                                  "State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        }
      } else {
        if(input$loc=="South"){
          mat=matrix(NA,nrow=dim(resSa[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resSa[1,-1],3))
          mat[4,]=as.numeric(round(resSa[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resSa[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           "State Issued Stay at Home - None",
                 #                               "State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        } else if(input$loc=="West") {
          mat=matrix(NA,nrow=dim(resWa[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resWa[1,-1],3))
          mat[4,]=as.numeric(round(resWa[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resWa[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           "State Issued Stay at Home - None",
                                   "State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        }
        else if(input$loc=="Midwest") {
          mat=matrix(NA,nrow=dim(resMWa[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resMWa[1,-1],3))
          mat[4,]=as.numeric(round(resMWa[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resMWa[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           "State Issued Stay at Home - None",
                           #                    "State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        }
        else if(input$loc=="Northeast"){
          mat=matrix(NA,nrow=dim(resNEa[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resNEa[1,-1],3))
          mat[4,]=as.numeric(round(resNEa[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resNEa[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           #                    "State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        }
        else {
          mat=matrix(NA,nrow=dim(resUSa[,-1])[1]+3,ncol=3)
          mat[2,]=as.numeric(round(resUSa[1,-1],3))
          mat[4,]=as.numeric(round(resUSa[2,-1],3))
          mat[6:nrow(mat),]=as.matrix(round(resUSa[-c(1,2),-1],3))
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("International Airport - No (Ref)",
                           "International Airport - Yes",
                           "Early Week (Ref)",
                           "Late Week",
                           "State Issued Stay at Home - Before (Ref)",
                           "State Issued Stay at Home - During Partial",
                           "State Issued Stay at Home - During Full",
                           "State Issued Stay at Home - None",
                           "State Issued Stay at Home - After",
                           "% Unemployed","% Smokers","% 65 and Older","% AA population")
          mat
        }
      }
    },rownames=T,colnames=T,digits=3)
  
  output$text2 <- renderText({
    paste("The next two plots display what we call random effect estimates. These are 
          estimates related to differences in risk among the spatial and temporal units 
          considered, after adjusting for the predictors in the the model. So, these are 
          often though of as residual (unexplained) risk. As in above, smaller estimates 
          suggest less risk (these are lighter areas in the map) and larger estimates 
          suggest more risk (these are darker areas in the map).")
  })

  #map  
output$map <- renderPlot({
  if(input$data=="Confirmed Cases"){
  if(input$loc=="South"){
    data <- resScSpt[,2]
    map <- regionsS
  } else if(input$loc=="West") {
    data <- resWcSpt[,2]
    map <- regionsW
  }
  else if(input$loc=="Midwest") {
    data <- resMWcSpt[,2]
    map <- regionsMW
  }
  else if(input$loc=="Northeast"){
    data <- resNEcSpt[,2]
    map <- regionsNE
  }
    else{
      data <- resUScSpt[,2]
      map <- regions
    }
  } else if (input$data=="Deaths"){
    if(input$loc=="South"){
      data <- resSdSpt[,2]
      map <- regionsS
    } else if(input$loc=="West") {
      data <- resWdSpt[,2]
      map <- regionsW
    }
    else if(input$loc=="Midwest") {
      data <- resMWdSpt[,2]
      map <- regionsMW
    }
    else if(input$loc=="Northeast"){
      data <- resNEdSpt[,2]
      map <- regionsNE
    }
    else{
      data <- resUSdSpt[,2]
      map <- regions
    }
  } else if (input$data=="New Cases"){
    if(input$loc=="South"){
      data <- resSnSpt[,2]
      map <- regionsS
    } else if(input$loc=="West") {
      data <- resWnSpt[,2]
      map <- regionsW
    }
    else if(input$loc=="Midwest") {
      data <- resMWnSpt[,2]
      map <- regionsMW
    }
    else if(input$loc=="Northeast"){
      data <- resNEnSpt[,2]
      map <- regionsNE
    }
    else{
      data <- resUSnSpt[,2]
      map <- regions
    }
  } else {
    if(input$loc=="South"){
      data <- resSaSpt[,2]
      map <- regionsS
    } else if(input$loc=="West") {
      data <- resWaSpt[,2]
      map <- regionsW
    }
    else if(input$loc=="Midwest") {
      data <- resMWaSpt[,2]
      map <- regionsMW
    }
    else if(input$loc=="Northeast"){
      data <- resNEaSpt[,2]
      map <- regionsNE
    }
    else{
      data <- resUSaSpt[,2]
      map <- regions
    }
  }   
    fillmap2(map,"Spatial Random Effect Estimate",exp(data),map.lty = 0,
             main.cex = 1,leg.cex = 1, leg.rnd=2,
         #    leg.loc="below")
             leg.loc="beside")

 })

#plot
  output$plot <- renderPlot({
    if(input$data=="Active Cases"){
    if(input$loc=="South"){
      data <- resSaTemp[,2]
    } else if(input$loc=="West") {
      data <- resWaTemp[,2]
    }
    else if(input$loc=="Midwest") {
      data <- resMWaTemp[,2]
    }
    else {
      data <- resNEaTemp[,2]
    }
    } else if (input$data=="New Cases"){
      if(input$loc=="South"){
        data <- resSnTemp[,2]
      } else if(input$loc=="West") {
        data <- resWnTemp[,2]
      }
      else if(input$loc=="Midwest") {
        data <- resMWnTemp[,2]
      }
      else {
        data <- resNEnTemp[,2]
      }
    } else if (input$data=="Deaths"){
      if(input$loc=="South"){
        data <- resSdTemp[,2]
      } else if(input$loc=="West") {
        data <- resWdTemp[,2]
      }
      else if(input$loc=="Midwest") {
        data <- resMWdTemp[,2]
      }
      else {
        data <- resNEdTemp[,2]
      }
    } else {
      if(input$loc=="South"){
        data <- resScTemp[,2]
      } else if(input$loc=="West") {
        data <- resWcTemp[,2]
      }
      else if(input$loc=="Midwest") {
        data <- resMWcTemp[,2]
      }
      else {
        data <- resNEcTemp[,2]
      }
    }
       par(mar=c(6.5,3,3,1))
       plot(1:length(dates),exp(data),type="l",xlab="Days beginning March 23, 2020",
            main="Temporal Random Effect Estimate")
  })
#   } else {
#     if(input$data=="Confirmed Cases"|input$data=="Active Cases"|
#        input$data=="New Cases"|input$data=="Deaths"|
#        input$loc=="South"|input$loc=="West"|input$loc=="Midwest"|
#        input$loc=="United States"|input$loc=="Northeast"){
#     output$text1 <- renderText({
#       paste("You have selected to display the predictors used in this regression model 
#             only. To view the outcome variables, please see the other dashboards linked 
#             in the side panel. To see the model results, please uncheck the View the 
#             regression predictors only box.")
#     })
#     output$text2 <- renderPlot({
#       par(mar=c(0,0,0,0))
#       fillmap(regions,"International Airport",regpreds$intpassRegBin,n.col=3,map.lty = 0,
#               main.cex = 1,legendtxt = NA,bk="c",cuts=c(-.5,0,.5,1))
#       shading <- gray(rev(0:(3 - 1)/(3 - 1)))
#       legend("topright",legend=c("None","At least one"),fill=shading[c(2,3)],
#              bty='n')
#     })
#     output$tab <- renderPlot({
#       par(mar=c(0,0,0,0))
#       fillmaps(regions,
#                c(paste("\n",dates[1]),
#                  paste("State Issued Stay at Home\n",dates[round(length(dates)/2)]),
#                  paste("\n",max(dates))),
#                cbind(stayReg[1:389],
#                      stayReg[1:389+389*(round(length(dates)/2)-1)],
#                      stayReg[1:389+389*(length(dates)-1)]),
#               n.col=4,map.lty = 0,leg.common=T,lay.hei = c(.9,.1),
#               lay.m=matrix(c(1,4,2,4,3,4),nrow=2,ncol=3),
#               main.cex = 1,legendtxt = NA,bk="c",cuts=c(-.5,0,.25,.75,1))
#       shading <- gray(rev(0:(4 - 1)/(4 - 1)))
#       legend("center",legend=c("No","Partial","Yes"),fill=shading[c(2,3,4)],
#              bty='n',horiz = T)
# })
#     output$map <- renderPlot({
#       fillmap2(regions,"% Unemployed",regpreds$unemplyReg,map.lty = 0,
#                main.cex = 1,leg.cex = 1, leg.rnd=2,
#                leg.loc="beside")
#     })
#     output$plot <- renderPlot({
#       fillmap2(regions,"% Smokers","% 65 and Older","% AA population"),regpreds$smokeReg,map.lty = 0,
#                main.cex = 1,leg.cex = 1, leg.rnd=2,
#                leg.loc="beside")
#       })
#}}
  })
