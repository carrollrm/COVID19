#libraries
library(shiny)
library(rgdal)
library(gdata)
library(fillmap)
library(viridis)
library(maptools)


###############maps################
UScnty <- readOGR("data/tl_2017_us_county2.shp")#local
NCcnty <- readOGR(system.file("shapes/sids.shp", package="maptools")[1])
NCcnty=NCcnty[order(NCcnty$FIPS),]
NCcnty.sub=NCcnty[c(10,24,65,67,71),]

##############JHU and other data set up#############
UScntyPop=read.csv("https://raw.githubusercontent.com/carrollrm/UScntyPop/master/UScntyPop.csv", header=T)

dates=format(seq(as.Date("03-23-2020",format="%m-%d-%Y"),
                 as.Date(Sys.time(),tz="US/Eastern")-1, 
                 by="days"),format="%m-%d-%Y")
us.ts.c=us.ts.d=us.ts.r=us.ts.n=us.ts.n.pop=
  us.ts.c.pop=us.ts.d.pop=us.ts.r.pop=matrix(0,nrow=length(dates),ncol=3141)
nc.ts.c=nc.ts.d=nc.ts.r=nc.ts.n=nc.ts.n.pop=
  nc.ts.c.pop=nc.ts.d.pop=nc.ts.r.pop=matrix(0,nrow=length(dates),ncol=100)
for (i in 1:length(dates)){
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
  nmnc=nmus[which(nmus$FIPS<38000&nmus$FIPS>37000),]
  us.ts.c[i,]=(nmus$Confirmed)
  if (i==1){
    us.ts.n[i,]=us.ts.c[i,]
  } else {
    us.ts.n[i,]=us.ts.c[i,]-us.ts.c[i-1,]
  }
  us.ts.d[i,]=(nmus$Deaths)
  us.ts.r[i,]=(nmus$Recovered)
  nc.ts.c[i,]=(nmnc$Confirmed)
  if (i==1){
    nc.ts.n[i,]=nc.ts.c[i,]
  } else {
    nc.ts.n[i,]=nc.ts.c[i,]-nc.ts.c[i-1,]
  }
  nc.ts.d[i,]=(nmnc$Deaths)
  nc.ts.r[i,]=(nmnc$Recovered)
  us.ts.c.pop[i,]=(nmus$Confirmed)/nmus$pop*100
  if (i==1){
    us.ts.n.pop[i,]=us.ts.c.pop[i,]
  } else {
    us.ts.n.pop[i,]=us.ts.c.pop[i,]-us.ts.c.pop[i-1,]
  }
  us.ts.d.pop[i,]=(nmus$Deaths)/nmus$pop*100
  us.ts.r.pop[i,]=(nmus$Recovered)/nmus$pop*100
  nc.ts.c.pop[i,]=(nmnc$Confirmed)/nmnc$pop*100
  if (i==1){
    nc.ts.n.pop[i,]=nc.ts.c.pop[i,]
  } else {
    nc.ts.n.pop[i,]=nc.ts.c.pop[i,]-nc.ts.c.pop[i-1,]
  }
  nc.ts.d.pop[i,]=(nmnc$Deaths)/nmnc$pop*100
  nc.ts.r.pop[i,]=(nmnc$Recovered)/nmnc$pop*100

  if (i!=length(dates)){  
  nmus.old=nmus[,-13]
  nmnc.old=nmnc[,-13]
  }
}


################main panel##############

shinyServer(function(input,output){
  output$text <- renderText({
    paste("You have selected to display ",
          ifelse(input$data=="Confirmed Cases",
                 "the number of confirmed cases ",
                 ifelse(input$data=="Active Cases","the number of active cases ",
                        ifelse(input$data=="Deaths","the number of deaths ","the number of new cases "))),
          ifelse(input$loc=="Country"&input$adj==F,"(in 10000s) ",""),
          ifelse(input$adj==T,"as a percent of the population ",""),
          "for ",
          ifelse(input$loc=="State",
                 "the entire state of North Carolina ",
                 ifelse(input$loc=="Country","the entire U.S. ",
                 "the Cape Fear region ")),
          "on ",dates[input$day],".",
          ifelse(input$loc=="Country"&input$adj==F,
                 " Note that the shading scale is not linear for these figures.",""),
          sep="")
  })

#map  
output$map <- renderPlot({
  if(input$loc=="State"){
    if(input$adj==T){
      if (input$data=="Confirmed Cases"){
      data <- nc.ts.c.pop
      yscl <- seq(0,max(data),.001)
      } else if (input$data=="Deaths"){
        data <- nc.ts.d.pop
        yscl <- seq(0,max(data),.0001)
      } else if (input$data=="New Cases"){
        data <- nc.ts.n.pop
        yscl <- seq(0,max(data),.0001)
      } else {
        data <- nc.ts.c.pop-nc.ts.d.pop-nc.ts.r.pop
        yscl <- seq(0,max(data),.001)
      }} else {
      if (input$data=="Active Cases") {
      data <- nc.ts.c-nc.ts.d-nc.ts.r
      yscl <- 0:max(data)
      } else if (input$data=="Deaths"){
        data <- nc.ts.d
        yscl <- 0:max(data)
      } else if (input$data=="New Cases"){
        data <- nc.ts.n
        yscl <- 0:max(data)
      } else {
        data <- nc.ts.c
        yscl <- 0:max(data)
      }
      }
    
    fillmap2(NCcnty,"",data[input$day,],map.lty = 0,y.scl=yscl,
             main.cex = 1,leg.cex = 1, main.line=-3,leg.rnd=2,
             leg.loc="below")

  } else if(input$loc=="Country"){
    if(input$adj==T){
      if (input$data=="Confirmed Cases"){
      data <- us.ts.c.pop
      yscl <- seq(0,max(data),.001)
      } else if (input$data=="Deaths"){
        data <- us.ts.d.pop
        yscl <- seq(0,max(data),.0001)
      } else if (input$data=="New Cases"){
        data <- us.ts.n.pop
        yscl <- seq(0,max(data),.0001)
      } else {
        data <- us.ts.c.pop-us.ts.d.pop-us.ts.r.pop
        yscl <- seq(0,max(data),.001)
      }
    } else {
    if (input$data=="Deaths"){
      data <- (us.ts.d)
      yscl <- unmatrix(data)
    } else if (input$data=="Active Cases") {
      data <- (us.ts.c-us.ts.d-us.ts.r)/10000
      yscl <- unmatrix(data)
    } else if (input$data=="New Cases") {
      data <- (us.ts.n)
      yscl <- unmatrix(data)
    } else {
      data <- us.ts.c/10000
      yscl <- unmatrix(data)
    }
    }
    
    fillmap2(UScnty,"",data[input$day,],map.lty = 0,y.scl=yscl,
             main.cex = 1,leg.cex = 1, main.line=-3,leg.rnd=2,
             leg.loc="below")
    
  } else {
    if(input$adj==T){
      if (input$data=="Confirmed Cases"){
      data <- nc.ts.c.pop[,c(10,24,65,67,71)]
      yscl <- seq(0,max(data),.001)
      } else if (input$data=="Deaths"){
        data <- nc.ts.d.pop[,c(10,24,65,67,71)]
        yscl <- seq(0,max(data),.0001)
      } else if (input$data=="New Cases"){
        data <- nc.ts.n.pop[,c(10,24,65,67,71)]
        yscl <- seq(0,max(data),.0001)
      } else {
        data <- nc.ts.c.pop[,c(10,24,65,67,71)]-nc.ts.d.pop[,c(10,24,65,67,71)]-nc.ts.r.pop[,c(10,24,65,67,71)]
        yscl <- seq(0,max(data),.001)
      }} else {
      if (input$data=="Active Cases") {
      data <- nc.ts.c[,c(10,24,65,67,71)]-nc.ts.d[,c(10,24,65,67,71)]-nc.ts.r[,c(10,24,65,67,71)]
      yscl <- 0:max(data)
      } else if (input$data=="Deaths"){
        data <- nc.ts.d[,c(10,24,65,67,71)]
        yscl <- 0:max(data)
      } else if (input$data=="New Cases"){
        data <- nc.ts.n[,c(10,24,65,67,71)]
        yscl <- 0:max(data)
      } else {
      data <- nc.ts.c[,c(10,24,65,67,71)]
      yscl <- 0:max(data)
      }}
    fillmap2(NCcnty.sub,"",data[input$day,],map.lty = 0,y.scl=yscl,
           main.cex = 1,leg.cex = 1, main.line=-3,leg.rnd=2,
           leg.loc="below")
  }
 })

#barplot
  output$barplot <- renderPlot({
    if (input$loc=="State"){
       if(input$adj==T){
         if (input$data=="Confirmed Cases"){
         data <- nc.ts.c.pop
         sclr <- seq(0,max(data),.001)
         cols <- viridis(length(sclr), direction = -1)
         shading = cols[round(sort(data[input$day,],decreasing=T)[1:10],3)*1000]
         } else if (input$data=="Deaths") {
           data <- nc.ts.d.pop
           sclr <- seq(0,max(data),.0001)
           cols <- viridis(length(sclr), direction = -1)
           shading = cols[round(sort(data[input$day,],decreasing=T)[1:10],4)*10000]
         } else if (input$data=="New Cases") {
           data <- nc.ts.n.pop
           sclr <- seq(0,max(data),.0001)
           cols <- viridis(length(sclr), direction = -1)
           shading = cols[round(sort(data[input$day,],decreasing=T)[1:10],4)*10000]
         } else{
           data <- nc.ts.c.pop-nc.ts.d.pop-nc.ts.r.pop
           sclr <- seq(0,max(data),.001)
           cols <- viridis(length(sclr), direction = -1)
           shading = cols[round(sort(data[input$day,],decreasing=T)[1:10],3)*1000]
         }
         } else {
        if (input$data=="Active Cases") {
        data <- nc.ts.c-nc.ts.d-nc.ts.r
        sclr <- 0:max(data)
        cols <- viridis(length(sclr), direction = -1)
        shading = cols[sort(data[input$day,],decreasing=T)[1:10]]
      } else if (input$data=="Deaths") {
        data <- nc.ts.d
        sclr <- 0:max(data)
        cols <- viridis(length(sclr), direction = -1)
        shading = cols[sort(data[input$day,],decreasing=T)[1:10]]
      } else if (input$data=="New Cases") {
        data <- nc.ts.n
        sclr <- 0:max(data)
        cols <- viridis(length(sclr), direction = -1)
        shading = cols[sort(data[input$day,],decreasing=T)[1:10]]
      } else {
         data <- nc.ts.c
         sclr <- 0:max(data)
         cols <- viridis(length(sclr), direction = -1)
         shading = cols[sort(data[input$day,],decreasing=T)[1:10]]
       }}
       
       par(mar=c(6.5,3,3,1))
       barplot(sort(data[input$day,],decreasing=T)[1:10],
                names.arg=NCcnty$NAME[order(data[input$day,],decreasing=T)][1:10],
                ylim=c(0,max(data)),las=2,col=shading)
       } 
    else if (input$loc=="Country"){
      if(input$adj==T){
        if (input$data=="Confirmed Cases"){
        data <- us.ts.c.pop
        sclr <- seq(0,max(data),.001)
        cols <- viridis(length(sclr), direction = -1)
        shading = cols[round(sort(data[input$day,],decreasing=T)[1:10],3)*1000]
        } else if (input$data=="Deaths"){
          data <- us.ts.d.pop
          sclr <- seq(0,max(data),.0001)
          cols <- viridis(length(sclr), direction = -1)
          shading = cols[round(sort(data[input$day,],decreasing=T)[1:10],4)*10000]
        } else if (input$data=="New Cases"){
          data <- us.ts.n.pop
          sclr <- seq(0,max(data),.0001)
          cols <- viridis(length(sclr), direction = -1)
          shading = cols[round(sort(data[input$day,],decreasing=T)[1:10],4)*10000]
        } else {
          data <- us.ts.c.pop-us.ts.d.pop-us.ts.r.pop
          sclr <- seq(0,max(data),.001)
          cols <- viridis(length(sclr), direction = -1)
          shading = cols[round(sort(data[input$day,],decreasing=T)[1:10],3)*1000]
        }
        } else {
        if (input$data=="Active Cases") {
        data <- (us.ts.c-us.ts.d-us.ts.r)/10000
        sclr <- sort(unmatrix(data))
        cols <- viridis(length(sclr), direction = -1)
        data10 <- sort(data[input$day,],decreasing=T)[1:10]
        shading=rep(0,10)
        for (i in 1:10){shading[i] = cols[which(sclr==data10[i])[1]]}
        } else if (input$data=="Deaths"){
          data <- us.ts.d/10000
          sclr <- sort(unmatrix(data))
          cols <- viridis(length(sclr), direction = -1)
          data10 <- sort(data[input$day,],decreasing=T)[1:10]
          shading=rep(0,10)
          for (i in 1:10){shading[i] = cols[which(sclr==data10[i])[1]]}
        } else if (input$data=="New Cases"){
          data <- us.ts.n/10000
          sclr <- sort(unmatrix(data))
          cols <- viridis(length(sclr), direction = -1)
          data10 <- sort(data[input$day,],decreasing=T)[1:10]
          shading=rep(0,10)
          for (i in 1:10){shading[i] = cols[which(sclr==data10[i])[1]]}
        } else {
          data <- us.ts.c/10000
          sclr <- sort(unmatrix(data))
          cols <- viridis(length(sclr), direction = -1)
          data10 <- sort(data[input$day,],decreasing=T)[1:10]
          shading=rep(0,10)
          for (i in 1:10){shading[i] = cols[which(sclr==data10[i])[1]]}
        }}
      
      par(mar=c(15,3,3,1))
      barplot(sort(data[input$day,],decreasing=T)[1:10],
              names.arg=UScntyPop$Geography[order(data[input$day,],decreasing=T)][1:10],
              ylim=c(0,max(data)),las=2,col=shading)
    } else {
      if(input$adj==T){
        if (input$data=="Confirmed Cases"){
        data <- nc.ts.c.pop[,c(10,24,65,67,71)]
        sclr <- seq(0,max(data),.001)
        cols <- viridis(length(sclr), direction = -1)
        shading = cols[round(sort(data[input$day,],decreasing=T),3)*1000]
        } else if (input$data=="Deaths"){
          data <- nc.ts.d.pop[,c(10,24,65,67,71)]
          sclr <- seq(0,max(data),.0001)
          cols <- viridis(length(sclr), direction = -1)
          shd=round(sort(data[input$day,],decreasing=T),4)*10000
          shading = cols[ifelse(shd==0,1,shd)]
        } else if (input$data=="New Cases"){
          data <- nc.ts.n.pop[,c(10,24,65,67,71)]
          sclr <- seq(0,max(data),.0001)
          cols <- viridis(length(sclr), direction = -1)
          shd=round(sort(data[input$day,],decreasing=T),4)*10000
          shading = cols[ifelse(shd==0,1,shd)]
        } else {
          data <- nc.ts.c.pop[,c(10,24,65,67,71)]-nc.ts.d.pop[,c(10,24,65,67,71)]-nc.ts.r.pop[,c(10,24,65,67,71)]
          sclr <- seq(0,max(data),.001)
          cols <- viridis(length(sclr), direction = -1)
          shading = cols[round(sort(data[input$day,],decreasing=T),3)*1000]
        }
        } else {
        if (input$data=="Active Cases") {
        data <- nc.ts.c[,c(10,24,65,67,71)]-nc.ts.d[,c(10,24,65,67,71)]-nc.ts.r[,c(10,24,65,67,71)]
        sclr <- 0:max(data)
        cols <- viridis(length(sclr), direction = -1)
        shading = cols[sort(data[input$day,],decreasing=T)]
        } else if (input$data=="Deaths"){
          data <- nc.ts.d[,c(10,24,65,67,71)]
          sclr <- 0:max(data)
          cols <- viridis(length(sclr), direction = -1)
          shading = cols[sort(data[input$day,],decreasing=T)+1]
        } else if (input$data=="New Cases"){
          data <- nc.ts.n[,c(10,24,65,67,71)]
          sclr <- 0:max(data)
          cols <- viridis(length(sclr), direction = -1)
          shading = cols[sort(data[input$day,],decreasing=T)+1]
        } else {
        data <- nc.ts.c[,c(10,24,65,67,71)]
        sclr <- 0:max(data)
        cols <- viridis(length(sclr), direction = -1)
        shading = cols[sort(data[input$day,],decreasing=T)]
      }}
      
      par(mar=c(6.5,3,3,1))
      barplot(sort(data[input$day,],decreasing=T),
              names.arg=NCcnty.sub$NAME[order(data[input$day,],decreasing=T)],
              ylim=c(0,max(data)),las=2,col=shading)
    }
  })
  })
