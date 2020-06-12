#libraries
library(shiny)


##############JHU and other data set up#############
dates=format(seq(as.Date("03-23-2020",format="%m-%d-%Y"),
                 as.Date(Sys.time(),tz="US/Eastern")-1, 
                 by="days"),format="%m-%d-%Y")
dates2=format(seq(as.Date("01-22-2020",format="%m-%d-%Y"),
                 as.Date(Sys.time(),tz="US/Eastern")-1, 
                 by="days"),format="%m-%d-%Y")
pops=c(66990000,#france
       82790000,#germany
       60480000,#italy
       46660000,#spain
       327200000)#US
nms=c("France","Germany","Italy","Spain","US")

COVID19.ts.c=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", header=T)
COVID19.ts.d=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", header=T)
COVID19.ts.r=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", header=T)

ts.c=ts.d=ts.r=
  ts.c.pop=ts.d.pop=ts.r.pop=matrix(0,nrow=length(dates2),ncol=5)

for (j in 1:5){
  ts.c[1:61,j]=colSums(COVID19.ts.c[which(COVID19.ts.c$Country.Region==nms[j]),5:65],na.rm=T)
  ts.d[1:61,j]=colSums(COVID19.ts.d[which(COVID19.ts.c$Country.Region==nms[j]),5:65],na.rm=T)
  ts.r[1:61,j]=colSums(COVID19.ts.r[which(COVID19.ts.c$Country.Region==nms[j]),5:65],na.rm=T)
  ts.c.pop[1:61,j]=colSums(COVID19.ts.c[which(COVID19.ts.c$Country.Region==nms[j]),5:65],na.rm=T)/pops[j]*100
  ts.d.pop[1:61,j]=colSums(COVID19.ts.d[which(COVID19.ts.c$Country.Region==nms[j]),5:65],na.rm=T)/pops[j]*100
  ts.r.pop[1:61,j]=colSums(COVID19.ts.r[which(COVID19.ts.c$Country.Region==nms[j]),5:65],na.rm=T)/pops[j]*100
}

for (i in 1:length(dates)){
  nm=read.csv(paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",dates[i],".csv",sep=""), header=T)
  for (j in 1:5){
    ts.c[i+61,j]=sum(nm$Confirmed[which(nm$Country_Region==nms[j])])
    ts.d[i+61,j]=sum(nm$Deaths[which(nm$Country_Region==nms[j])])
    ts.r[i+61,j]=sum(nm$Recovered[which(nm$Country_Region==nms[j])])
    ts.c.pop[i+61,j]=sum(nm$Confirmed[which(nm$Country_Region==nms[j])])/pops[j]*100
    ts.d.pop[i+61,j]=sum(nm$Deaths[which(nm$Country_Region==nms[j])])/pops[j]*100
    ts.r.pop[i+61,j]=sum(nm$Recovered[which(nm$Country_Region==nms[j])])/pops[j]*100
  }
}
l=length(dates2)
maxUS=max(ts.c)
maxSpain=max(ts.c.pop)

ts.n=ts.c;ts.n.pop=ts.c.pop
for (i in 2:length(dates2)){
  ts.n[i,]=ts.c[i,]-ts.c[i-1,]
  ts.n.pop[i,]=ts.c.pop[i,]-ts.c.pop[i-1,]
}
ts.n[which(ts.n<0)]=0
ts.n.pop[which(ts.n.pop<0)]=0

CntryNm=as.character(labels(table(nm$Country_Region))[[1]])
CntryConf=CntryRecov=CntryDead=rep(0,length(CntryNm))
for (i in 1:length(CntryNm)){
  CntryConf[i]=sum(nm$Confirmed[which(nm$Country_Region==CntryNm[i])])
  CntryRecov[i]=sum(nm$Recovered[which(nm$Country_Region==CntryNm[i])])
  CntryDead[i]=sum(nm$Deaths[which(nm$Country_Region==CntryNm[i])])
}

CntryTots=data.frame(CntryNm=CntryNm,CntryConf=CntryConf,
                     CntryDead=CntryDead,CntryRecov=CntryRecov)
CntryTots5=CntryTots[which(CntryTots$CntryNm=="US"|
                             CntryTots$CntryNm=="Spain"|
                             CntryTots$CntryNm=="Italy"|
                             CntryTots$CntryNm=="Germany"|
                             CntryTots$CntryNm=="France"),]
CntryTots5$Pop=c(66990000,#france
                 82790000,#germany
                 60480000,#italy
                 46660000,#spain
                 327200000)#US
CntryTots5$CntryNm=as.character(CntryTots5$CntryNm)




################main panel##############

shinyServer(function(input,output){

  output$text <- renderText({
    paste("View the bar plots displaying information on COVID19 confirmed, recovered, 
          and dead cases in 10,000s and 
          as a percent of the population for several countries. Then, use the available 
          options to populate the time series plots. New Cases over time is displayed on 
          a separate plot for scaling purposes. These data are from the ",
          dates[length(dates)]," data release.",sep="")
  })
  
  output$barplot2 <- renderPlot({
    layout(1:3,heights=c(.45,.45,.1))
    par(mar=c(5.5,4,5,0))
    mat=t(cbind(CntryTots5$CntryConf,CntryTots5$CntryRecov,CntryTots5$CntryDead))/10000
    barplot(height=mat,beside=TRUE,col=c("black","gray","white"),
            las=2,names.arg=CntryTots5$CntryNm,cex.main=1.5,cex.axis = 1.5,
            main="Cases in 10,000s",
            ylim=c(0,max(mat)),cex.names = 1.5)
    matperc=(mat*10000/CntryTots5$Pop[rep(1:5,each=3)])*100
    barplot(height=matperc,beside=TRUE,col=c("black","gray","white"),
            las=2,names.arg=CntryTots5$CntryNm,cex.names = 1.5,cex.main=1.5,cex.axis = 1.5,
            main="Cases as % of the Population",
            ylim=c(0,max(matperc)))
    par(mar=c(0,0,0,0))
    plot(0,type='n',axes=FALSE)
    legend("center",legend=c("Confirmed","Recovered","Dead"),
           fill=c("black","gray","white"),horiz=TRUE,bty='n',cex=1.5)
  })
  
  output$lineplot <- renderPlot({
    if (input$adj==FALSE){
      plot(0,type='n',ylim=c(0,maxUS/10000),xlim=c(1,l),
           ylab="Selected Data in 10,000s",
           xlab="Days (beginning January 22, 2020)")
      if ("US"%in%input$optsCntry){
        if ("Active Cases"%in%input$optsData){
          lines(1:l,(ts.c[,5]-ts.d[,5]-ts.r[,5])/10000,
                type="l",col="blue",lty=1)
        }
        if ("Confirmed Cases"%in%input$optsData){
          lines(1:l,ts.c[,5]/10000,
                type="l",col="blue",lty=2)
        }
        if ("Recoveries"%in%input$optsData){
          lines(1:l,ts.r[,5]/10000,
                type="l",col="blue",lty=3)
        }
        if ("Deaths"%in%input$optsData){
          lines(1:l,ts.d[,5]/10000,
                type="l",col="blue",lty=4)
        }
      }
      if ("France"%in%input$optsCntry){
        if ("Active Cases"%in%input$optsData){
          lines(1:l,(ts.c[,1]-ts.d[,1]-ts.r[,1])/10000,
                type="l",col="red",lty=1)
        }
        if ("Confirmed Cases"%in%input$optsData){
          lines(1:l,ts.c[,1]/10000,
                type="l",col="red",lty=2)
        }
        if ("Recoveries"%in%input$optsData){
          lines(1:l,ts.r[,1]/10000,
                type="l",col="red",lty=3)
        }
        if ("Deaths"%in%input$optsData){
          lines(1:l,ts.d[,1]/10000,
                type="l",col="red",lty=4)
        }
      }
      if ("Germany"%in%input$optsCntry){
        if ("Active Cases"%in%input$optsData){
          lines(1:l,(ts.c[,2]-ts.d[,2]-ts.r[,2])/10000,
                type="l",col="black",lty=1)
        }
        if ("Confirmed Cases"%in%input$optsData){
          lines(1:l,ts.c[,2]/10000,
                type="l",col="black",lty=2)
        }
        if ("Recoveries"%in%input$optsData){
          lines(1:l,ts.r[,2]/10000,
                type="l",col="black",lty=3)
        }
        if ("Deaths"%in%input$optsData){
          lines(1:l,ts.d[,2]/10000,
                type="l",col="black",lty=4)
        }
      }
      if ("Spain"%in%input$optsCntry){
        if ("Active Cases"%in%input$optsData){
          lines(1:l,(ts.c[,4]-ts.d[,4]-ts.r[,4])/10000,
                type="l",col="gold",lty=1)
        }
        if ("Confirmed Cases"%in%input$optsData){
          lines(1:l,ts.c[,4]/10000,
                type="l",col="gold",lty=2)
        }
        if ("Recoveries"%in%input$optsData){
          lines(1:l,ts.r[,4]/10000,
                type="l",col="gold",lty=3)
        }
        if ("Deaths"%in%input$optsData){
          lines(1:l,ts.d[,4]/10000,
                type="l",col="gold",lty=4)
        }
      }
      if ("Italy"%in%input$optsCntry){
        if ("Active Cases"%in%input$optsData){
          lines(1:l,(ts.c[,3]-ts.d[,3]-ts.r[,3])/10000,
                type="l",col="forestgreen",lty=1)
        }
        if ("Confirmed Cases"%in%input$optsData){
          lines(1:l,ts.c[,3]/10000,
                type="l",col="forestgreen",lty=2)
        }
        if ("Recoveries"%in%input$optsData){
          lines(1:l,ts.r[,3]/10000,
                type="l",col="forestgreen",lty=3)
        }
        if ("Deaths"%in%input$optsData){
          lines(1:l,ts.d[,3]/10000,
                type="l",col="forestgreen",lty=4)
        }
      }
      legend(x=0,y=maxUS/10000,
             legend=c("France","Germany","Italy","Spain","US","Active Cases","Confirmed Cases","Recoveries","Deaths"),
            # fill=c("red","black","forestgreen","gold","blue",rep(NA,4)),
             lty=c(rep(1,5),1:4),
             col=c("red","black","forestgreen","gold","blue",rep("grey",4)),
             bty='n')
    } else {
      plot(0,type='n',ylim=c(0,maxSpain),xlim=c(1,l),
           ylab="Selected Data as a % of the Population",
           xlab="Days (beginning January 22, 2020)")
      if ("US"%in%input$optsCntry){
        if ("Active Cases"%in%input$optsData){
          lines(1:l,ts.c.pop[,5]-ts.d.pop[,5]-ts.r.pop[,5],
                type="l",col="blue",lty=1)
        }
        if ("Confirmed Cases"%in%input$optsData){
          lines(1:l,ts.c.pop[,5],
                type="l",col="blue",lty=2)
        }
        if ("Recoveries"%in%input$optsData){
          lines(1:l,ts.r.pop[,5],type="l",col="blue",lty=3)
        }
        if ("Deaths"%in%input$optsData){
          lines(1:l,ts.d.pop[,5],type="l",col="blue",lty=4)
        }
        if ("New Cases"%in%input$optsData){
          lines(1:l,ts.n.pop[,5],type="l",col="blue",lty=4)
        }
      }
      if ("France"%in%input$optsCntry){
        if ("Active Cases"%in%input$optsData){
          lines(1:l,ts.c.pop[,1]-ts.d.pop[,1]-ts.r.pop[,1],
                type="l",col="red",lty=1)
        }
        if ("Confirmed Cases"%in%input$optsData){
          lines(1:l,ts.c.pop[,1],
                type="l",col="red",lty=3)
        }
        if ("Recoveries"%in%input$optsData){
                  lines(1:l,ts.r.pop[,1],type="l",col="red",lty=3)
        }
        if ("Deaths"%in%input$optsData){
          lines(1:l,ts.d.pop[,1],type="l",col="red",lty=4)
        }
        if ("New Cases"%in%input$optsData){
          lines(1:l,ts.n.pop[,1],type="l",col="red",lty=4)
        }
      }
        if ("Germany"%in%input$optsCntry){
          if ("Active Cases"%in%input$optsData){
            lines(1:l,ts.c.pop[,2]-ts.d.pop[,2]-ts.r.pop[,2],
                  type="l",col="black",lty=1)
          }
          if ("Confirmed Cases"%in%input$optsData){
            lines(1:l,ts.c.pop[,2],type="l",col="black",lty=2)
          }
          if ("Recoveries"%in%input$optsData){
            lines(1:l,ts.r.pop[,2],type="l",col="black",lty=3)
          }
          if ("Deaths"%in%input$optsData){
            lines(1:l,ts.d.pop[,2],type="l",col="black",lty=4)
          }
          if ("New Cases"%in%input$optsData){
            lines(1:l,ts.n.pop[,2],type="l",col="black",lty=4)
          }
        }
        if ("Spain"%in%input$optsCntry){
          if ("Active Cases"%in%input$optsData){
            lines(1:l,ts.c.pop[,4]-ts.d.pop[,4]-ts.r.pop[,4],
                  type="l",col="gold",lty=1)
          }
          if ("Confirmed Cases"%in%input$optsData){
            lines(1:l,ts.c.pop[,4],type="l",col="gold",lty=2)
          }
          if ("Recoveries"%in%input$optsData){
            lines(1:l,ts.r.pop[,4],type="l",col="gold",lty=3)
          }
          if ("Deaths"%in%input$optsData){
            lines(1:l,ts.d.pop[,4],type="l",col="gold",lty=4)
          }
          if ("New Cases"%in%input$optsData){
            lines(1:l,ts.n.pop[,4],type="l",col="gold",lty=4)
          }
        }
        if ("Italy"%in%input$optsCntry){
          if ("Active Cases"%in%input$optsData){
            lines(1:l,ts.c.pop[,3]-ts.d.pop[,4]-ts.r.pop[,4],
                  type="l",col="forestgreen",lty=1)
          }
          if ("Confirmed Cases"%in%input$optsData){
            lines(1:l,ts.c.pop[,3],type="l",col="forestgreen",lty=2)
          }
          if ("Recoveries"%in%input$optsData){
            lines(1:l,ts.r.pop[,3],type="l",col="forestgreen",lty=3)
          }
          if ("Deaths"%in%input$optsData){
            lines(1:l,ts.d.pop[,3],type="l",col="forestgreen",lty=4)
          }
          if ("New Cases"%in%input$optsData){
            lines(1:l,ts.n.pop[,3],type="l",col="forestgreen",lty=4)
          }
        }
        legend(x=0,y=maxSpain,
               legend=c("France","Germany","Italy","Spain","US","Active Cases","Confirmed Cases","Recoveries","Deaths"),
               # fill=c("red","black","forestgreen","gold","blue",rep(NA,4)),
               lty=c(rep(1,5),1:4),
               col=c("red","black","forestgreen","gold","blue",rep("grey",4)),
               bty='n')
      }
    })  
  output$lineplot2 <- renderPlot({
    if (input$adj==FALSE){
      plot(0,type='n',ylim=c(0,max(ts.n)),xlim=c(1,l),
           ylab="New Cases",
           xlab="Days (beginning January 22, 2020)")
      if ("US"%in%input$optsCntry){
          lines(1:l,ts.n[,5],
                type="l",col="blue")
      }
      if ("France"%in%input$optsCntry){
          lines(1:l,ts.n[,1],
                type="l",col="red")
      }
      if ("Germany"%in%input$optsCntry){
          lines(1:l,ts.n[,2],
                type="l",col="black")
        }
      if ("Spain"%in%input$optsCntry){
          lines(1:l,ts.n[,4],
                type="l",col="gold")
      }
      if ("Italy"%in%input$optsCntry){
          lines(1:l,ts.n[,3],
                type="l",col="forestgreen")
      }
      legend(x=0,y=max(ts.n),legend=c("France","Germany","Italy","Spain","US"),
             col=c("red","black","forestgreen","gold","blue"),
             lty=1,
             bty='n')
    } else {
      plot(0,type='n',ylim=c(0,max(ts.n.pop)),xlim=c(1,l),
           ylab="New Cases as a % of the Population",xlab="Days (beginning January 22, 2020)")
      if ("US"%in%input$optsCntry){
          lines(1:l,ts.n.pop[,5],type="l",col="blue")
        }
      if ("France"%in%input$optsCntry){
          lines(1:l,ts.n.pop[,1],type="l",col="red")
      }
      if ("Germany"%in%input$optsCntry){
          lines(1:l,ts.n.pop[,2],type="l",col="black")
      }
      if ("Spain"%in%input$optsCntry){
          lines(1:l,ts.n.pop[,4],type="l",col="gold")
      }
      if ("Italy"%in%input$optsCntry){
          lines(1:l,ts.n.pop[,3],type="l",col="forestgreen")
      }
      legend(x=0,y=max(ts.n.pop),legend=c("France","Germany","Italy","Spain","US"),
             col=c("red","black","forestgreen","gold","blue"),
             lty=1,
             bty='n')
    }
  })  
})
  