library(shiny)

shinyUI(fluidPage(
  titlePanel("COVID-19 in the US, North Carolina, and the Cape Fear Region"),
  sidebarLayout(
    sidebarPanel(
      p("By: Rachel Carroll, Ph.D.",a("(Personal Website)", 
                                      href ="https://rachelmcarroll.weebly.com/")),
      p(a("Link to mobile friendly dashoard", 
          href = "https://carrollrm.shinyapps.io/COVID19_US_NC_CF_mobile/")),
      p(a("Link to host dashoard", 
          href = "https://carrollrm.shinyapps.io/COVID19_US_NC_CF/"),"(not mobile friendly)"),
      br(),
      p("Please allow time at launch for the data to load."),
      sliderInput("day", label="Days beginning with March 22, 2020", 
                  min=1, 
                  max=as.numeric(as.Date(Sys.time(),tz="US/Eastern",format="%m-%d-%Y")-as.Date("03-23-2020",format="%m-%d-%Y")), 
                  value=as.numeric(as.Date(Sys.time(),tz="US/Eastern",format="%m-%d-%Y")-as.Date("03-23-2020",format="%m-%d-%Y"))),      
      radioButtons("data",label="Data:",c("Confirmed Cases","Active Cases","New Cases","Deaths")),
      helpText("Active Cases does not consider recoveries at this time because US recoveries are not being tracked at the county level."),
      checkboxInput("adj",label="As percent of the population"),
      radioButtons("loc",label="Location:",c("Region","State","Country"),
                   selected="Region"),
      helpText("Selecting Country leads to slower load times."),
      submitButton("Update"),
      br(),
      p(a("International time series visualizations dashboard", 
          href = "https://carrollrm.shinyapps.io/COVID19ts/")),
      p(a("Spatio-temporal modeling of COVID19 dashboard", 
          href = "https://carrollrm.shinyapps.io/COVIDmod/")),
      br(),
      br(),
      div(img(src = "UNCWlogo.png", height = 70, width = 150), style="text-align: center;"),
      br(),
      div(p(a("UNCW Data Science", 
          href = "https://uncw.edu/datascience/")), style="text-align: center;"),
      br(),
      br(),
      p("Data Source: ",
        a("Johns Hopkins CSSE", 
          href = "https://github.com/CSSEGISandData/COVID-19"))
    ),
    mainPanel(
                       textOutput("text"),
                       plotOutput("map"),
                       plotOutput("barplot"),width=6)
    )
  )
)


