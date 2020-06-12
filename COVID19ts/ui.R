shinyUI(fluidPage(
  titlePanel("COVID-19 Over Time"),
  sidebarLayout(
  sidebarPanel(
    p("By: Rachel Carroll, Ph.D.",a("(Personal Website)", 
                                    href ="https://rachelmcarroll.weebly.com/")),
    p(a("Link to host dashoard",
      href = "https://carrollrm.shinyapps.io/COVID19ts/")),
   br(),
   p("Please allow time at launch for the data to load."),
   br(),
    checkboxGroupInput("optsCntry",label="Select at least one country",
                       choices=c("France","Germany","Italy","Spain","US"),
                       selected=c("US")),
   br(), 
   checkboxGroupInput("optsData",label="Select at least one data type",
                       choices=c("Confirmed Cases","Active Cases","Recoveries","Deaths"),
                       selected=c("Active Cases")),
   br(),
   checkboxInput("adj",label="Display as Percent of the Population"),
    submitButton("Update"),
   br(),
   p(a("Spatial-temporal US COVID19 dashboard", 
       href = "https://carrollrm.shinyapps.io/COVID19_US_NC_CF/")),
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
    plotOutput("barplot2"),
    plotOutput("lineplot"),
    plotOutput("lineplot2"))
)
)
)
