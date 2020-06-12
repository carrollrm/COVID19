library(shiny)

shinyUI(fluidPage(
  titlePanel("Spatial-temporal regression model of COVID-19 in the US"),
  sidebarLayout(
    sidebarPanel(
      p("By: Rachel Carroll, Ph.D.",a("(Personal Website)", 
                                      href ="https://rachelmcarroll.weebly.com/")),
      p(a("Link to host dashoard", 
          href = "https://carrollrm.shinyapps.io/COVIDmod")),
      p(a("Link to mobile friendly dashoard", 
          href = "https://carrollrm.shinyapps.io/COVIDmod_mobile")),
      br(),
   #   checkboxInput("pred",label="View the regression predictors only"),
      radioButtons("data",label="Data:",c("Confirmed Cases","Active Cases","New Cases","Deaths")),
      radioButtons("loc",label="Location:",c("South","West","Midwest","Northeast","United States")),
      submitButton("Update"),
      br(),
      p(a("International time series visualizations dashboard", 
          href = "https://carrollrm.shinyapps.io/COVID19ts")),
      p(a("Spatial-temporal COVID-19 visualizations dashboard", 
          href = "https://carrollrm.shinyapps.io/COVID19_US_NC_CF")),
      br(),
      br(),
      div(img(src = "UNCWlogo.png", height = 70, width = 150), style="text-align: center;"),
      br(),
      div(p(a("UNCW Data Science", 
          href = "https://uncw.edu/datascience")), style="text-align: center;"),
      br(),
      br(),
      p("Data Sources"),
      p("   COVID19: ",
        a("Johns Hopkins CSSE", 
          href = "https://github.com/CSSEGISandData/COVID-19")),
      p("   Airports: ",
        a("U.S. Department of Transportation", 
          href = "https://data.transportation.gov/Aviation/International_Report_Passengers/xgub-n9bw"),
        " and ",
        a("U.S. Bureau of Transportation Statistics",
          href = "https://koordinates.com/layer/748-us-airports")),
      p("   Stay Home: Several sources including Twitter, documentation of official government ordinances, and news articles. This data source is checked and updated daily for appropriate changes."),
      p("   Unemployment: ",
        a("U.S. Bureau of Labor Statistics made available by ESRI", 
          href = "https://coronavirus-resources.esri.com/datasets/esri::bureau-of-labor-statistics-monthly-unemployment-current-14-months?geometry=138.655%2C-0.672%2C-138.024%2C76.524&layer=2&showData=true")),
      p("   Smoking: ",
        a("Centers for Disease Control and Prevention made available by Harvard University", 
          href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VZ21KD"))
    ),
    mainPanel(
      textOutput('text'),
      br(),
      textOutput('text1'),
      br(),
      tableOutput("tab"),
      textOutput('text2'),
      plotOutput("map"),
      plotOutput("plot"))#,width=6)
    )
  )
)


