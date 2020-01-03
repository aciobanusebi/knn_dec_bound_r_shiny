shinyUI(fluidPage(
  useSweetAlert(),
  useShinyjs(),
  tags$style(appCSS),
  titlePanel("kNN with Euclidian Distance - Decision Boundaries"),
  tags$p("'Ties are broken at random.'"),
  tags$p("'If there are ties for the kth nearest vector, all candidates are included in the vote.'"),
  sidebarLayout(
    
    sidebarPanel(
      tags$a(href="data.zip","Examples of data files"),
      fileInput("file", "Choose CSV File with input data",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      textInput("k", "Value for k:", "1"),
      
      textInput("x_min", "Minimum value on horitonzal axis [for drawing]:","0"),
      textInput("x_max", "Maximum value on horizontal axis [for drawing]:","0"),
      
      textInput("y_min", "Minimum value on vertical axis [for drawing]:","0"),
      textInput("y_max", "Maximum value on vertical axis [for drawing]:","0"),
      
      withBusyIndicatorUI(
        actionButton("go","Go!")
      )
    ),
    
    mainPanel(
       plotOutput("plot",height = 700),
       plotOutput("auxPlot",height = 700)
    )
  )
)
)
