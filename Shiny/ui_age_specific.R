shinyUI(pageWithSidebar(
  
  headerPanel(""),
  
  sidebarPanel(
    uiOutput("choose_registry"),
    uiOutput("choose_cancer"),
    textInput("filename", "filename", "slide"),
    downloadButton('downloadData', 'Download')
   
  ),
  
  
  mainPanel(
    plotOutput("plot", height = "800px")

  )
))