shinyUI(pageWithSidebar(
  
  headerPanel(""),
  
  sidebarPanel(
    uiOutput("choose_continent"),
    uiOutput("choose_sex"),
    textInput("filename", "Name of the pptx", "slide"),
    downloadButton('downloadData', 'Download')
   
  ),
  
  
  mainPanel(
    

  )
))