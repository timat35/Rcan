
library(data.table)
library(Rcan)
library(ReporteRs)

app_folder <- "C:/Projects/Rcan/Shiny"
dt_CI5 <- data.table(readRDS(paste0(app_folder, "/data/CI5X.rds")))
source(paste0(app_folder, "/source/Rcan_core.r"))


registry_list <- unique(dt_CI5$registry_lab)
cancer_list <- unique(dt_CI5$cancer_lab)

shinyServer(function(input, output) {
  
  # Drop-down selection box for which data set
  output$choose_registry <- renderUI({
    selectInput("registry", "Registry", as.list(registry_list))
  })
  
  output$choose_cancer <- renderUI({
    selectInput("cancer", "Cancer", as.list(cancer_list))
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$filename, ".pptx")
    },
    content = function(file) {
      
      
      dt_select <- dt_CI5[registry_lab == input$registry]
      dt_select <- dt_select[cancer_lab == input$cancer]
      
      dt_select$sex <- factor(dt_select$sex,levels=c(1,2), labels=c("Male", "Female"))
      
      temp <- Rcan:::core.csu_ageSpecific(df_data = dt_select,
                                          group_by="sex",
                                          missing_age = NULL,
                                          plot_title = input$registry,
                                          plot_subtitle = input$cancer)$csu_plot
      
      

      graph_width <- 8
      graph_width_split <- 4
      graph_width_vertical <- 5
      time_limit <- 9
      
      doc <- pptx(template=paste(sep="/", app_folder,"slide_template", "canreg_template.pptx"))
      
      #################
      doc <- addSlide(doc, "Canreg_basic") ## add PPTX slide (Title + content)
      doc <- addTitle(doc, "Age specific rate")
      
      landscape <- TRUE
      
      png_width <- ifelse(landscape, 2339 , 1654 )
      png_height <- ifelse(landscape, 1654 , 2339 )
      
      filename <- paste0(tempdir(), "\\temp_graph")
      
      png(paste0(filename,".png"),width = png_width, height = png_height, units = "px",res = 200) 
      
      
      print(temp)
      
      dev.off()
      
      
      dims <- attr( png::readPNG (paste0(tempdir(), "\\temp_graph.png")), "dim" )
      doc <- addImage(doc, paste0(tempdir(), "\\temp_graph.png"),width=graph_width,height=graph_width*dims[1]/dims[2])
      
      writeDoc(doc,file)
    }
  )
  
  
  output$plot <- renderPlot({
    
    if(is.null(input$registry))
      return()
  
    dt_select <- dt_CI5[registry_lab == input$registry]
    dt_select <- dt_select[cancer_lab == input$cancer]
    
    dt_select$sex <- factor(dt_select$sex,levels=c(1,2), labels=c("Male", "Female"))
    
    temp <- Rcan:::core.csu_ageSpecific(df_data = dt_select,
                                        group_by="sex",
                                        missing_age = NULL,
                                        plot_title = input$registry,
                                        plot_subtitle = input$cancer)$csu_plot
    
    print(temp)
    
  })
  
})