
library(data.table)
library(Rcan)
library(ReporteRs)
library(ggplot2)

app_folder <- "C:/Projects/Rcan/Shiny"

source(paste(sep="/", app_folder, "source/Rcan_core.r"))

dt_CI5 <- data.table(readRDS(paste0(app_folder, "/data/CI5XI.rds")))
dt_continent <- data.table(read.csv(paste0(app_folder, "/data/continent_lab.csv")))
dt_cancer_color <- data.table(read.csv(paste0(app_folder, "/data/color_cancer.csv")))

dt_CI5 <- merge(dt_CI5, dt_continent, by="CI5_continent")

continent_list <- unique(dt_CI5$continent_lab)
sex_list <- c("Male", "Female")

shinyServer(function(input, output) {
  
  # Drop-down selection box for which data set
  output$choose_continent <- renderUI({
    selectInput("continent", "Continent", as.list(continent_list))
  })
  
  output$choose_sex <- renderUI({
    selectInput("sex", "Sex", as.list(sex_list))
  })
  

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$filename, ".pptx")
    },
    content = function(file) {
      
      #to test
      # input <- list()
      # input$continent <- "Africa"
      # input$sex <- "Male"
      
      dt_select <- dt_CI5[continent_lab == input$continent]
      if (input$sex == "Male") {
        dt_select <- dt_select[sex == 1]
      } else {
        dt_select <- dt_select[sex == 2]
      }
      
      # drop all site
      dt_select <- dt_select[cancer <62]
      # asr
      dt_temp <- data.table(csu_asr(dt_select, group_by=c("registry", "registry_lab", "cancer", "cancer_lab"), missing_age = 19))
      
      #keep only top 10
      dt_temp <- Rcan:::core.csu_dt_rank(dt_temp, var_value = "asr",
                                         var_rank = "cancer_lab",
                                         group_by = c("registry"),
                                         number = 10)
      
      

      #need to add function canreg_bar_top to rcan!!
      
      csu_ratio = 0.6 
      csu_bar_label_size = 4
      line_size <- 0.4
      text_size <- 14
      var_top <- "asr"
      var_bar <- "cancer_lab"
      nb_top <- 10
      xtitle<-paste0("Age-standardized incidence rate per ", formatC(100000, format="d", big.mark=","))
      landscape <- TRUE

      graph_width <- 8
      png_width <- ifelse(landscape, 2339 , 1654 )
      png_height <- ifelse(landscape, 1654 , 2339 )

      plot_subtitle <-  paste0("Top ",nb_top, " cancer sites\n", input$sex)
      
      dt_temp <- merge(dt_temp, dt_cancer_color, by=c("cancer_lab", "cancer"))


      
      #test graph for 1 registry
      dt_temp[, registry_lab:= factor(registry_lab)]
      

      withProgress(message = 'create slide', value = 0, {
        
      doc <- pptx(template=paste(sep="/", app_folder,"slide_template", "shiny_template.pptx"))
      
      
        
        n <- length(levels(dt_temp$registry_lab))
      
        for (i in levels(dt_temp$registry_lab)) {
          
            
          doc <- addSlide(doc, "Canreg_basic") ## add PPTX slide (Title + content)
        
          dt_plot <- dt_temp[registry_lab == i]
          plot_title <- unique(dt_plot$registry_lab)
          
          dt_plot$cancer_lab <-Rcan:::core.csu_legend_wrapper(dt_plot$cancer_lab, 15)
          dt_label_order <- setkey(unique(dt_plot[, c(var_bar,"cancer_color", "CSU_RANK"), with=FALSE]), CSU_RANK)
          dt_plot$cancer_lab <- factor(dt_plot$cancer_lab,levels = rev(dt_label_order$cancer_lab)) 
          color_cancer <- as.character(rev(dt_label_order$cancer_color))
          
          csu_plot <- csu_bar_plot(
            dt_plot,var_top=var_top,var_bar=var_bar,
            plot_title=plot_title,plot_caption=NULL,plot_subtitle = plot_subtitle,
            color_bar=color_cancer,
            landscape=TRUE,digit=1,
            xtitle=xtitle)
          
          filename <- paste0(tempdir(), "\\temp_graph")
          png(paste0(filename,".png"),width = png_width, height = png_height, units = "px",res = 200) 
          print(csu_plot)
          dev.off()
          
          dims <- attr( png::readPNG (paste0(tempdir(), "\\temp_graph.png")), "dim" )
          doc <- addImage(doc, paste0(tempdir(), "\\temp_graph.png"),width=graph_width,height=graph_width*dims[1]/dims[2])
         
          incProgress(1/n, detail=plot_title)
           
        }
        
      })

      writeDoc(doc,file)
    }
  )
  
  
  output$plot <- renderPlot({
    
    
    return()
  
  })
  
})