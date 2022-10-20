library(shiny)
library(DT)
library(shinydashboard)
library(dplyr)
library(plotly)
library(RColorBrewer)
library(semantic.dashboard)
library(shiny.semantic)
library(shinySignals)
library(readr)  
library(htmlwidgets) 
library(shinyWidgets)
library(stringr)
library(heatmaply)
setwd("/data11/jinoklee/trend")

## STUDY list
metadata <- read.table("./app/data/1_list/meta.study.id.txt", sep = "\t", quote = "", header = T,stringsAsFactors = F)
metadata$select  <- paste(metadata$Project, metadata$Study.title, sep = " : ")
trna <- read.table("./app/data/trna.txt", sep = "\t", stringsAsFactors = F, header = T)
## DEtRNA study list
DEmeta <- read.table("./app/data/5_detRNA/list.all.DEstudy.txt", sep = "\t", quote = "", header = T,stringsAsFactors = F)
# Rscript
source("app/data/R/pie.R")
source("app/data/R/volcano.R")
source("app/data/R/barplot.R")
source("app/data/R/pyramid.R")
source("app/data/R/boxplot.R")
source("app/data/R/tcga.R")
#source("app/data/R/dx.R")
source("app/data/R/tumor.R")
source("app/data/R/normal.R")


server <- function(input, output, session){
# output$main <- renderImage({
#   list(src = "./www/trex1.png",
#        width = 400)
# }, deleteFile = FALSE)

  observe({
    output$pie <- pie.plt()
  })
  
  output$plt <- renderPlotly({
    cols <- colorRampPalette(brewer.pal(10,"Paired"))(23)
    plot_ly(x = ti.site$Var1, y = ti.site$Freq, 
            type="bar", marker=list(color= cols, opacity = 0.6)) %>% layout(xaxis = list(categoryorder='trace'))
  })
  

  output$cl <- renderPlotly({
    plot_ly(x = cline$Var1, y = cline$Freq, 
            type="bar", marker=list(color= c(rep("#66C2A5", 41), rep("#FC8D62",22), rep("#8DA0CB", 11)), opacity = 0.6)) %>% layout(xaxis = list(categoryorder='trace'))
  })
  
  #tcga
    # observe({
    #   output$tcga  <- tcga()
    # })

    # observe({
    #   output$dx <- dx()
    # })
    
    # observe({
    #   output$tumor <- tumor()
    # })
    # 
    # observe({
    #   output$normal <- normal()
    # })
    
  css <- ".nowrap {
  white-space: nowrap;
}"

  output$prjlist <- DT::renderDataTable({
    df <- read.table("./app/data/1_list/meta.study.id.txt", sep = "\t", quote = "", header = T)
  },rownames = F
  ,extensions=c('FixedColumns','FixedHeader', 'Scroller')
  ,options = list(scrollX = TRUE,
                  fixedColumns = list(leftColumns = 2))
  )
    
    
  
  output$meta <- DT::renderDataTable({
    meta <- read.table("./app/data/1_list/meta.study.id.txt", sep = "\t", header = T)
    df <- read.table("./app/data/0_source/totl.sample.3.txt", sep = "\t", header = T )
    d <- event_data("plotly_click" )
    if(is.null(d)){
      meta
    }else if(length(grep(d$key, df$Reference)) > 1){
      tt <- df[grep(d$key, df$Reference),]
      meta[meta$Project%in%unique(tt$Project),]
    }else if(length(grep(d$key, df$Source)) > 1){
      tt  <- filter(df, Source == d$key)
      meta[meta$Project%in%unique(tt$Project),]
    }else if(length(grep(d$key, df$Cell.Line.Source.Type))> 1){
      tt <- filter(df, Cell.Line.Source.Type == d$key)
      meta[meta$Project%in%unique(tt$Project),]
    }else if(length(grep(d$key, df$Cell.Source.Type))){
      tt <- filter(df, Cell.Source.Type == d$key)
      meta[meta$Project%in%unique(tt$Project),]
    }else{
      meta
    }
  }
  ,extensions=c('FixedColumns','FixedHeader', 'Scroller')
  ,options = list(scrollX = TRUE,
                  fixedColumns = list(leftColumns = 2))
  )
  
  
    
  output$datalist <- DT::renderDataTable({
    read.table("./app/data/0_source/totl.sample.3.txt", sep = "\t", quote = "", header = T)
  },rownames = F
  ,extensions=c('FixedColumns','FixedHeader', 'Scroller')
  ,options = list(scrollX = TRUE,
                  fixedColumns = list(leftColumns = 2))
  )
  
  ## List========
  sm0 <- eventReactive(input$projects,{
    t <- gsub("\\:.*","",input$projects)
    gsub(" ","", t)
  })
  
  
  output$samplelist <- DT::renderDataTable({
    DT::datatable({
      prj <- sm0()
      df <- read.table(paste0("./app/data/2_metadata/", prj, ".txt"), sep = "\t", quote = "", header = T )
      df <- df[,colSums(is.na(df)) != nrow(df)]
      colnames(df)[1] <- "Sample.id"
      df <- subset(df, select = -c(Group))
      df
    }
    ,extensions=c('FixedColumns','FixedHeader', 'Scroller')
    ,options = list(autowidth = FALSE,
                    #fixedHeader = TRUE,
                    scroller = TRUE,
                    scrollX = TRUE,
                    scrollY= 500,
                    fixedColumns = list(leftColumns = 2)
                      ))
  })
  
  
  output$studylist <- DT::renderDataTable({
    DT::datatable({
      prj <- sm0()
      df <- read.table("./app/data/1_list/meta.study.id.txt", sep = "\t", quote = "", header = T)
      df <- df %>% filter(Project == prj)
      df <- df[,-c(1,2)]
      data.frame(t(df))
    }, colnames = ''
    , options = list(dom = 't')
    )
  })
  

  
  output$degr <- DT::renderDataTable({
    DT::datatable({
      prj <- sm0()
      df <- read.table("./app/data/1_list/meta.study.id.txt", sep = "\t", quote = "", header = T)
      df <- df %>% filter(Project == prj)
      df <- df[,-c(1:5), drop = F]
      df <- data.frame(t(df))
      gr <- data.frame(matrix("NA", nrow=1, ncol=1))
      colnames(gr) <- colnames(df)
      gr$t.df. <- as.character(actionLink(inputId = "L1", label = "link_1", 
                             onclick = paste0('$("#Project li a")[1].click();')))
      test <- rbind(df,gr)
      test
      
    }, colnames = ''
    , options = list(dom = 't')
    , escape = F
    )
  })
  ## heatmap 
  ## individual======================================
  output$heatmap1 <- renderImage({
    prj <- sm0()
    prj <- gsub("TCGA-","", prj)
    output <- paste0("./app/data/6_heatmap/cpm/individual.",prj,".cpm.png")
    list (src = output,
          width = 900)
  }, deleteFile = FALSE)
  
  
  output$indi_pdfdw <- downloadHandler(
    filename = function(){
      paste("Heatmap-", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      prj <- sm0()
      prj <- gsub("TCGA-","", prj)
      file.copy(paste0("./app/data/6_heatmap/cpm/individual.",prj,".cpm.pdf"), file)
    }
  )
  
  
  output$heatmap11 <- renderPlotly({
    prj <- sm0()
    prj <- gsub("TCGA-","", prj)
    validate(need(!prj%in%"BRCA", "Sorry! The graph is not available. Please download html and check it out on your local computer." ))
    load(paste0("./app/data/6_heatmap_html/cpm/individual.",prj,".cpm.RData"))
    h
  })
  
  output$indi_htmldw <- downloadHandler(
    filename = function(){
      paste("Heatmap-", Sys.Date(), ".html", sep="")
    },
    content = function(file) {
      prj <- sm0()
      prj <- gsub("TCGA-","", prj)
      file.copy(paste0("./app/data/6_heatmap_html/save/cpm/individual.",prj,".cpm.html"), file)
    }
  )
  
  
  ## isodecoderl======================================

  output$heatmap2 <- renderImage({
    prj <- sm0()
    prj <- gsub("TCGA-","", prj)
    output <- paste0("./app/data/6_heatmap/cpm/isodecoder.",prj,".cpm.png")
    list (src = output,
          width = 900)
  }, deleteFile = FALSE)
  
  output$deco_pdfdw <- downloadHandler(
    filename = function(){
      paste("Heatmap-", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      prj <- sm0()
      prj <- gsub("TCGA-","", prj)
      file.copy(paste0("./app/data/6_heatmap/cpm/isodecoder.",prj,".cpm.pdf"), file)
    }
  )
  

  output$heatmap22 <- renderPlotly({
    prj <- sm0()
    prj <- gsub("TCGA-","", prj)
    load(paste0("./app/data/6_heatmap_html/cpm/isodecoder.",prj,".cpm.RData"))
    h
  })
  
  output$deco_htmldw <- downloadHandler(
    filename = function(){
      paste("Heatmap-", Sys.Date(), ".html", sep="")
    },
    content = function(file) {
      prj <- sm0()
      prj <- gsub("TCGA-","", prj)
      file.copy(paste0("./app/data/6_heatmap_html/save/cpm/isodecoder.",prj,".cpm.html"), file)
    }
  )
  
  ## isoacceptor ======================================
  output$heatmap3 <- renderImage({
    prj <- sm0()
    prj <- gsub("TCGA-","", prj)
    output <- paste0("./app/data/6_heatmap/cpm/isoacceptor.",prj,".cpm.png")
    list (src = output,
          width = 900)
  }, deleteFile = FALSE)
  
  output$acep_pdfdw <- downloadHandler(
    filename = function(){
      paste("Heatmap-", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      prj <- sm0()
      prj <- gsub("TCGA-","", prj)
      file.copy(paste0("./app/data/6_heatmap/cpm/isoacceptor.",prj,".cpm.pdf"), file)
    }
  )
  
  output$heatmap33 <- renderPlotly({
    prj <- sm0()
    prj <- gsub("TCGA-","", prj)
    load(paste0("./app/data/6_heatmap_html/cpm/isoacceptor.",prj,".cpm.RData"))
    h
  })
  
  output$acep_htmldw <- downloadHandler(
    filename = function(){
      paste("Heatmap-", Sys.Date(), ".html", sep="")
    },
    content = function(file) {
      prj <- sm0()
      prj <- gsub("TCGA-","", prj)
      file.copy(paste0("./app/data/6_heatmap_html/save/cpm/isoacceptor.",prj,".cpm.html"), file)
    }
  )
  ## download========
  output$indiCPM <- downloadHandler(
    filename = function(){
      paste("Individual.tRNAs.cpm-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      prj <- sm0()
      prj <- gsub("TCGA-","", prj)
      data <- read.table(paste0("./app/data/4_normalization/cpm/individual/", prj,".cpm.txt"))
      write.csv(data, file)}
  )
  
  output$indiRPKM<- downloadHandler(
    filename = function(){
      paste("Individual.tRNAs.rpkm-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      prj <- sm0()
      prj <- gsub("TCGA-","", prj)
      data <- read.table(paste0("./app/data/4_normalization/rpkm/individual/", prj,".rpkm.txt"))
      write.csv(data, file)}
  )
  
  
  output$indiTPM<- downloadHandler(
    filename = function(){
      paste("Individual.tRNAs.tpm-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      prj <- sm0()
      prj <- gsub("TCGA-","", prj)
      data <- read.table(paste0("./app/data/4_normalization/tpm/individual/", prj,".tpm.txt"))
      write.csv(data, file)}
  )
  
  
  output$isodCPM <- downloadHandler(
    filename = function(){
      paste("Isodecoder.cpm-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      prj <- sm0()
      prj <- gsub("TCGA-","", prj)
      data <- read.table(paste0("./app/data/4_normalization/cpm/isodecoder/", prj,".cpm.txt"))
      write.csv(data, file)}
  )
  
  output$isoaCPM <- downloadHandler(
    filename = function(){
      paste("Isoacceptor.cpm-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      prj <- sm0()
      prj <- gsub("TCGA-","", prj)
      data <- read.table(paste0("./app/data/4_normalization/cpm/isoacceptor/", prj,".cpm.txt"))
      write.csv(data, file)}
  )
  
  output$samList <- downloadHandler(
    filename = function(){
      paste("SampleList-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      prj <- sm0()
      df <- read.table(paste0("./app/data/2_metadata/", prj, ".txt"), sep = "\t", quote = "", header = T )
      df <- df[,colSums(is.na(df)) != nrow(df)]
      colnames(df)[1] <- "Sample.id"
      df <- subset(df, select = -c(Group))
      write.csv(df, file)}
  )
  #
  
  observeEvent(input$goDE,{
   print(input$tab)
  })

  
  ## fitler DE project list========
  df0 <- eventReactive(input$RCs,{
    DEmeta %>% filter(S.Select == input$RCs)
  })


  output$subRCs <- shiny::renderUI({
    selectInput("subgroup",width = '100%', label = shiny::tags$div(style = "font-size:20px;color :#CB4335;","Select Condition"), choices = unique(df0()$B.Select)) 
  })
  

  detRNA <- eventReactive(input$derun,{
    # t <- gsub("\\:.*", "", input$subgroup)
    if(nrow(df0()) == 1){
      fn <- unique(df0()$ID)
    }else{
      fn <- unique(df0()$ID[df0()$B.Select%in%input$subgroup])# unique(df0()$ID[grep(t, df0()$B.Select)])  # 
    }
    dt <- read.table(paste0("./app/data/5_detRNA/individual/stat/",unique(fn),"_stat_individual.txt"), sep = '\t', header = T,stringsAsFactors = F)
    fc <- as.numeric(input$fc)
    pval <- as.numeric(input$pval)
    dt <- mutate(dt, Sig=ifelse(dt$FDR<=pval & dt$logFC<=-fc , "Down.sig", ifelse(dt$FDR<=0.05 & dt$logFC>=fc,"Up.sig", "No.sig")))
    dt <- arrange(dt, FDR)
    return(dt)
  })
  
  decodon <- eventReactive(input$derun,{
    t <- gsub("\\:.*", "", input$subgroup)
    if(nrow(df0()) == 1){
      fn <- unique(df0()$ID)
    }else{
      fn <- unique(df0()$ID[grep(t, df0()$B.Select)]) 
    }
    dt <- read.table(paste0("./app/data/5_detRNA/isodecoder/stat/",unique(fn),"_stat_isodecoder.txt"), sep = '\t', header = T,stringsAsFactors = F)
    fc <- as.numeric(input$fc)
    pval <- as.numeric(input$pval)
    dt <- mutate(dt, Sig=ifelse(dt$FDR<=pval & dt$logFC<=-fc , "Down.sig", ifelse(dt$FDR<=0.05 & dt$logFC>=fc,"Up.sig", "No.sig")))
    dt <- arrange(dt, FDR)
    return(dt)
  })
  
  deaa<- eventReactive(input$derun,{
    t <- gsub("\\:.*", "", input$subgroup)
    if(nrow(df0()) == 1){
      fn <- unique(df0()$ID)
    }else{
      fn <- unique(df0()$ID[grep(t, df0()$B.Select)]) 
    }
    dt <- read.table(paste0("./app/data/5_detRNA/isoacceptor/stat/",unique(fn),"_stat_isoacceptor.txt"), sep = '\t', header = T,stringsAsFactors = F)
    fc <- as.numeric(input$fc)
    pval <- as.numeric(input$pval)
    dt <- mutate(dt, Sig=ifelse(dt$FDR<=pval & dt$logFC<=-fc , "Down.sig", ifelse(dt$FDR<=0.05 & dt$logFC>=fc,"Up.sig", "No.sig")))
    dt <- arrange(dt, FDR)
    return(dt)
  })
  

  # volcano plot ========
  output$tRNA_volcano <- renderPlotly({
    fc <- as.numeric(input$fc)
    pval <- as.numeric(input$pval)
    volcano(detRNA(),fc,pval)
  })
  
  output$indistat <-DT::renderDataTable(server = FALSE,{
    DT::datatable({
      df <- detRNA()
      df[,c(2:6)] <- sapply(df[,c(2:6)], function(x) format(x,scientific= T, digits=4))
      df[,c(7)] <- as.factor(df[,c(7)])
      data.frame(df)
    },rownames = F
    , filter = 'top'
    ,extensions=c('FixedHeader', 'Scroller', 'Buttons')
    ,options = list(autowidth = FALSE,
                    dom = 'Bti',
                    buttons =list(list(extend="csv", filename = "Individual.tRNAs.stat")),
                    scroller = TRUE,
                    scrollX = TRUE,
                    scrollY= 390)
    
    )})
  # bar plot ========
  output$codon_barplot <- renderPlot({
      if(length(grep("No.sig", decodon()$Sig)) == nrow(decodon())){
       ggplot()+ theme_void()+annotate("text", x = 1,y=1, label="There is no significant DE tRNAs", size = 7)
      }else{
        barplot(decodon())
      }
  })
  
  output$isodstat <-  DT::renderDataTable(server = FALSE,{
    DT::datatable({
      df <- decodon()
      df[,c(2:6)] <- sapply(df[,c(2:6)], function(x) format(x,scientific= T, digits=4))
      df[,c(7)] <- as.factor(df[,c(7)])
      data.frame(df)
    },rownames = F
    , filter = 'top'
    ,extensions=c('FixedHeader', 'Scroller')
    ,options = list(autowidth = FALSE,
                    dom = 'Bti',
                    buttons =list(list(extend="csv", filename = "Isodecoders.stat")),
                    scroller = TRUE,
                    scrollX = TRUE,
                    scrollY= 390)
    )})
  
  # pyrmiad=========
  
  output$aa_pyramid <- renderPlotly({
      if(length(grep("No.sig", deaa()$Sig)) == nrow(deaa())){
        ggplot() +  theme_void()+annotate("text", x = 1,y=1, label="There is no significant DE tRNAs", size = 6)
      }else{
      pyramid(deaa())
        }
  })
  
  output$isoastat <-  DT::renderDataTable(server = FALSE,{
    DT::datatable({
      df <- deaa()
      df[,c(2:6)] <- sapply(df[,c(2:6)], function(x) format(x,scientific= T, digits=4))
      df[,c(7)] <- as.factor(df[,c(7)])
      data.frame(df)
    },rownames = F
    , filter = 'top'
    ,extensions=c('FixedHeader', 'Scroller')
    ,options = list(autowidth = FALSE,
                    buttons =list(list(extend="csv", filename = "Isoacceptors.stat")),
                    dom = 'Bti',
                    scroller = TRUE,
                    scrollX = TRUE,
                    scrollY= 390)
    )})
  
  ## filter tRNAs========
  tr0 <- eventReactive(input$tRNA1,{
    trna %>% filter(trna$aa %in% input$tRNA1)
  })
  
  output$tRNA2 <- shiny::renderUI({
    selectInput("isoacceptor", "Isocceptor",choices = unique(tr0()$isoacceptor ))
  })
  
  tr1 <- eventReactive(input$isoacceptor,{
    trna %>% filter(trna$isoacceptor %in% input$isoacceptor)
  })
  
  output$tRNA3 <- shiny::renderUI({
    selectInput("isodecoder","Isodecoder", choices= unique(tr1()$isodecoder))
  })
  
  tr2 <- eventReactive(input$isodecoder, {
    trna %>% filter(trna$isodecoder %in% input$isodecoder)
  })
  
  output$tRNA4 <- shiny::renderUI({
    selectInput("InditRNA", "Individual tRNA",choices = unique(tr2()$indi))
  })
  


  ### boxplot ===============================================================================================
  # individual tRNA=============================================================================================
  inditRNA <- eventReactive(input$InditRNA,{
    input$InditRNA
  })
  
  output$nttRNA_boxplot <- renderPlotly({
    i <- inditRNA()
    load(paste0("./app/data/8_tRNA/cpm/individual/", i, ".normal.tissue.individual.cpm.RData"))
    tt
    })

  output$dttRNA_boxplot <- renderPlotly({
    i <- inditRNA()
    load(paste0("./app/data/8_tRNA/cpm/individual/", i, ".tumor.tissue.individual.cpm.RData"))
    tt
  })
  
  
  output$nctRNA_boxplot <- renderPlotly({
    i <- inditRNA()
    load(paste0("./app/data/8_tRNA/cpm/individual/", i, ".celine.immortal.individual.cpm.RData"))
    tt
  })
  
  
  output$dctRNA_boxplot <- renderPlotly({
    i <- inditRNA()
    load(paste0("./app/data/8_tRNA/cpm/individual/", i, ".celine.cancer.individual.cpm.RData"))
    tt
  })
  
  #isodecoder=============================================================================================
  isodecodertRNA <- eventReactive(input$isodecoder,{
    input$isodecoder
  })
  
  output$ntcodon_boxplot<- renderPlotly({
    i <- isodecodertRNA()
    load(paste0("./app/data/8_tRNA/cpm/isodecoder/", i, ".normal.tissue.isodecoder.cpm.RData"))
    tt
  })

  output$dtcodon_boxplot <- renderPlotly({
    i <- isodecodertRNA()
    load(paste0("./app/data/8_tRNA/cpm/isodecoder/", i, ".tumor.tissue.isodecoder.cpm.RData"))
    tt
  })

  output$nccodon_boxplot <- renderPlotly({
    i <- isodecodertRNA()
    load(paste0("./app/data/8_tRNA/cpm/isodecoder/", i, ".celine.immortal.isodecoder.cpm.RData"))
    tt
  })
  
  
  output$dccodon_boxplot <- renderPlotly({
    i <- isodecodertRNA()
    load(paste0("./app/data/8_tRNA/cpm/isodecoder/", i, ".celine.cancer.isodecoder.cpm.RData"))
    tt
  })
  
  #isoacceptor=============================================================================================
  isoacceptortRNA <- eventReactive(input$isoacceptor,{
    input$isoacceptor
  })
  
  output$ntaa_boxplot <- renderPlotly({
    i <- isoacceptortRNA()
    i <- paste0("tRNA-",i)
    load(paste0("./app/data/8_tRNA/cpm/isoacceptor/", i, ".normal.tissue.isoacceptor.cpm.RData"))
    tt
  })

  output$dtaa_boxplot <- renderPlotly({
    i <- isoacceptortRNA()
    i <- paste0("tRNA-",i)
    load(paste0("./app/data/8_tRNA/cpm/isoacceptor/", i, ".tumor.tissue.isoacceptor.cpm.RData"))
    tt
  })
  # 
  
  output$ncaa_boxplot <- renderPlotly({
    i <- isoacceptortRNA()
    i <- paste0("tRNA-",i)
    load(paste0("./app/data/8_tRNA/cpm/isoacceptor/", i, ".celine.immortal.isoacceptor.cpm.RData"))
    tt
  })
  
  
  output$dcaa_boxplot <- renderPlotly({
    i <- isoacceptortRNA()
    i <- paste0("tRNA-",i)
    load(paste0("./app/data/8_tRNA/cpm/isoacceptor/", i, ".celine.cancer.isoacceptor.cpm.RData"))
    tt
  })
  
  ## project group pattern ========================
 
  tp0 <- eventReactive(input$tproject,{
    t <- gsub("\\:.*","",input$tproject)
    t <- gsub(" ","", t)
    gsub("TCGA-","",t)
  })
  
  
  output$ptRNA_boxplot <- renderPlotly({
    i <- inditRNA()
    p <- tp0()
    t <- "individual"
    load(file.path("./app/data/8_tRNA_prj",p,t, paste(i,p,t,"cpm","RData", sep = ".")))
    tt
  })
  
  output$pcodon_boxplot <- renderPlotly({
    i <- isodecodertRNA()
    p <- tp0()
    t <- "isodecoder"
    load(file.path("./app/data/8_tRNA_prj",p,t, paste(i,p,t,"cpm","RData", sep = ".")))
    tt
  })
  
  output$paa_boxplot <- renderPlotly({
    i <- isoacceptortRNA()
    i <- paste0("tRNA-",i)
    p <- tp0()
    t <- "isoacceptor"
    load(file.path("./app/data/8_tRNA_prj",p,t, paste(i,p,t,"cpm","RData", sep = ".")))
    tt
  })
  
  ## web liks========
  web <- eventReactive(input$InditRNA,{
    i <- input$InditRNA
    t <- read.csv(file.path("./app/data/7_html_real", paste0(i, "_html_s.tbl")), header = F)
    t$V2 <- gsub("<a href=", "<a href='", t$V2)
    t$V2 <- gsub(" > ", "' target='_blank'>", t$V2)
    t <- t[-1,]
    t
  })
  output$weblink <- renderTable({
    web()
  }, colnames = F, sanitize.text.function = function(x) x)
  

  output$trnadf <- renderTable({
    if ( vt$doTable == FALSE) return ()
    isolate({
      if( input$indit != "Select"){
        i = input$indit
        df <- read.csv(file.path("./app/data/7_html_real", paste0(i, "_html_s.tbl")), header = F)
      }else{df <- data.frame(NA)}
      df
    })
  }, colnames = F, sanitize.text.function = function(x) x)
  
}