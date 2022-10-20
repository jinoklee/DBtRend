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
metadata <- arrange(metadata, Reference)
metadata[grep("PRJNA", metadata$Project),] <- arrange(metadata[grep("PRJNA", metadata$Project),], Project)

trna <- read.table("./app/data/trna.txt", sep = "\t", stringsAsFactors = F, header = T)
## DEtRNA study list
DEmeta <- read.table("./app/data/5_detRNA/list.all.DEstudy.txt", sep = "\t", quote = "", header = T,stringsAsFactors = F)



HEADER <- dashboardHeader(color = "black", inverted = T, # ----------------
                          left = shiny::tagList(h2("DBtRend", style="margin-left:200px; color: white; font-weight: bold")),
                          titleWidth = "thin", show_menu_button = TRUE)

SIDEBAR <- dashboardSidebar(size = "",# ----------------
                            overlay = FALSE,
                            pushable = TRUE,
                            visible = TRUE,
                            dim_page = FALSE,
                            closable = FALSE,
                            sidebarMenu(
                              menuItem("Home", tabName = "Home", icon = icon("home")),
                              menuItem("Explore Projects", tabName = "List", icon = icon("globe")),
                              menuItem("Explore DEtRNAs", tabName = "Project", icon = icon("cubes")),
                              menuItem("Explore tRNAs", tabName = "tRNA",icon=icon("chart bar")),
                              menuItem("Download", tabName = "Download",icon=icon("download")),
                              menuItem("Help", tabName = "Guide",icon=icon("book")),
                              menuItem("Contact", tabName = "Contact",icon=icon("mail")),
                              br(),
                              br(),
                              hr()
                              #,
                              #h5(shiny::tags$div(HTML('<i class="far fa-bell" style = "color:black; margin-left:50px; "></i> Update Information')))
                              #,
                              #h6(shiny::tags$div(HTML('<i class="far fa-calendar-alt" style = "color:black; margin-left:30px; "></i> &nbsp 2021-09-07 &nbsp Total 183 Projects')))
  )
)


BODY <- dashboardBody(theme = "darkly",
  tabItems(
    tabItem(tabName = "Home", fluid = TRUE, # ----------------
            br(),
            br(),
            br(),
            column(16,align="center",
                            div(shiny::tags$img(src="main.png", width = "1100"), align="center")),
            h2("Data"),
            fluidRow(column(width= 8, align="center",
              infoBox("TCGA Projects", 33, color = "red", width = 6, size = "small")),
              column(width = 8, align="center",#11078,
              infoBox("GEO Projects", 150 ,color = "red", width = 6, size = "small"))),
            fluidRow(column(width = 16,
                           plotlyOutput("pie",height = "350"),
                           DT::dataTableOutput("meta",height = "200px")
                           )),
            fluidRow(column(16, align="center",
            shiny::HTML("<br><br><h5><center> We are applying the Creative Commons Attribution 4.0 International License
                        for all copyrigh table material on our website </cencer></h5>"))),
            fluidRow(column(16, align= "center",
                            shiny::HTML("<h5><center> @Copyright 2021 </cencer></h5>")))
            ),
    tabItem(tabName = "List", # ----------------
            br(),
            br(),
            h2("tRNA gene expression within projects"),
            fluidRow(fluid = FALSE,
                     box(width = 16,  color = "red" ,
                         selectInput(width = '100%', "projects", label = div(style = "font-size:20px;color :#CB4335;","Select Project"),
                                     choices = unique(metadata$select)))),
            fluidRow(fluid = FALSE,
                     tabBox(width =16, color = "blue",tabs = list(
                       list(menu = "Sample Information", content = list(br(),DT::dataTableOutput("samplelist"))),
                       list(menu = "Individual tRNAs",content = list(
                         tabBox(width =16, tabs = list(
                           list(menu="Static heatmap", content=list(column(9),column(3, downloadLink("indi_pdfdw", "Download PDF file")),column(12,imageOutput("heatmap1")))),
                           list(menu="Interactive heatmap", content=list(column(9, h5("It takes some time to load.  Hold on a minute, please. ")),
                                                                         column(3, downloadLink("indi_htmldw", "Download HTML file")),
                                                                         column(12, plotlyOutput("heatmap11", height = 1000, width = 900))
                                                                         )))))),
                       list(menu = "Isodecoders",content = list(
                         tabBox(width =16, tabs = list(
                           list(menu="Static heatmap", content=list(column(9),column(3, downloadLink("deco_pdfdw", "Download PDF file ")),column(12,imageOutput("heatmap2")))),
                           list(menu="Interactive heatmap", content=list(column(9, h5("It takes some time to load.  Hold on a minute, please. ")),
                                                                         column(3, downloadLink("deco_htmldw", "Download HTML file")),
                                                                         column(12,plotlyOutput("heatmap22", height = 1000, width = 900))
                           )))))),
                       list(menu = "Isoacceptors",content = list(
                         tabBox(width =16, tabs = list(
                           list(menu="Static heatmap", content=list(column(9),column(3, downloadLink("acep_pdfdw", "Download PDF file")),column(12,imageOutput("heatmap3")))),
                           list(menu="Interactive heatmap", content=list(column(9, h5("It takes some time to load.  Hold on a minute, please. ")),
                                                                         column(3, downloadLink("acep_htmldw", "Download HTML file")),
                                                                         column(12,plotlyOutput("heatmap33", height = 1000, width = 900))
                           ))))))
                       ,
                       list(menu = "Download", content =
                              fluidRow(column(8,semantic.dashboard::box(width = 8, DT::dataTableOutput("studylist", height = '330px'))),
                                       column(8,semantic.dashboard::box(height = '200px',
                                                                       column(width = 2,
                                                                              h3("Individual tRNAs count matrix"),
                                                                              downloadBttn("indiCPM", "CPM", size = "sm", color = "royal", style = "simple"),
                                                                              downloadBttn("indiRPKM", "RPKM", size = "sm", color = "royal", style = "simple"),
                                                                              downloadBttn("indiTPM", "TPM", size = "sm", color = "royal", style = "simple")),
                                                                       column(width = 2,
                                                                              h4(),
                                                                              h3("Isodecoders count matrix"),
                                                                              downloadBttn("isodCPM", "CPM", size = "sm", color = "royal", style = "simple")),
                                                                       column(width = 2,
                                                                              h4(),
                                                                              h3("Isoacceptor count matrix"),
                                                                              downloadBttn("isoaCPM", "CPM", size = "sm", color = "royal", style = "simple")),
                                                                       column(width = 2,
                                                                              h4(),
                                                                              h3("Sample Information"),
                                                                              downloadBttn("samList", "List", size = "sm", color = "royal", style = "simple"))
                                               ))))
                       # ,
                       # list(menu="Link to Explore DEtRNAs", content=
                       #        fluidRow(column(8, textOutput(outputId = "grinfo"),action_button("goDE", label="Go DEtRNAs"))))
                              #fluidRow(column(8, DT::dataTableOutput("degr", height = '100px')) )
                            )))),
    tabItem(tabName = "Project",fluid = TRUE,# ----------------
            br(),
            br(),
            h2("Differential expression of tRNA genes across conditions"),
            fluidRow(
              semantic.dashboard::box(width =16, color = "red",
                                      selectInput(width = '100%', "RCs",  
                                                  label = div(style = "font-size:20px;color :#CB4335;","Select Project"),
                                                  choices = unique(DEmeta$S.Select)),
                                      br(),
                                      shiny::uiOutput("subRCs"),
                                      #h4("Setting the values"),
                                      br(),
                                      shiny::tags$div(style = "font-size:20px;color:#CB4335;font-weight:bold;","Select Values"),
                                      split_layout(
                                        cell_widths =c("25%", "25%", "50%"),
                                        cell_args = "padding: 10px;",
                                        style = "border: 1px white;",
                                        numeric_input('pval', label="Adjusted p-value (BH) < ",
                                                                 min=0, max = 0.1,value = 0.05, step = 0.01, width="500px"),
                                        numeric_input('fc', label="|log2Fold Change| > ",
                                                                 min = 0, max = 10, value = 1, step = 0.5, width = "500px")
                                      ),
                                      br(),
                                      action_button("derun",label = div(style ="color :white;","RUN"), width = "100px",
                                                    style = "background-color: #CB4335;")
                                      )
              ),
            #projects ----------------------------------------------------------------
            fluidRow(fluid = FALSE, style = "height:1500px",
                     semantic.dashboard::tab_box(
                       width =16, color = "blue",tabs = list(
                         list(menu = "Individual tRNAs",
                              content = fluidRow(column(16, 
                                                        semantic.dashboard::box(width=6, div(style = 'overflow-x: scroll; height:650px',
                                                                                             h3("Results of the statistical analysis"),
                                                                                             DT::dataTableOutput("indistat")))),
                                                 column(16, semantic.dashboard::box(width=6, div(style = 'overflow-x: scroll; height:600px',
                                                                                                 h3("Volcano plot for differentially expressed individual tRNAs"),
                                                                                                 plotlyOutput("tRNA_volcano" )))))),
                         list(menu = "Isodecoders",
                              content = fluidRow(column(16, 
                                                        semantic.dashboard::box(div(style = 'overflow-x: scroll; height:650px',
                                                                                    h3("Results of the statistical analysis"),
                                                                                    DT::dataTableOutput("isodstat" )))),
                                                 column(16, semantic.dashboard::box(div(style = 'overflow-x: scroll; height:500px',
                                                                                        h3("Bar plot for differentially expressed isodecoders"),
                                                                                        plotOutput("codon_barplot" )))))),
                         list(menu = "Isoacceptors",
                              content = fluidRow(column(16, semantic.dashboard::box( width=6,div(style = 'overflow-x: scroll; height:650px',
                                                                                                 h3("Results of the statistical analysis"),
                                                                                                 DT::dataTableOutput("isoastat")))),
                                                 column(16, semantic.dashboard::box( width=6,div(style = 'overflow-x: scroll; height:600px',
                                                                                                 h3("Pyramid plot for differentially expressed isoacceptors"),
                                                                                                 plotlyOutput("aa_pyramid"))))
                                                                           
                                                        ))))
                     )),
    tabItem(tabName = "tRNA", fluid = TRUE,# ----------------
            br(),
            br(),
            h2("Expression of human tRNA genes"),
            br(),
            br(),
            fluidRow(fulid = TRUE,# ----------------
                     column(width = 1),
                     column(width = 4 ,
                            sidebarPanel(width = 12,
                            selectInput("tRNA1", "Aminoacid",choices = unique(trna$aa))),
                            shiny::uiOutput("tRNA2"),
                            shiny::uiOutput("tRNA3"),
                            shiny::uiOutput("tRNA4")),
                     column(width = 6,
                            tableOutput("weblink"))
            ),
            fluidRow(fluid = FALSE, style = "height:2500px",# ----------------
                     column(width= 16, h3(shiny::tags$div(HTML('<i class="fas fa-caret-right" style = "color:black;"></i> Expression patterns across normal tissues')))), # normal tissue-(nt)-------------
                     column(width = 16,
                            tabBox(width =16, color = "red",tabs = list(
                              list(menu = "Individual tRNAs",
                                   content = list(plotlyOutput("nttRNA_boxplot", width = 1000, height = 300))),
                              list(menu = "Isodecoders",
                                   content = list(plotlyOutput("ntcodon_boxplot", width = 1000, height =300))),
                              list(menu ="Isoacceptors",
                                   content = list(plotlyOutput("ntaa_boxplot", width = 1000, height = 300))))
                            )),
                     column(width= 16, h3(shiny::tags$div(HTML('<i class="fas fa-caret-right" style = "color:black;"></i> Expression patterns across tumor tissues')))), # tumor tissue--(dt)------------
                     column(width = 16,
                            tabBox(width =16, color = "red"  ,tabs = list(
                              list(menu = "Individual tRNAs",
                                   content = list(plotlyOutput("dttRNA_boxplot", width = 1000, height = 300))),
                              list(menu = "Isodecoders",
                                   content = list(plotlyOutput("dtcodon_boxplot", width = 1000, height = 300))),
                              list(menu ="Isoacceptors",
                                   content = list(plotlyOutput("dtaa_boxplot", width = 1000, height = 300)))))
                            ),
                     column(width= 16, h3(shiny::tags$div(HTML('<i class="fas fa-caret-right" style = "color:black;"></i> Expression patterns across immortal cell lines')))), # immortal cell line-(nc)------------------
                     column(width = 16,
                            tabBox(width =16, color = "red"  ,tabs = list(
                              list(menu = "Individual tRNAs",
                                   content = list(plotlyOutput("nctRNA_boxplot", width = 1000, height = 300))),
                              list(menu = "Isodecoders",
                                   content = list(plotlyOutput("nccodon_boxplot", width = 1000, height = 300))),
                              list(menu ="Isoacceptors",
                                   content = list(plotlyOutput("ncaa_boxplot", width = 1000, height = 300)))))),
                     column(width= 16, h3(shiny::tags$div(HTML('<i class="fas fa-caret-right" style = "color:black;"></i> Expression patterns across cancer cell lines')))), # cancer cell line -(dc)-------------
                     column(width = 16,
                            tabBox(width =16, color = "red" ,tabs = list(
                              list(menu = "Individual tRNAs",
                                   content = list(plotlyOutput("dctRNA_boxplot", width = 1000, height = 300))),
                              list(menu = "Isodecoders",
                                   content = list(plotlyOutput("dccodon_boxplot", width = 1000, height = 300))),
                              list(menu ="Isoacceptors",
                                   content = list(plotlyOutput("dcaa_boxplot", width = 1000, height = 300)))))),
                     column(width= 16, h3(shiny::tags$div(HTML('<i class="fas fa-caret-right" style = "color:black;"></i> Expression patterns across groups in projects')))),
                     column(width=16,
                            selectInput(width = '100%', "tproject", label = div(style = "font-size:15px;","Select Project"),
                                        choices = unique(metadata$select))
                            ),# project -(p)-------------
                     column(width = 16,
                            tabBox(width = 16, color = 'red', tabs = list(
                              list(menu="Individual tRNAs",
                                   content = list(plotlyOutput("ptRNA_boxplot", width = 1000, height = 300))),
                              list(menu="Isodecoders",
                                   content= list(plotlyOutput("pcodon_boxplot", width = 1000, height = 300))),
                              list(menu="Isoacceptors",
                                   content = list(plotlyOutput("paa_boxplot", width = 1000, height= 300)))
                            )))
                     )
    ),
    tabItem(tabName = "Download",fluidRow(h5("Under Construction"), style = "height:1000px")),
    # tabItem(tabName = "Guide", fluidRow(h5("Under Construction"), style = "height:1000px")),
    tabItem(tabName = "Guide",fluidRow(column(16,align="center",#style = "height:1000px",
                     div(shiny::tags$img(src="userguide.png", width = "1080"), align="center")))),
    tabItem(tabName = "Contact",fluidRow(column(width = 16,
                                                includeHTML("/data11/jinoklee/trend/app/www/contact.html")), style = "height:1000px")))
  )



  

# ----------------
ui <- shiny::tagList(
  shiny::tags$style("html,body{background-color: white;}
                .container{
                    width: 100%;
                    margin: 0 auto;
                    padding: 0;
                }
                #myimg{
                    width:30%;
                }
               @media screen and (min-width: 1400px){
                .container{
                    width: 1400px;
                }
               }"),
  shiny::tags$div(class="container",
           dashboardPage(HEADER,
                         SIDEBAR,
                         BODY)
  )
)

