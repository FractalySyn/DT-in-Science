# LIBS ----

rm(list = ls())

library(bs4Dash)
library(shiny)
library(shinyWidgets)
library(tools)
library(htmltools)
library(htmlwidgets)
library(tidyverse)
library(shinycssloaders)
library(png)
library(sjmisc)
library(magrittr)

# PATH ------

dir <- "plots/"

opt <- read.delim("data/options.txt") #location of the options.txt file

# DATA ----

years <- seq(2000,2019)
years_2 <- seq(2000,2020)
years_3 <- seq(2000,2018)


authors <- opt[1,]

inventors <- opt[3,]

publishers <- opt[5,]

DT <- opt[9,]

ST <- opt[11,]

countries <- opt[13,]


authors <- as.data.frame(strsplit(authors,split = ",", fixed = T),col.names = c ("authors")) %>% mutate_if(is.character, str_trim)
inventors <- as.data.frame(strsplit(inventors,split = ",", fixed = T),col.names = c("inventors")) %>% mutate_if(is.character, str_squish) %>% distinct()
publishers <- as.data.frame(strsplit(publishers,split = ",", fixed = T), col.names = c("publishers")) %>% mutate_if(is.character, str_trim)
DT <- as.data.frame(strsplit(DT,split = ",", fixed = T), col.names = c("DT")) %>% mutate_if(is.character, str_squish)
ST <- as.data.frame(strsplit(ST,split = ",", fixed = T), col.names = c("ST")) %>% mutate_if(is.character, str_squish)
countries <- as.data.frame(strsplit(countries,split = ",", fixed = T), col.names = c("countries")) %>% mutate_if(is.character, str_squish)

popularity_lv <- c("less popular","little popular","moderately popular","most popular")


# UI -----


ui <- bs4DashPage(
  
  dashboardHeader(
    
    title = "DT in Science"
    
  ),
  
  bs4DashSidebar(
    
    skin = "light",
    status = "gray",
    title = "Sidebar menu",
    
    bs4SidebarMenu(
      
      bs4SidebarMenuItem(
        
        text = "Home",
        tabName = "home"
        
      ),
      
      bs4SidebarMenuItem(
        
        text = "Overview",
        tabName = "over"
        
      ),
      
      bs4SidebarMenuItem(
        
        text = "Contributions",
        tabName = "contrib"
        
      ),
      
      bs4SidebarMenuItem(
        
        text = "Evolutions",
        tabName = "evo"
        
      ),
      
      bs4SidebarMenuItem(
        
        text = "Causality",
        tabName = "causal"
        
      ),
      
      bs4SidebarMenuItem(
        
        text = "Fields and Countries",
        tabName = "ctry"
        
      )
      
    )
    
  ),
  
  body = bs4DashBody(
    
    # Networks dependencies -----
    # 
    includeScript(paste0(dir,"networks/original/lib/htmlwidgets-1.5.4/htmlwidgets.js")),
    includeScript(paste0(dir,"networks/original/lib/visNetwork-binding-2.1.0/visNetwork.js")),
    includeScript(paste0(dir,"networks/original/lib/vis-9.1.0/vis-network.min.js")),
    
    includeCSS(paste0(dir,"networks/original//lib/vis-9.1.0/vis-network.min.css")),
    
    # contrib dependencies -----
    
    includeScript(paste0(dir,"contrib/lib/crosstalk-1.2.0/js/crosstalk.js")),
    includeScript(paste0(dir,"contrib/lib/crosstalk-1.2.0/js/crosstalk.min.js")),
    
    includeScript(paste0(dir,"contrib/lib/plotly-binding-4.10.0/plotly.js")),
    includeScript(paste0(dir,"contrib/lib/plotly-main-2.5.1/plotly-latest.min.js")),
    includeScript(paste0(dir,"contrib/lib/typedarray-0.1/typedarray.min.js")),
    
    includeCSS(paste0(dir,"contrib/lib/plotly-htmlwidgets-css-2.5.1/plotly-htmlwidgets.css")),
    
    # evo dependencies ----
    
    includeCSS(paste0(dir,"evo/lib/plotly-htmlwidgets-css-2.5.1/plotly-htmlwidgets.css")),
    
    
    
    bs4TabItems(
      
      bs4TabItem(
        
        tabName = "home",
        h3("Digital Topics in Science and Industry"),
        p("We aim through this dashboard to make available some insights on the propagation of digital technologies in research - both in science and in the industry."),
        h4("Contributors"),
        p("G. Burdloff, C. Lobet, R. Mondjehi, V. Schott"),
        tags$a(href="https://github.com/FractalySyn/DT-in-Science/tree/main/app%20autarcy/plots", "Github Repository"),
        p(), h4("Presentation"),
        p("This work has been performed with two datasets. The first is composed of scientific articles metadata that include digital keywords, either as a research target or as a tool for applied research. The second is the corollary for industrial research i.e. made of patents metadata. Data have been provided by M. Müller and S. Bianchini - BETA Strasbourg. Scientific papers have been obtained from Clarivate (Web of Science)."),
        h4("Known Issues / Further Improvements"),
        tags$ol(
           tags$li("In Contribution and Evolution tabs,  make the plot picker more user-friendly, i.e. get rid of choosing the plot by its filename."), 
           tags$li("Include the light versions of Overview networks (these versions exist and can be found in the Github repository). See if it loads significantly faster compared to original networks."), 
           tags$li("Manage plots dimensions. Right now the dashboard is well displayed on a 1080p format but without borders (the browser takes some place so we lack space). For now this issue can be solved by zooming out the web page to about 80%."),
           tags$li("In the Fields and Countries tab - in the sub-tabs Topics and World - the user has to manually select an option for the visualization to be loaded. The default plot does not load automatically."),
           tags$li("In the same tab, nodes names are quite small so the user has to zoom in to see them."),
           tags$li("In the Contributions tab, for inventors plots, several are not working because of path specification issues.")
        )
        
      ),
      
      bs4TabItem(
        
        tabName = "over",
        
        fluidRow(
          
          tabBox(
            
            id = "overview_tab",
            selected = "Networks",
            status = "gray",
            solidHeader = F,
            width = 12,
            collapsible = F,
            closable = F,
            type = "tabs",
            
            tabPanel(
              
              title = "Networks",
              
              fluidRow(
                
                column(width = 3,
                       
                       bs4Card(
                         
                         width = 12,
                         title = "Network Tuner",
                         status = "gray",
                         closable = F,
                         collapsible = F,
                         solidHeader = T,
                         
                         br(),
                         
                         radioGroupButtons(
                           
                           inputId = "network_btn",
                           choices = c("Science" = "papers", 
                                       "Industry" = "patents",
                                       "Both" = "all"),
                           
                           direction = "vertical",
                           justified = T,
                           checkIcon = list(
                             yes = tags$i(class = "fa fa-check-square", 
                                          style = "color: steelblue"),
                             no = tags$i(class = "fa fa-square-o", 
                                         style = "color: steelblue"))
                         ),
                         
                         br(),
                         
                         pickerInput(
                           inputId = "net_year_picker",
                           label = "Select a Year", 
                           choices = years,
                           options = list(
                             `live-search` = TRUE)
                         )
                         
                         
                       )
                       
                ),
                
                column(width = 9,
                       
                       uiOutput("network_card")
                       
                )
                
              )
              
            ),
            
            
            tabPanel(
              
              title = "Wordcloud",
              
              fluidRow(
                
                column(width = 3,
                       
                       bs4Card(
                         
                         width = 12,
                         title = "Wordcloud Tuner",
                         status = "gray",
                         closable = F,
                         collapsible = F,
                         solidHeader = T,
                         
                         br(),
                         
                         radioGroupButtons(
                           
                           inputId = "wc_btn",
                           
                           choices = c("Science" = "papers", 
                                       "Industry" = "patents",
                                       "Both" = "all"),
                           
                           direction = "vertical",
                           justified = T,
                           checkIcon = list(
                             yes = tags$i(class = "fa fa-check-square", 
                                          style = "color: steelblue"),
                             no = tags$i(class = "fa fa-square-o", 
                                         style = "color: steelblue"))
                         ),
                         
                         br(),
                         
                         uiOutput("if_both_btn"),
                         
                         br(),
                         
                         pickerInput(
                           inputId = "wc_year_picker",
                           label = "Select a Year", 
                           choices = years_2,
                           options = list(
                             `live-search` = TRUE)
                         )
                         
                         
                       )
                       
                ),
                
                column(width = 9,
                       
                       bs4Card(
                         
                         width = 12,
                         # title = "Wordcloud displayed here",
                         status = "white",
                         closable = F,
                         solidHeader = F,
                         collapsible = F,
                         
                         imageOutput("wordcloud_img")
                         
                         
                       )
                       
                       
                )
                
              )
              
            )
            
          )
          
        )
        
      ),
      
      bs4TabItem(
        
        tabName = "contrib",
        
        fluidRow(
          
          tabBox(
            id = "tabcard1",
            selected = "Contribution plots",
            status = "gray",
            solidHeader = F,
            width = 12,
            closable = F,
            collapsible = F,
            type = "tabs",
            
            tabPanel(
              title = "Contribution plots",
              
              fluidRow(
                
                column(width = 3,
                       
                       bs4Card(
                         
                         width = 12,
                         title = "Contribution Tuner",
                         status = "gray",
                         closable = F,
                         collapsible = F,
                         solidHeader = T,
                         
                         br(),
                         
                         radioGroupButtons(
                           
                           inputId = "contrib_btn",
                           choices = c("author", 
                                       "inventor", "publisher"),
                           direction = "vertical",
                           justified = T,
                           checkIcon = list(
                             yes = tags$i(class = "fa fa-check-square", 
                                          style = "color: steelblue"),
                             no = tags$i(class = "fa fa-square-o", 
                                         style = "color: steelblue"))
                         ),
                         
                         br(),
                         
                         pickerInput(
                           inputId =   "name_picker",
                           label = "Choose a contributor",
                           choices = NULL,
                           options = list(
                             `live-search` = TRUE
                           )
                           
                           
                           
                         ),
                         
                         br(),
                         
                         pickerInput(
                           inputId = "contrib_plot_picker",
                           label = "Select a plot to display",
                           choices = NULL,
                           options = list(
                             `live-search` = TRUE
                           )
                           
                           
                         )
                         
                       )
                       
                ),
                
                column(width = 9,
                       
                       uiOutput("contrib_card")
                       
                )
                
              )
              
              
              
            )
            
            
          )
          
        )
        
        
      ),
      
      bs4TabItem(
        
        tabName = "evo",
        
        fluidRow(
          
          tabBox(
            id = "tabcard2",
            selected = "Digital Topics Popularity",
            status = "gray",
            solidHeader = F,
            width = 12,
            closable = F,
            collapsible = F,
            type = "tabs",
            
            tabPanel(
              title = "Digital Topics Popularity",
              
              fluidRow(
                
                column(
                  
                  width = 3,
                  
                  bs4Card(
                    
                    width = 12,
                    solidHeader = T,
                    closable = F,
                    status = "gray",
                    collapsible = F,
                    title = "Plot Tuner",
                    
                    pickerInput(
                      
                      inputId = "DT_popularity",
                      label = "Popularity level(s)",
                      choices = c("less popular","little popular","moderately popular","most popular"),
                      selected = c("less popular","little popular","moderately popular","most popular"),
                      multiple = T,
                      options = list(
                        `live-search` = T,
                        `actions-box` = T,
                        `selected-text-format` = "count > 3"
                        
                      )
                    ),
                    
                    pickerInput(
                      
                      inputId = "DT_topics",
                      label = "Select a Digital Topic",
                      choices = DT,
                      selected = NULL,
                      multiple = F,
                      options = list(
                        `live-search` = T
                        
                      )
                    ),
                    
                    pickerInput(
                      
                      inputId = "DT_plot_to_display",
                      label = "Select a plot to display",
                      choices = NULL,
                      # selected = NULL,
                      multiple = F,
                      options = list(
                        `live-search` = T
                        
                      )
                    )
                    
                  )
                  
                ),
                
                column(
                  
                  width = 9,
                  
                  bs4Card(
                    
                    width = 12,
                    # title = "Plot -> Selected plot name here",
                    closable = F,
                    solidHeader = F,
                    collapsible = F,
                    status = "white",
                    
                    htmlOutput("evo_DT_plot")
                    
                  )
                  
                )
                
              )
              
              
              
            ),
            
            tabPanel(
              title = "Digital Topics in Scientific Fields",
              
              fluidRow(
                
                column(
                  
                  width = 3,
                  
                  bs4Card(
                    
                    width = 12,
                    solidHeader = T,
                    closable = F,
                    status = "gray",
                    collapsible = F,
                    title = "Plot Tuner",
                    
                    pickerInput(
                      
                      inputId = "ST_popularity",
                      label = "Popularity level(s)",
                      choices = c("less popular","little popular","moderately popular","most popular"),
                      selected = c("less popular","little popular","moderately popular","most popular"),
                      multiple = T,
                      options = list(
                        `live-search` = T,
                        `actions-box` = T,
                        `selected-text-format` = "count > 3"
                        
                      )
                    ),
                    
                    pickerInput(
                      
                      inputId = "ST_topics",
                      label = "Select a Scientific Field",
                      choices = ST,
                      selected = NULL,
                      multiple = F,
                      options = list(
                        `live-search` = T
                        
                      )
                    ),
                    
                    pickerInput(
                      
                      inputId = "ST_plot_to_display",
                      label = "Select a plot to display",
                      choices = NULL,
                      # selected = NULL,
                      multiple = F,
                      options = list(
                        `live-search` = T
                        # `actions-box` = T,
                        # `selected-text-format` = "count > 3"
                        
                      )
                    )
                    
                  )
                  
                ),
                
                column(
                  
                  width = 9,
                  
                  bs4Card(
                    
                    width = 12,
                    # title = "Plot -> Selected plot name here",
                    closable = F,
                    solidHeader = F,
                    status = "white",
                    collapsible = F,
                    
                    
                    htmlOutput("evo_ST_plot")
                    
                  )
                  
                )
                
              )
              
            )
            
          )
          
        )
        
        
      ),
      
      bs4TabItem(
        
        tabName = "causal",
        
        fluidRow(
          
          column(width = 3,
                 
                 bs4Card(
                   
                   width = 12,
                   title = "Select a domain",
                   status = "gray",
                   closable = F,
                   collapsible = F,
                   solidHeader = T,
                   
                   br(),
                   
                   radioGroupButtons(
                     
                     inputId = "causality_btn",
                     
                     choices = c("Science" = "papers", 
                                 "Industry" = "patents",
                                 "Both" = "all"),
                     
                     direction = "vertical",
                     justified = T,
                     checkIcon = list(
                       yes = tags$i(class = "fa fa-check-square", 
                                    style = "color: steelblue"),
                       no = tags$i(class = "fa fa-square-o", 
                                   style = "color: steelblue"))
                   )
                   
                   
                 )
                 
          ),
          
          column(width = 9,
                 
                 bs4Card(
                   
                   width = 12,
                   # title = "Heatmap displayed here",
                   status = "white",
                   closable = F,
                   collapsible = F,
                   solidHeader = F,
                   maximizable = T,
                   
                   
                   uiOutput("causality_img")
                   
                   
                 )
                 
                 
          )
          
        )
        
        
      ),
      
      bs4TabItem(
        
        tabName = "ctry",
        
        fluidRow(
          
          tabBox(
            
            id = "countries_tab",
            selected = "Countries",
            status = "gray",
            solidHeader = F,
            collapsible = F,
            width = 12,
            closable = F,
            type = "tabs",
            
            tabPanel(
              
              title = "Countries",
              
              fluidRow(
                
                column(width = 3,
                       
                       bs4Card(
                         
                         width = 12,
                         title = "Countries Network Tuner",
                         status = "gray",
                         closable = F,
                         collapsible = F,
                         solidHeader = T,
                         
                         br(),
                         
                         pickerInput(
                           inputId = "ctry_year_picker",
                           label = "Select a Year",
                           choices = years_3,
                           selected = NULL,
                           options = list(
                             `live-search` = TRUE)
                         )
                         
                         
                       )
                       
                ),
                
                column(width = 9,
                       
                       uiOutput("Countries")
                       
                )
                
              )
              
            ),
            
            
            tabPanel(
              
              title = "Topics",
              
              fluidRow(
                
                column(width = 3,
                       
                       bs4Card(
                         
                         width = 12,
                         title = "Country Topics Tuner",
                         status = "gray",
                         closable = F,
                         collapsible = F,
                         solidHeader = T,
                         
                         br(),
                         
                         pickerInput(
                           inputId = "ctry_picker",
                           label = "Select a Country",
                           choices = countries,
                           selected = NULL,
                           options = list(
                             `live-search` = TRUE)
                         )
                         
                         
                       )
                       
                ),
                
                column(width = 9,
                       
                       uiOutput("Topics")
                       
                )
                
              )
              
            ),
            
            tabPanel(
              
              title = "World",
              
              
              fluidRow(
                
                column(width = 3,
                       
                       bs4Card(
                         
                         width = 12,
                         title = "Main Topics Tuner",
                         status = "gray",
                         closable = F,
                         collapsible = F,
                         solidHeader = T,
                         
                         br(),
                         
                         # radioGroupButtons(
                         # 
                         #   inputId = "wc_btn",
                         # 
                         #   choices = c("Science" = "papers",
                         #               "Industry" = "patents",
                         #               "Both" = "all"),
                         # 
                         #   direction = "vertical",
                         #   justified = T,
                         #   checkIcon = list(
                         #     yes = tags$i(class = "fa fa-check-square",
                         #                  style = "color: steelblue"),
                         #     no = tags$i(class = "fa fa-square-o",
                         #                 style = "color: steelblue"))
                         # ),
                         # 
                         # br(),
                         # 
                         # uiOutput("if_both_btn"),
                         
                         br(),
                         
                         pickerInput(
                           inputId = "main_topics_year",
                           label = "Select a Year",
                           choices = years_3,
                           options = list(
                             `live-search` = TRUE)
                         )
                         
                         
                       )
                       
                ),
                
                column(width = 9,
                       
                       # bs4Card(
                       #   
                       #   width = 12,
                       #   title = "Main topics net displayed here",
                       #   status = "gray",
                       #   closable = F,
                       #   solidHeader = T,
                       #   
                       #   # uiOutput("main_topics_net")
                       #   
                       #   
                       # )
                       
                       uiOutput("World")
                       
                       
                )
                
              )
              
            )
            
          )
          
        )
        
      )
      
    )
    
  )
  
)

# SERVER -----

server <- function(input,output,session){
  
  
  output$network_card <- renderUI({
    
    bs4Card(
      
      width = 12,
      title = paste0(as.character(input$network_btn)," ",as.character(input$net_year_picker)," ","network"),
      status = "gray",
      closable = F,
      collapsible = F,
      solidHeader = T,
      
      htmltools::includeHTML(paste0(dir,
                                    "networks/original/",
                                    as.character(input$network_btn),
                                    "_",
                                    as.character(input$net_year_picker),
                                    "_",
                                    "network.html"))
      
    )
    
    
  })
  
  
  observeEvent(input$wc_btn,{
    
    output$if_both_btn <- if(input$wc_btn == "papers"){
      
      renderUI({
        
        radioGroupButtons(
          
          inputId = "wc_btn_2",
          
          choices = c("Count" = "count", 
                      "Popularity" = "popularity"),
          
          direction = "vertical",
          justified = T,
          checkIcon = list(
            yes = tags$i(class = "fa fa-check-square", 
                         style = "color: steelblue"),
            no = tags$i(class = "fa fa-square-o", 
                        style = "color: steelblue"))
        )
        
      })
      
    }else if(input$wc_btn =="patents"){
      
      renderUI({
        
        radioGroupButtons(
          
          inputId = "wc_btn_2",
          
          choices = c("Count" = "count"),
          
          direction = "vertical",
          justified = T,
          checkIcon = list(
            yes = tags$i(class = "fa fa-check-square", 
                         style = "color: steelblue"),
            no = tags$i(class = "fa fa-square-o", 
                        style = "color: steelblue"))
        )
        
      })
      
    }
    
  })
  
  
  output$wordcloud_img <- renderImage({
    
    
    if(input$wc_btn =="all"){
      
      list(src = paste0(dir,"wordclouds/wc_all_year_",input$wc_year_picker,".png"), width = 700, height = 390, style="display: block; margin-left: auto; margin-right: auto;")
      
    }else if(input$wc_btn == "papers"){
      
      list(src = paste0(dir,"wordclouds/wc_",as.character(input$wc_btn),"_",as.character(input$wc_btn_2),"_year_",as.character(input$wc_year_picker),".png"), width = 700, height = 390, style="display: block; margin-left: auto; margin-right: auto;")
      
    }else{
      
      list(src = paste0(dir,"wordclouds/wc_",as.character(input$wc_btn),"_count_year_",as.character(input$wc_year_picker),".png"), width = 700, height = 390, style="display: block; margin-left: auto; margin-right: auto;")
      
    }
    
  }, deleteFile = FALSE)
  
  
  
  
  
  observeEvent(input$contrib_btn,{
    
    updatePickerInput(
      
      getDefaultReactiveDomain(),
      "name_picker",
      choices = if(input$contrib_btn == "author"){
        authors
      }else if(input$contrib_btn == "inventor"){
        inventors
      }else{
        publishers
      }
      
    )
    
  })
  
  observeEvent(input$name_picker,{
    
    
    updatePickerInput(
      
      getDefaultReactiveDomain(),
      inputId = "contrib_plot_picker",
      choices = as.data.frame(list.files(paste0(dir,"contrib/")))%>%
        set_colnames(c("file_name"))%>%
        filter(grepl(input$name_picker,file_name)),
      options = list(
        `live-search` = TRUE
      )
      
    )
    
  })
  
  output$contrib_card <- renderUI({
    
    bs4Card(
      
      width = 12,
      # title = "Contrib plot displayed here",
      status = "white",
      closable = F,
      solidHeader = F,
      collapsible = F,
      
      
      htmltools::includeHTML(paste0(dir,"contrib/",as.character(input$contrib_plot_picker)))
      
    )
    
  })
  
  
  
  toObserve <- reactive({
    
    list(input$DT_popularity, input$DT_topics)
    
  })
  
  
  
  observeEvent(toObserve(),{
    
    
    updatePickerInput(
      
      getDefaultReactiveDomain(),
      inputId = "DT_plot_to_display",
      choices = as.data.frame(list.files(paste0(dir,"evo/")))%>%
        set_colnames(c("file_name"))%>%
        filter(grepl(input$DT_topics ,file_name))%>%
        filter(grepl(paste(as.vector(input$DT_popularity), collapse="|"), file_name)),
      options = list(
        `live-search` = TRUE
      )
      
    )
    
  })
  
  output$evo_DT_plot <- renderUI({
    
    
    if(is.null(input$DT_popularity)){
      
      validate("Please make sure to select atleast one popularity level")
      
      
    }else{
      htmltools::includeHTML(paste0(dir,"evo/",as.character(input$DT_plot_to_display)))
      
    }
    
    
    
  })
  
  toObserve_ST <- reactive({
    
    list(input$ST_popularity, input$ST_topics)
    
  })
  
  
  observeEvent(toObserve_ST(),{
    
    
    updatePickerInput(
      
      getDefaultReactiveDomain(),
      inputId = "ST_plot_to_display",
      choices = as.data.frame(list.files(paste0(dir,"evo/")))%>%
        set_colnames(c("file_name"))%>%
        filter(grepl(input$ST_topics ,file_name))%>%
        filter(grepl(paste(as.vector(input$ST_popularity), collapse="|"), file_name)),
      options = list(
        `live-search` = TRUE
      )
      
    )
    
  })
  
  output$evo_ST_plot <- renderUI({
    
    
    if(is.null(input$ST_popularity)){
      
      validate("Please make sure to select atleast one popularity level")
      
      
    }else{
      htmltools::includeHTML(paste0(dir,"evo/",as.character(input$ST_plot_to_display)))
      
    }
    
    
  })
  
  output$causality_img <- renderUI({
    
    
    if(input$causality_btn =="all"){
      
      tags$img(src = "https://raw.githubusercontent.com/gbrdf/Personnal-files/main/causality/granger_cross.jpg", width = 700, height = 390 ,style="display: block; margin-left: auto; margin-right: auto;")
      
    }else if(input$causality_btn == "papers"){
      
      tags$div(tags$img(src = "https://raw.githubusercontent.com/gbrdf/Personnal-files/main/causality/granger_papers.jpg", width = 700, height = 390, style="display: block; margin-left: auto; margin-right: auto;"),
               tags$img(src = "https://raw.githubusercontent.com/gbrdf/Personnal-files/main/causality/var_papers.jpg", width = 700, height = 390, style="display: block; margin-left: auto; margin-right: auto;"))
      
    }else{
      
      tags$div(img(src = "https://raw.githubusercontent.com/gbrdf/Personnal-files/main/causality/granger_patents.jpg", width = 700, height = 390, style="display: block; margin-left: auto; margin-right: auto;"),
               img(src = "https://raw.githubusercontent.com/gbrdf/Personnal-files/main/causality/var_patents.jpg", width = 700, height = 390, style="display: block; margin-left: auto; margin-right: auto;"))
      
    }
    
  })
  
  
  output$Countries <- renderUI({
    
    req(input$countries_tab =="Countries")
    
    bs4Card(
      
      width = 12,
      title = paste0("Countries relation in"," ",as.character(input$ctry_year_picker)),
      status = "gray",
      collapsible = F,
      closable = F,
      solidHeader = T,
      
      htmltools::includeHTML(paste0(dir,
                                    "ctry_per_years/country_network",
                                    # as.character(input$network_btn),
                                    " ",
                                    as.character(input$ctry_year_picker),
                                    # "_",
                                    ".html"))
      
      
      
    )
    
    
  })
  
  
  output$Topics <- renderUI({
    
    req(input$countries_tab =="Topics")
    
    bs4Card(
      
      width = 12,
      title = paste0("Main topics in"," ",as.character(input$ctry_picker)," ","between 2000 & 2018"),
      status = "gray",
      closable = F,
      collapsible = F,
      solidHeader = T,
      
      htmltools::includeHTML(paste0(dir,
                                    "ctry_topics/main topics network in",
                                    # as.character(input$network_btn),
                                    "  ", #double space
                                    as.character(input$ctry_picker),
                                    " ",
                                    "(2000-2018).html"))
      
      
    )
    
    
    
  })
  
  output$World <- renderUI({
    
    req(input$countries_tab =="World")
    
    bs4Card(
      
      width = 12,
      title = paste0("Main topics in the world in"," ",as.character(input$main_topics_year)),
      status = "gray",
      collapsible = F,
      closable = F,
      solidHeader = T,
      
      htmltools::includeHTML(paste0(dir,
                                    "topics_per_years/main_topics",
                                    # as.character(input$network_btn),
                                    " ", #space
                                    as.character(input$main_topics_year),
                                    ".html"))
      
      
    )
    
    
    
  })
  
  
}

shinyApp(ui,server)

