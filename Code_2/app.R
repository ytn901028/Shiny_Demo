#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
require(gridExtra)
require(maps)
library(mapproj)
library(ggplot2)
library(shinyjs)

### Global Variables ###

state_map <- map_data("state")

state_map$region <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", state_map$region, perl=TRUE)

state_map$Abb <- state.abb[match(state_map$region,state.name)]



# Define UI for application that draws a histogram
ui <- fluidPage(
   navlistPanel(
     tabPanel("File Upload",
              fileInput('file', 
                        label = 'Choose Your File To Upload',
                        buttonLabel = 'Choose'),
              checkboxInput('header', 'Header'),
              radioButtons('sep',
                           'Seperator',
                           c("Comma"=',',
                             "Tab"='\t',
                             "Semicolon"=';',
                             "Pipe"='|')),
              checkboxInput('confirm',
                            'I agree to upload my file on this website'),
              actionButton('upload','Upload'),
              actionButton('columns', "Click to View Columns"),
              dataTableOutput("table"),
              textOutput('columns')
              ),
     tabPanel("Data Visualization",
              selectInput("year",
                          "Choose a Year",
                          list (`2013` = 2013,
                                `2014` = 2014,
                                `2015` = 2015)),
              selectInput("type",
                          "Choose a Year",
                          list ("All Homes" = 'Total_All_Home',
                                "REO" = 'Total_REO_Sale',
                                "TPS" = 'Total_TPS',
                                "Completed Foreclosure" = 'Total_Completed_FCL')),
              plotOutput("map",
                         height = 500
                         ),
              numericInput("width",
                           "Choose the width of your downloaded image",
                           value=10,
                           min =5,
                           max = 50),
              numericInput("height",
                           "Choose the height of your downloaded image",
                           value=10,
                           min =5,
                           max = 50),
              downloadButton(outputId= 'downloadplot', label = 'Download Image'),
              dataTableOutput("reo_table")
              ),
     tabPanel("Interactive Visualization",
              
              plotOutput("plot2",
                         height = 500,
                         click = 'plot2_click',
                         brush = brushOpts(
                           id = 'plot2_brush',
                           resetOnNew = TRUE
                         ))
              )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   ranges <- reactiveValues(x = NULL, y = NULL)
   inFile <- reactive({input$file})
   file_path <- reactive({inFile()$datapath})
   file_name <- reactive({inFile()$name})
   content <- reactive({read.csv(file_path(), sep = input$sep, header=input$header)})
   upload_data <- eventReactive(input$upload, 
                                {if (input$confirm == TRUE)
                                  content()
                                })
   file_columns <- eventReactive(input$columns,
                                 {if (is.null(inFile()))
                                    return(NULL)
                                  colnames(upload_data())
                                 })
   year_data <- reactive({data.table(subset(upload_data(), !is.na(Year) & Year == input$year))})
   
   agg_year_data <- reactive({
     year_data()[order(State),list(Total_All_Home = sum(All_Home_Sales),
                                 Total_Non_FCL = sum(Non.Foreclosure.Sales),
                                 Total_Completed_FCL = sum(Completed.Foreclosures),
                                 Total_TPS = sum(Pre.Foreclosure.Sales..Short.Sales..TPS),
                                 Total_FCL_AS_REO = sum(Foreclosures.reported.as.REO),
                                 Total_REO_Sale = sum(REO.Sales),
                                 Avg_BTA_Share = mean(BTA_Market.Share,na.rm=T),
                                 Avg_TPS_Share = mean(TPS_Market.Share,na.rm=T),
                                 Avg_Rvt_Share = mean(Reverts_Market.Share,na.rm=T)),
                by=State]
                        }
                      )
   agg_data <- reactive({
     merge(state_map ,agg_year_data(),by.x = "region", by.y = "State", all.x = T)
   })
   
   
   final_map <- function(){
     ggplot(state_map, aes(map_id = region)) +
       geom_map(fill="white", map=state_map, color="black") +
       geom_map(data=agg_data(), aes(fill = agg_data()[input$type]), map = state_map, color ="black") +
       expand_limits(x = state_map$long, y = state_map$lat) +
       coord_map(projection="mercator") +
       theme(legend.position = "bottom",
             axis.ticks = element_blank(), 
             axis.title = element_blank(), 
             axis.text =  element_blank()) +
       scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint=20000) +
       guides(fill = guide_colorbar(barwidth = 20, barheight = .5)) + 
       theme(panel.background=element_rect(fill='transparent',color=NA),plot.background=element_rect(fill='transparent',colour=NA),panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
       ggtitle(paste(input$type, "By State", collapse=' '))
   }
   
   
   
   output$table <- renderDataTable({
     if (is.null(inFile()))
       return(NULL)
     upload_data()
   })
   
   output$columns <- renderPrint({file_columns()})
   
   
   output$map <- renderPlot({
     final_map()
   })
   
   dim <- reactiveValues()
   
   output$downloadplot <- downloadHandler(
     filename = function(){
       paste0(input$type,'_',input$year,'.png')
       },
     content = function(file) {
       device <- function(...,width,height){
         grDevices::png(...,width=width,height=height,res=300,units='in')
       }
       ggsave(file,plot=final_map(),height=input$height,width=input$width,device=device)
     }
   )
   
   output$reo_table <- renderDataTable({
     if (is.null(inFile()))
       return(NULL)
     agg_data()
   })
   
   output$plot2 <- renderPlot({
     ggplot(mtcars, aes(wt, mpg)) +
     geom_point() +
     coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand=FALSE)
   })
   
   
   observeEvent(input$plot2_click, {
     brush <- input$plot2_brush
     if (!is.null(brush)) {
       ranges$x <- c(brush$xmin, brush$xmax)
       ranges$y <- c(brush$ymin, brush$ymax)
       
     } else {
       ranges$x <- NULL
       ranges$y <- NULL
     }
   })
   
   session$onSessionEnded(stopApp)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

