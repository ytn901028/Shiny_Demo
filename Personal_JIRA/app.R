#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  navlistPanel(
    tabPanel("Ticket Creation",
             textInput('summary',label='Summary'),
             selectInput("priority",
                         "Priority",
                         list (`High` = "High",
                               `Medium` = "Medium",
                               `Low` = "Low")),
             textInput('description',label='Description of the Ticket'),
             textInput('point', label = 'Point of the Ticket'),
             actionButton('create','Create'),
             actionButton('clear', 'Clear')
    ),
    tabPanel("Open Tickets",
             dataTableOutput("open_tickets")
    ),
    tabPanel("Closed Tickets",
             
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
   summary <- reactive({input$summary})
   description <- reactive({input$description})
   point <- reactive({input$point})
   df <- reactiveValues(data=NULL)
   open_list <- eventReactive(input$create,
                              {
                                  new_ticket <- c(summary(), description(), input$priority, point())
                                  df$data <- data.frame(rbind(df$data, new_ticket),
                                                        stringsAsFactors = FALSE)
                                  setNames(df$data, c("Summary","Description","Priority","Point"))
                                   
                              })
   output$open_tickets <- renderDataTable({
     if(is.null(open_list()))
       return(NULL)
     open_list()
   }
   )
  
   session$onSessionEnded(stopApp)
}

# Run the application 
shinyApp(ui = ui, server = server)

