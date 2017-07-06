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
   new_ticket <- reactiveValues(df=NULL, summary=NULL, description=NULL, point=NULL)
   #open_list <- eventReactive(input$create,
   #                            {
   #                            new_ticket <- c(summary(), description(), priority, point())
   #                            df <- rbind(df, new_ticket)
   #                          })
   observeEvent(input$clear,
                {
                  input$summary <- NULL
                  input$description <- NULL
                  input$point <- NULL
                })
  
   session$onSessionEnded(stopApp)
}

# Run the application 
shinyApp(ui = ui, server = server)

