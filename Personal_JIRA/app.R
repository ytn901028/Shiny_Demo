#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# https://stackoverflow.com/questions/19042192/checkbox-on-table-or-dataframe (adding checkbox)

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_131')
#Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_131')

library(shiny)
library(ReporteRs)



# Define UI for application that draws a histogram
ui <- fluidPage(
   
  navlistPanel(
    tabPanel("Create Ticket",
             textInput('summary',label='Summary'),
             selectInput("priority",
                         "Priority",
                         list (`High` = "High",
                               `Medium` = "Medium",
                               `Low` = "Low")),
             textInput('description',label='Description of the Ticket'),
             textInput('point', label = 'Point of the Ticket'),
             #checkboxInput("confirm_creation", "Confirm to create ticket"),
             actionButton('create','Create'),
             actionButton('clear', 'Clear')
    ),
    tabPanel("Open Tickets",
             tableOutput("open_tickets"),
             actionButton('close', 'Close Tickets')
    ),
    tabPanel("Closed Tickets",
             
             dataTableOutput("closed_tickets")
             #textOutput("closed_tickets")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   summary <- reactive({input$summary})
   description <- reactive({input$description})
   point <- reactive({input$point})
   ticket_number <- reactive({input$create})
   df <- reactiveValues(open_data=NULL, closed_data = NULL)
   
   
   open_list <- eventReactive(input$create,
                               {   #ticket_number <-  paste0('<label><input type="checkbox" id="check"', 
                                  #                          ticket_number(), 
                                  #                          '"> <span>', 
                                  #                          ticket_number(), 
                                  #                          '</span></label>')
                                #if (input$confirm_creation)
                                   check <- paste0('<label><input type="checkbox" id="check',ticket_number(),'"></label>')
                                   new_ticket <- c(ticket_number(), summary(), description(), input$priority, point(), check)
                                   df$open_data <- data.frame(rbind(df$open_data, new_ticket),
                                                         stringsAsFactors = FALSE)
                                   setNames(df$open_data, c("Ticket_Number","Summary","Description","Priority","Point", "Check"))
                                   
                                   
                                   
                              
                               })
   
   
   closed_list <- eventReactive(input$close,
                              { 
                                #res <- unlist(lapply(1:nrow(open_list()),
                                #                     function(i) input[[paste0("check", open_list()[i,1])]]))
                                
                                
                                res <- unlist(lapply(1:nrow(df$open_data),
                                                                    function(i) input[[paste0("check", df$open_data[i,1])]]))

                                
                                
                                closed_ticket <- open_list()[res,]
                                closed_ticket <- subset(closed_ticket, select = seq(1,5))
                                df$closed_data <- data.frame(rbind(df$closed_data, closed_ticket),
                                                           stringsAsFactors = FALSE)
                                setNames(df$closed_data, c("Ticket_Number","Summary","Description","Priority","Point"))
                                
                                
                                
                              })
   



   
   
   output$open_tickets <- renderFlexTable({
      if(is.null(open_list()))
       return(NULL)
     temp <- open_list()
     res <- unlist(lapply(1:nrow(df$open_data),
                         function(i) input[[paste0("check", df$open_data[i,1])]]))
     if (any(res) & input$close > 0)
         temp <- temp[!res,]
     clean_ticket <- vanilla.table(temp)
     clean_ticket[, "Check", to = "header"] <- parLeft()
     clean_ticket[, "Check"] <- parCenter()
     clean_ticket[, "Ticket_Number"] <- parCenter()
     clean_ticket[, "Summary"] <- parCenter()
     clean_ticket[, "Description"] <- parCenter()
     clean_ticket[, "Priority"] <- parCenter()
     clean_ticket[, "Point"] <- parCenter()
     return(clean_ticket)
     
   }
   )
     

   output$closed_tickets <- renderDataTable({
      if(is.null(closed_list()))
        return(NULL)
      unique(closed_list())
     
   })
  
  
   session$onSessionEnded(stopApp)
}

# Run the application 
shinyApp(ui = ui, server = server)

