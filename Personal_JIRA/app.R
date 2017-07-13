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



library(shinyjs)



# Define UI for application that draws a histogram
if (interactive()) {
  library(shiny)
  library(ReporteRs)

  ui <- fluidPage(
    useShinyjs(),
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
             actionButton('create','Create'),
             actionButton('clear', 'Clear')
      ),
      tabPanel("Open_tickets",
             dateInput('date',"Date:"),
             tableOutput("open_tickets"),
             actionButton('close', 'Close Tickets')
      ),
      tabPanel("Closed Tickets",
             dateInput('date',"Date:"),
             dataTableOutput("closed_tickets")
      )
    )
  )

# Define server logic required to draw a histogram
  server <- function(input, output, session) {
    
     
    
     summary <- reactive({input$summary})
     description <- reactive({input$description})
     point <- reactive({input$point})
     ticket_number <- reactive({input$create})
     status <- reactive({input$status})
     df <- reactiveValues()
     df$open_data <- data.frame(Select=NA, 
                                Ticket_Number=NA,
                                Summary=NA,
                                Description=NA,
                                Priority=NA,
                                Point=NA)
     
     observeEvent(input$create,
                               {  
                                   select <- paste0('<label><input type="checkbox" id="check',ticket_number(),'"></label>')
                                   new_ticket <- isolate(c(select, input$create, input$summary, input$description, input$priority, input$point))
                                   isolate(df$open_data <- data.frame(rbind(df$open_data, new_ticket), stringsAsFactors = FALSE))
                                   
                              
                               })
   
  
     observeEvent(input$close,
                              { 
                                if (is.na(df$open_data[1,1]))
                                    df$open_data <- df$open_data[-1,]
                                res <- unlist(lapply(1:nrow(df$open_data), 
                                                             function(i) input[[paste0("check", df$open_data[i,2])]]))
                                closed_ticket <- df$open_data[res,]
                                closed_ticket <- subset(closed_ticket, select = seq(2,6))
                                isolate(df$closed_data <- data.frame(rbind(df$closed_data, closed_ticket),
                                                           stringsAsFactors = FALSE))
        
                                isolate(df$open_data <- df$open_data[!res, ])
                                if (nrow(df$open_data) == 0)
                                  df$open_data <- isolate(data.frame(Select=NA, 
                                             Ticket_Number=NA,
                                             Summary=NA,
                                             Description=NA,
                                             Priority=NA,
                                             Point=NA))
                                  
                              })
     

   
     output$open_tickets <- renderFlexTable({
       if(is.null(df$open_data))
         return(NULL)
       temp <- df$open_data

       clean_ticket <- vanilla.table(temp)
       clean_ticket[, "Select", to = "header"] <- parLeft()
       clean_ticket[, "Select"] <- parCenter()
       clean_ticket[, "Ticket_Number"] <- parCenter()
       clean_ticket[, "Summary"] <- parCenter()
       clean_ticket[, "Description"] <- parCenter()
       clean_ticket[, "Priority"] <- parCenter()
       clean_ticket[, "Point"] <- parCenter()
       return(clean_ticket)
     
     }
     )
     

     output$closed_tickets <- renderDataTable({
        if(is.null(df$closed_data))
          return(NULL)
        unique(df$closed_data)
     
     })
  
  
     session$onSessionEnded(stopApp)
  }
}
# Run the application 
shinyApp(ui = ui, server = server)

