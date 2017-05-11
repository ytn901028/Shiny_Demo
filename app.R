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
  #fluidRow() adds rows to the grid. Each new row goes below the previous row.
  #column(width, offset)
  #can use panels to group elements
   
   
   
   #tabsetPanel(
  navlistPanel(
     
   tabPanel("Overwatch",
   column(width=5,
          offset=5,
          tags$p(tags$em(tags$strong("Test Image Placement Here")),
                 tags$br(),
                 #https:// image urls won't display in the output of the app
                 tags$img(height=100,
                          width=100,
                          src="http://us.battle.net/forums/static/images/social-thumbs/overwatch.png"),
                 tags$img(height=100,
                          width=100,
                          src="mercy.jpg")
                 )
          )),
   
   tabPanel("Test File UPload",
            titlePanel("Test Page For File Upload"),
            
            tags$p("Please Find", 
                   tags$a(href="https://shiny.rstudio.com/tutorial/", 
                          tags$strong("Shiny Tutorial Link Here"))),
            textInput(inputId = "intext", 
                      label= "Description of the Input Dataset",
                      value = "This is a test data set",
                      placeholder="Write your description here"),
            fileInput(inputId = "file", 
                      label = "Choose File to Upload",
                      multiple = TRUE,
                      buttonLabel = "Choose",
                      placeholder = "No file chosen"),
            checkboxInput(inputId="confirm_upload",
                          label = "I agree to upload the files on this page"),
            actionButton(inputId="upload", label = "Upload"),
            dataTableOutput("table"),
            textOutput("outtext"),
            verbatimTextOutput("summary"),
            actionButton(inputId="click", label = "Click Here To View Summary Stats"),
            dateInput(inputId = "calendar", label = "Choose the date to upload")),
   tabPanel("Normal vs Uniform",
            actionButton(inputId="normal", label = "Normal Plot"),
            actionButton(inputId = "uniform", label = "Uniform Plot"),
            plotOutput("hist"))
   )
   
          
  
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Reactive Expression to avoid inconsistency
  # Call the object like calling a function in R
  # Can build another reactive object by calling existing ones
  inFile <- reactive({input$file})
  path <- reactive({inFile()$datapath})
  data <- reactive({read.csv(path())})
  
  # isolate() example
  # isolate() returns a non-reactive value
  fix_text <- isolate({input$intext})
  #output$outtext <- renderText({fix_text}) # If using this line, the outtext won't change no matter what I type
  
  output$outtext <- renderText({input$intext})
  
  # Using eventReactive() to delay actions
  # eventReactive() is a REACTIVE EXPRESSION that only responds to specific values
  upload_data <- eventReactive(input$upload,{data()})

  
  output$table <- renderDataTable({
    if (is.null(inFile()))
      return(NULL)
    #data()         ## using data() only when you want to automatically pop up the table
    upload_data()})  ## using upload_data() when you want to upload until click the button
  
  output$summary <- renderPrint({
    if (is.null(inFile()))
      return(NULL)
    summary(data())})
  
  #observeEvent() to trigger code to run on server
  # need the print() function to actually showing the output
  observeEvent(input$click, {
    if (is.null(inFile()))
      return(NULL)
    print(summary(data()))
    print(as.numeric(input$click))})
  
  # reactiveValues() creates a list of reactive values to manipulate programmatically
  # can assign reactive values
  rv <- reactiveValues(data = rnorm(100))
  observeEvent(input$normal, {rv$data <- rnorm(100)})
  observeEvent(input$uniform, {rv$data <- runif(100)})
  output$hist <- renderPlot({hist(rv$data)})
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

