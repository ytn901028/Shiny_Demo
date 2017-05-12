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
          tags$p(tags$em(tags$strong("Overwatch Icon")),
                 tags$br(),
                 #https:// image urls won't display in the output of the app
                 tags$img(height=100,
                          width=100,
                          src="http://us.battle.net/forums/static/images/social-thumbs/overwatch.png"),
                 tags$img(height=100,
                          width=100,
                          src="http://images.pushsquare.com/news/2016/05/play_overwatch_early_and_win_the_game_on_the_ps4/attachment/0/original.jpg")
                 ),
          tags$p(tags$em(tags$strong("Mercy")),
                 tags$br(),
                 #https:// image urls won't display in the output of the app
                 tags$img(height=100,
                          width=100,
                          src="mercy.jpg"),
                 tags$img(height=100,
                          width=100,
                          src="mercy_2.jpg")
          ),
          
          tags$p(tags$em(tags$strong("Genji")),
                 tags$br(),
                 #https:// image urls won't display in the output of the app
                 tags$img(height=100,
                          width=100,
                          src="genji.jpg"),
                 tags$img(height=100,
                          width=100,
                          src="genji_2.jpg")
          ),
          tags$p(tags$em(tags$strong("Bastion")),
                 tags$br(),
                 #https:// image urls won't display in the output of the app
                 tags$img(height=100,
                          width=100,
                          src="http://i2.wp.com/www.cgmeetup.net/home/wp-content/uploads/2016/08/Overwatch-The-Last-Bastion-Animated-Short-7.jpg"),
                 tags$img(height=100,
                          width=100,
                          src="http://assets.vg247.com/current//2015/06/overwatch_bastion.jpg")
          ),
          tags$p(tags$em(tags$strong("Mei")),
                 tags$br(),
                 #https:// image urls won't display in the output of the app
                 tags$img(height=100,
                          width=100,
                          src="http://cdn2.vox-cdn.com/uploads/chorus_asset/file/7867475/mei_christmas.jpg"),
                 tags$img(height=100,
                          width=100,
                          src="http://orig13.deviantart.net/0c3b/f/2016/176/f/4/mei_by_pmolita-da7lgb2.png")
          )
          )),
   
   tabPanel("Test File UPload",
            sidebarLayout(
              sidebarPanel(textInput(inputId = "intext", 
                                     label= "Description of the Input Dataset",
                                     value = "This is a test data set",
                                     placeholder="Write your description here"),
                           fileInput(inputId = "file", 
                                     label = "Choose File to Upload",
                                     multiple = TRUE,
                                     buttonLabel = "Choose",
                                     placeholder = "No file chosen"),
                           checkboxInput(inputId="confirm_upload",
                                         label = "I agree to upload the files on this page",
                                         value = FALSE),
                           actionButton(inputId="upload", label = "Upload"),
                           dateInput(inputId = "calendar", label = "Choose the date to upload"),
                           downloadButton("download","Download")),
              mainPanel(titlePanel("Test Page For File Upload"),
                        tags$p("Please Find", 
                               tags$a(href="https://shiny.rstudio.com/tutorial/", 
                                      tags$strong("Shiny Tutorial Link Here"))),
                        dataTableOutput("table"),
                        textOutput("outtext"),
                        verbatimTextOutput("summary"),
                        actionButton(inputId="click", label = "Click Here To View Summary Stats")
                        )
                       )
            ),
            
   tabPanel("Normal vs Uniform",
            sidebarLayout(
              sidebarPanel(
                actionButton(inputId="normal", label = "Normal Plot"),
                actionButton(inputId = "uniform", label = "Uniform Plot")
                
              ),
              mainPanel(plotOutput("hist"))
           )
   )
   )
)
   
          
  

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Reactive Expression to avoid inconsistency
  # Call the object like calling a function in R
  # Can build another reactive object by calling existing ones
  inFile <- reactive({input$file})
  path <- reactive({inFile()$datapath})
  file_name <- reactive({inFile()$name})
  data <- reactive({read.csv(path())})
  
  
  # isolate() example
  # isolate() returns a non-reactive value
  fix_text <- isolate({input$intext})
  #output$outtext <- renderText({fix_text}) # If using this line, the outtext won't change no matter what I type
  
  output$outtext <- renderText({input$intext})
  
  # Using eventReactive() to delay actions
  # eventReactive() is a REACTIVE EXPRESSION that only responds to specific values
  upload_data <- eventReactive(input$upload,
                               {
                                 if (input$confirm_upload == TRUE)
                                   data()})

  
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
  
  output$download <- downloadHandler(
    filename = function(){
      paste(file_name(),'.csv', sep='')
    },
    content = function(file) {
      write.csv(data(), file, row.names=FALSE, quote=FALSE)
    }
  )
  
  # reactiveValues() creates a list of reactive values to manipulate programmatically
  # can assign reactive values
  rv <- reactiveValues(data = rnorm(100),title="Let's Decide What Distribution We Want")
  observeEvent(input$normal, {rv$data <- rnorm(100)})
  observeEvent(input$normal, {rv$title <- "Histogram of Normal Distribution"})
  observeEvent(input$uniform, {rv$data <- runif(100)})
  observeEvent(input$uniform, {rv$title <- "Histogram of Uniform Distribution"})
  output$hist <- renderPlot({hist(rv$data, main=rv$title)})
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

