library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(rsconnect)
FL2015_34 <- read.table("ML_recruitment_output.tab", sep = "\t", header=TRUE)
headers <- c("length", "contig", "num_single_copies", "gc", "kingdom", "phylum", "class", "order", "family", "genus", "species", "taxid", "cluster", "ML_expander_clustering")
print(headers)
nms <- names(FL2015_34)

###
server <- function(input, output, session) {

dataset <- reactive({
  FL2015_34[,c(input$x, input$y, input$category, input$cov, input$gc)]
})

output$value <- renderPrint({
  str(input$file)
  print(input$category)
})
output$range <- renderPrint({ input$slider })

buildPlot <- reactive(function() {
  paste("Plot of: ", input$category)
})
mystring <- reactive({
  print(paste(input$mytext, ""))
})
coloring <- reactive({
  print(paste(input$category))
})
observe({
  # The reactive will run each time the textbox changes and
  # print results to the console.
  txt <- mystring()
  updateTextInput(session, inputId = "myresults", value = txt)
  
  
})
observeEvent( input$category, {
  #gg$color <- coloring()
})

output$myplot <- renderPlot({
  #gplot(FL2015_34, aes(bh_tsne_x, bh_tsne_y)) + geom_point(aes(color=input$category))
  ggplot(dataset(), aes_string(x = input$x, y = input$y, color = input$category)) + 
    geom_point(show.legend = FALSE)
})


output$value <- renderPrint({ input$select })
}

###
ui <- fluidPage(theme=shinytheme("cosmo"),
                
                titlePanel("Preliminary Webtool"),
                sidebarLayout(
                  sidebarPanel(
                    h3("We can put buttons on the page!"),
                    actionButton("button", "Click me"),
                    textInput("mytext", "Input goes here"),
                    textInput("myresults", "Results will be printed here", "")
                  ), 
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Plot", plotOutput("myplot")), 
                      tabPanel("Summary"), 
                      tabPanel("Table", shiny::dataTableOutput("myTable"))
                    )
                  )
                ),
                column(10,
                       sliderInput("slider", label = h3("Slider Range"), min = 0, 
                                   max = 100, value = c(40, 60))
                ),
                column(12,
                       selectInput("category", label = h3("Color by category"), 
                                   choices = names(FL2015_34), selected = "cluster"),
                       
                       selectInput('x', 'X', choices = c("bh_tsne_x","cov","gc")),
                       selectInput('y', 'Y', choices = c("bh_tsne_y","cov","gc"))
                )
                
)

shinyApp(ui = ui, server = server)
