#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(DT)


mobility <- read.csv("movement_data.csv",sep=';')
mobility$Date <- as.Date(mobility$Date) #as.Date() method returns the object of a class "Date"
mobility$Province <- as.factor(mobility$Province) #returns the original object of a class with the requested column specified as a factor rather than a numeric

# Define UI for application that will show a simple project
ui <- fluidPage(
   titlePanel("Covid-19 effects on veterans Data"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "dv",label="Category",
                  choices = c("Retail_Recreation", "Grocery_Pharmacy", "Parks", "Transit_Stations", "Workplaces, Residential"),
                  selected = "Grocery_Pharmacy"),
      
      selectInput(inputId = "provinces","Provinces(s)",
                  choices = levels(mobility$Province),
                  multiple = TRUE,
                  selected = "Grocery_Pharmacy", "Parks", "Transit_Stations"),
      
    dateRangeInput(inputId = "date", label = "Date Range",
                   start = ,min(mobility$Date),
                   end = max (mobility$Date))
        
    ),
    mainPanel(
      plotOutput (outputId = "plot"),
      em("Positive and negative percantages indicate an increase and decrease from the aseline period(median value between January 3 and Febuary 6, 2020) respectively."),
      
      DT::dataTableOutput(outputId = "table")
      
    )
  )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
filtered_data <- reactive({
  subset(mobility,
         Province %in% input$provinces & 
           Date >= input$date[1] & Date <= input$date[2])
})

output$plot <- renderPlot ({
  ggplot(filtered_data(),
         aes_string(x="Date", y = input$dv, color = "Province")) + geom_point(alpha = 0.5) + 
         ylab("%change from baseline")
})
   
output$table <- DT::renderDataTable({
  filtered_data()
})

output$download_data <- downloadHandler(
  filename = "download_data.csv",
  content = function(file) {
    data <- filtered_data()
    write.csv(data, file, row.names = FALSE)
  }
)

}

# Run the application 
shinyApp(ui = ui, server = server)
