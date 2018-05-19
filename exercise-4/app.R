# Exercise 4: Interactive plots

# Load the Shiny library
library(shiny)

# Define a UI using a `fluidPage()` layout
ui <- fluidPage(
  
  # Set the page titlePanel to be "Milage by Engine Power"
  titlePanel("Milage by Engine Power"),
  
  # Output a plot 'milage', and respond to clicks on the plot
  plotOutput('milage', click = "plot_click"),
  
  
  # Output the word "Highlighting:" followed by a 'selected' text output
  p("Highlighting:",  strong(textOutput('selected', inline=TRUE)) )
)


# Define a server function
server <- function(input, output) {
  
  # Create a `reactiveValues()` list to store reactive data values
  data <- reactiveValues()
  
  # Assign a key 'selected_class' to the reactiveValue with a default value of ""
  data$selected_class <- ""  # assign a default value
  
  
  # Render the 'milage' plot output
  output$milage <- renderPlot({
    
    # Return a ggplot scatterplot for the `mpg` dataset, with `displ` on the x 
    # and `hwy` on the y axis. 
    # Color each point by whether the `class` is %in% the `selected.class` 
    # reactive value.
    # Add `guides()` so that the `color` legend is not shown (FALSE)
    gplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
      geom_point(aes(color=(class %in% data$selected_class)), size = 5) +
      guides(color = FALSE)
  })
  # Render the 'selected' text output, which should be the (text) value of your
  # the `selected.class` reactive value
  output$selected <- renderText({
    return(data$selected_class)
  })
  
  # Use `observeEvent()` to respond to plot clicks
  observeEvent(input$plot_click, {
    
    
    # Use `nearPoints()` to get selected rows in the `mpg` data set near to the 
    # click location
    selected <- nearPoints(mpg, input$plot_click)
    
    
    # Store `unique()` values from the `class` feature of the selected rows in 
    # the `selected.class` reactiveValue
    data$selected_class <- unique(selected$class)
  })
  
  # Bonus: also use `observeEvent()` to respond to brushing and change the plot
  # in a different way!
}


# Create a new `shinyApp()` using the above ui and server
shinyApp(ui = ui, server = server)
