#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load necessary libraries
library(shiny)
library(plotly)
library(ggplot2)
library(readr)

# Load Netflix data
netflix_data <- read_csv("/Users/alexandrakakadiaris/Dropbox/A-Family/People/AIK/Columbia/Columbia,FALL23/Exploratory Data Analysis and Visualization/PSet2/netflix.csv")

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Netflix Movie Information"),
  navbarPage(
    "Netflix Info",
    tabPanel("Movie Info",
             sidebarLayout(
               sidebarPanel(
                 selectInput("movie_title", "Select a Movie:", choices = unique(netflix_data$title)),
               ),
               mainPanel(
                 verbatimTextOutput("movie_info")
               )
             )
    ),
    tabPanel("Movie Ratings",
             sidebarLayout(
               sidebarPanel(
                 selectInput("country_filter", "Filter by Country:", 
                             choices = c("All", unique(netflix_data$country))),
                 selectInput("year_filter", "Filter by Release Year:",
                             choices = c("All", unique(netflix_data$release_year)))
               ),
               mainPanel(
                 plotlyOutput("rating_chart")
               )
             )
    )
  )
)

# Define the server logic for the Shiny app
server <- function(input, output) {
  output$movie_info <- renderText({
    movie_title <- input$movie_title
    movie <- netflix_data[netflix_data$title == movie_title, ]
    
    if (nrow(movie) > 0) {
      info <- paste("Title:", movie$title, "\n", 
                    "Description:", movie$description, "\n", 
                    "Release Year:", movie$release_year, "\n",
                    "Director:", movie$director, "\n",
                    "Cast:", movie$cast, "\n")
    } else {
      "Movie not found. Please try another title."
    }
  })
  
  # Create a bar chart of rating frequencies
  output$rating_chart <- renderPlotly({
    country_filter <- input$country_filter
    year_filter <- input$year_filter
    movies <- netflix_data[netflix_data$type == "Movie", ]
    
    # Filter movies based on the selected country
    if (country_filter != "All") {
      movies <- movies[movies$country %in% country_filter, ]
    }
    
    # Filter movies based on the selected release year
    if (year_filter != "All") {
      movies <- movies[movies$release_year == year_filter, ]
    }
    
    # Check for unique values - if not a rating, then delete the rows that are not part of the MPAA rating
    unique_ratings <- unique(movies$rating)
    
    # Create the bar plot
    p <- ggplot(movies, aes(x = rating)) + 
      geom_bar(fill = "lightblue", color = "black") + 
      labs(title = "Movie Ratings", x = "Rating", y = "Frequency") + 
      theme_minimal()
    
    # Convert ggplot to plotly for interactivity
    ggplotly(p)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)