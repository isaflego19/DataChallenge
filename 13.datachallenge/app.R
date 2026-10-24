library(shiny)
library(tidyverse)
library(gapminder)

# User interface
ui <- fluidPage(
  
  titlePanel("Shiny Gapminder"),
  
  sidebarLayout(
    sidebarPanel(
      
      # SLIDER PER SELEZIONARE ANNO
      sliderInput(
        "anno",
        "Year",
        min = min(gapminder$year),
        max = max(gapminder$year),
        value = min(gapminder$year),
        step = 5
      ),
      
      
      # MENU' A TENDINA PER SELEZIONARE CONTINENTI
      selectInput(
        "continenti",
        "Continent",
        choices = c(sort(unique(as.character(gapminder$continent))), "All"),
        selected = "All"
      ),
      
      
      # ASSEGNAZIONE ASSE X
      radioButtons(
        "asseX",
        "X",
        choices = c(
          "Life expectancy" = "lifeExp",  
          "Population"      = "pop",
          "GDP per capita"  = "gdpPercap"
        ),
        selected = "gdpPercap"
      ),
      
      # ASSEGNAZIONE ASSE Y
      radioButtons(
        "asseY",
        "Y",
        choices = c(
          "Life expectancy" = "lifeExp",
          "Population" = "pop",
          "GDP per capita" = "gdpPercap"
        ),
        selected = "lifeExp"
      )
      
    ),
    
    mainPanel(
      plotOutput("scatter")
    )
  )
)



# Server logic
server <- function(input, output, session) {
  
  # dataset filtrato
  filtered_gap <- reactive({
    
    gapminder |>
      filter(
        year == input$anno,
        input$continenti == "All" | input$continenti == continent     
      )
  })
  
  
  # SCATTERPLOT
  output$scatter <- renderPlot({
    
    data <- filtered_gap()
    p <- data |>
      ggplot(
        aes(x = .data[[input$asseX]], 
            y = .data[[input$asseY]]
        )
      ) +
      
      geom_point(
        aes(color = country, size = pop),
        alpha = 0.5,
        show.legend = FALSE
      ) +
    
      labs(
        x = case_when(
          input$asseX == "gdpPercap" ~ "GDP per capita",
          input$asseX == "pop"       ~ "Population",
          input$asseX == "lifeExp"   ~ "Life expectancy"
        ),
        y = case_when(
          input$asseY == "gdpPercap" ~ "GDP per capita",
          input$asseY == "pop"       ~ "Population",
          input$asseY == "lifeExp"   ~ "Life expectancy"
        )
      ) +
    
      theme_minimal() +
      scale_colour_manual(values = country_colors) +
      scale_size(range = c(2, 20)) +
      list(
        if (input$asseX %in% c("gdpPercap", "pop")) scale_x_log10(),
        if (input$asseY %in% c("gdpPercap", "pop")) scale_y_log10()
      )
    p
  })
  
  
}

shinyApp(ui, server)