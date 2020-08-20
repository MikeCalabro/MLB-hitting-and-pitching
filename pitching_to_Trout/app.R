library(shiny)
library(tidyverse)
library(png)

trout_data <- read_rds("trout_data")

ui <- fluidPage(
   
   titlePanel("Pitching To Mike Trout"),

   sidebarLayout(
      sidebarPanel(
         checkboxGroupInput("pitches",
                     "Pitches to include:",
                     c("2-Seam Fastball",
                       "4-Seam Fastball",
                       "Changeup",
                       "Curveball",
                       "Cutter",
                       "Sinker",
                       "Slider"),
                     selected = "4-Seam Fastball"),
         sliderInput("year",
                     "Seasons to Include",
                     min = 2017,
                     max = 2020,
                     value = c(2017, 2020),
                     sep = ""),
         sliderInput("balls",
                     "Balls in the Count",
                     min = 0,
                     max = 3,
                     value = c(0, 3)),
         sliderInput("strikes",
                     "Strikes in the Count",
                     min = 0,
                     max = 2,
                     value = c(0, 2)
                     )
      ),

      mainPanel(
         plotOutput("plateView", width = "90%", height = "700px"),
         HTML('<img src="plate.png", width = "80%", height="100px"/>')
      )
   )
)



server <- function(input, output) {
   
   output$plateView <- renderPlot({
     trout_data %>%
       filter(pitch_name %in% input$pitches,
              game_year %in% (input$year[1]:input$year[2]),
              balls %in% (input$balls[1]:input$balls[2]),
              strikes %in% (input$strikes[1]:input$strikes[2])) %>%
       ggplot(aes(x = plate_x, y = plate_z)) +
       geom_segment(aes(x = -0.333, y = mean(sz_top), xend = -0.333, yend = mean(sz_bot)), color = "gray") +
       geom_segment(aes(x = 0.333, y = mean(sz_top), xend = 0.333, yend = mean(sz_bot)), color = "gray") +
       geom_segment(aes(x = -1, y = ((mean(sz_top) - mean(sz_bot))/3) + mean(sz_bot),
                        xend = 1, yend = ((mean(sz_top) - mean(sz_bot))/3) + mean(sz_bot)), color = "gray") +
       geom_segment(aes(x = -1, y = mean(sz_top) - ((mean(sz_top) - mean(sz_bot))/3),
                        xend = 1, yend = mean(sz_top) - ((mean(sz_top) - mean(sz_bot))/3)), color = "gray") +
       geom_segment(aes(x = -1, y = mean(sz_top), xend = 1, yend = mean(sz_top))) +
       geom_segment(aes(x = -1, y = mean(sz_bot), xend = 1, yend = mean(sz_bot))) +
       geom_segment(aes(x = -1, y = mean(sz_top), xend = -1, yend = mean(sz_bot))) +
       geom_segment(aes(x = 1, y = mean(sz_top), xend = 1, yend = mean(sz_bot))) +
       geom_segment(aes(x = 1, y = mean(sz_top), xend = 1, yend = mean(sz_bot))) +
       geom_point(aes(color = pitch_name), size = 1.5) +
       ylim(1, 4) +
       xlim(-2, 2) +
       theme(axis.ticks = element_blank(),
             axis.text = element_blank(),
             axis.title = element_blank(),
             panel.grid = element_blank(),
             panel.background = element_blank())
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

