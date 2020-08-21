library(shiny)
library(tidyverse)
library(png)
library(shinythemes)

trout_data <- read_rds("trout_data")

ui <- fluidPage(
   theme = shinytheme("sandstone"),
   titlePanel("Pitching To Mike Trout"),
   navbarPage("Navbar",
   tabPanel("All Pitches",
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
                     ),
         selectInput("geom",
                     "Select Geom Display",
                     c("Point: Color = Pitch Type" = "Ppt",
                       "Point: Color = Pitch Result" = "Ppr",
                       "Bin: Color = Pitch Count" = "Bpc"),
                     selected = "Bpc")
      ),

      mainPanel(
         plotOutput("plateView", width = "62%", height = "540px"),
         img(src="plate.png", width = "45%", height = "60px")
      )
   )
   ),
   tabPanel("Batted Balls"
     
   )
   )
)



server <- function(input, output) {
   
   output$plateView <- renderPlot({
     trout_data %>%
       filter(pitch_name %in% input$pitches,
              balls %in% (input$balls[1]:input$balls[2]),
              strikes %in% (input$strikes[1]:input$strikes[2])) %>%
       ggplot(aes(x = plate_x, y = plate_z)) +
       {
         if(input$geom == "Bpc"){
         geom_bin2d(binwidth = c(0.33, 0.2323))}
       } +
       geom_segment(aes(x = -0.333, y = mean(sz_top), xend = -0.333, yend = mean(sz_bot)), color = "gray") +
       geom_segment(aes(x = 0.333, y = mean(sz_top), xend = 0.333, yend = mean(sz_bot)), color = "gray") +
       geom_segment(aes(x = -1, y = ((mean(sz_top) - mean(sz_bot))/3) + mean(sz_bot),
                        xend = 1, yend = ((mean(sz_top) - mean(sz_bot))/3) + mean(sz_bot)), color = "gray") +
       geom_segment(aes(x = -1, y = mean(sz_top) - ((mean(sz_top) - mean(sz_bot))/3),
                        xend = 1, yend = mean(sz_top) - ((mean(sz_top) - mean(sz_bot))/3)), color = "gray") +
       geom_segment(aes(x = -1, y = mean(sz_top), xend = 1, yend = mean(sz_top)), size = 1.5) +
       geom_segment(aes(x = -1, y = mean(sz_bot), xend = 1, yend = mean(sz_bot)), size = 1.5) +
       geom_segment(aes(x = -1, y = mean(sz_top), xend = -1, yend = mean(sz_bot))) +
       geom_segment(aes(x = 1, y = mean(sz_top), xend = 1, yend = mean(sz_bot))) +
       geom_segment(aes(x = 1, y = mean(sz_top), xend = 1, yend = mean(sz_bot))) +
       {
         if(input$geom == "Ppt"){
           geom_point(aes(color = pitch_name), size = 1.5)
         }else if(input$geom == "Ppr"){
           geom_point(aes(color = description), size = 2)
         }
       } +
       ylim(4.1, 1.03) +
       xlim(-1.67, 1.67) +
       theme(axis.ticks = element_blank(),
             axis.text = element_blank(),
             axis.title = element_blank(),
             panel.grid = element_blank(),
             panel.background = element_blank())
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

