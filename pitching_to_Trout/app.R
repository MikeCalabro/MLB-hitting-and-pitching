library(shiny)
library(tidyverse)
library(png)
library(shinythemes)

trout_data <- read_rds("trout_data")

ui <- fluidPage(
   theme = shinytheme("sandstone"),
   titlePanel("Pitching To Mike Trout 2017-2020: A Shiny App by Michael Calabro"),
   navbarPage("Navbar",
   tabPanel("All Pitches",
        column(2, 
         checkboxGroupInput("pitches",
                     "Pitches to include:",
                     c("2-Seam Fastball",
                       "4-Seam Fastball",
                       "Changeup",
                       "Curveball",
                       "Cutter",
                       "Sinker",
                       "Slider"),
                     selected = c("2-Seam Fastball",
                                  "4-Seam Fastball",
                                  "Changeup",
                                  "Curveball",
                                  "Cutter",
                                  "Sinker",
                                  "Slider")),
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
                     selected = "Bpc"),
         br(),
         checkboxInput("nums",
                       "Show Zone Numbers:",
                       value = TRUE)
        ),
      column(2,
         checkboxGroupInput("results",
                            "Pitch Results to include:",
                            c("ball",
                              "called_strike",
                              "foul",
                              "foul_tip",
                              "hit_into_play",
                              "hit_into_play_no_out",
                              "swinging_strike"),
                            selected = c("ball",
                                         "called_strike",
                                         "foul",
                                         "foul_tip",
                                         "hit_into_play",
                                         "hit_into_play_no_out",
                                         "swinging_strike")),
         tableOutput("allTable3")
                                                
             ),

      column(4,
         plotOutput("allPlot", height = "540px"),
         img(src="plate.png", width = "70%", height = "60px")
      ),
      column(4,
         tableOutput("allTable1"),
         br(),
         tableOutput("allTable2")
      )
   ),
   tabPanel("Batted Balls"
     
   )
   )
)



server <- function(input, output) {
   
   output$allPlot <- renderPlot({
     
     num_x <- c(-.7, 0, .7, -.7, 0, .7, -.7, 0, .7, -1.3, 1.3, -1.3, 1.3)
     num_y <- c(2, 2, 2, 2.6, 2.6, 2.6, 3.2, 3.2, 3.2, 3.7, 3.7, 1.4, 1.4)
     val <- c(7, 8, 9, 4, 5, 6, 1, 2, 3, 11, 12, 13, 14)
     num_tab <- data.frame(num_x, num_y, val)
     
     trout_data %>%
       filter(pitch_name %in% input$pitches,
              description %in% input$results,
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
       {
         if(input$nums){
           geom_text(data = num_tab, aes(x = num_x, y = num_y, label = val), size = 4)
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
   
   output$allTable1 <- renderTable({
    table_data <- trout_data %>%
       filter(pitch_name %in% input$pitches,
              description %in% input$results,
              balls %in% (input$balls[1]:input$balls[2]),
              strikes %in% (input$strikes[1]:input$strikes[2]))
    table_data <- table_data %>%
       mutate(total = nrow(table_data))
    
    table_data %>%
       group_by(pitch_name, total) %>%
       summarise(count = n()) %>%
       mutate(share = count/total) %>%
       select(pitch_name, count, share) %>%
       arrange(desc(count))
   },
   striped = TRUE,
   bordered = TRUE,
   spacing = "s")
   
   output$allTable2 <- renderTable({
     table_data <- trout_data %>%
       filter(pitch_name %in% input$pitches,
              description %in% input$results,
              balls %in% (input$balls[1]:input$balls[2]),
              strikes %in% (input$strikes[1]:input$strikes[2]))
     table_data <- table_data %>%
       mutate(total = nrow(table_data))
     
     table_data %>%
       group_by(description, total) %>%
       summarise(count = n()) %>%
       mutate(share = count/total) %>%
       select(description, count, share) %>%
       arrange(desc(count))
   },
   striped = TRUE,
   bordered = TRUE,
   spacing = "s")   
   
   output$allTable3 <- renderTable({
     table_data <- trout_data %>%
       filter(pitch_name %in% input$pitches,
              description %in% input$results,
              balls %in% (input$balls[1]:input$balls[2]),
              strikes %in% (input$strikes[1]:input$strikes[2]))
     table_data <- table_data %>%
       mutate(total = nrow(table_data))
     
     table_data %>%
       group_by(zone, total) %>%
       summarise(count = n()) %>%
       mutate(share = count/total) %>%
       select(zone, count, share) %>%
       arrange(desc(count)) %>%
       head(n = 10L)
   },
   striped = TRUE,
   bordered = TRUE,
   spacing = "s") 
   
}

# Run the application 
shinyApp(ui = ui, server = server)

