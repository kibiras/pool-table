library(shiny)
library(DBI)
library(RMariaDB)
library(tidyverse)
library(grid)
library(DBI)
library(png)
library(shinythemes)
library(ggpubr)

ui <- fluidPage(theme = shinytheme("cyborg"),
  align="center",
  titlePanel("SEB pool game"),
  h3(textOutput("shot_number"), style="color:green"),
  plotOutput("plot1", width = "100%", height = "600px"),
  tableOutput("tbl"),
  tags$style(type="text/css", "#plot1.recalculating { opacity: 1.0; }"),
  tags$style(type="text/css", "#tbl.recalculating { opacity: 1.0; }")
)

server <- function(input, output, session) {
  table_name <- 'event'
  pool_img <- png::readPNG("pool.png")
  conn <- dbConnect(
    drv = RMariaDB::MariaDB(), 
    username = 'seb',
    password = 'sebpsw',
    dbname = 'seb',
    host = '127.0.0.1',
    port = '3306'
  ) 
  
  data <- reactivePoll(1000, session,
                         checkFunc = function() {
                          (dbGetQuery(conn, paste0('SELECT id FROM ', table_name, ' ORDER BY id DESC LIMIT 1')))
       
                       },
                         valueFunc = function() {
                         ((dbReadTable(conn, table_name)) %>%
                           filter(game_id == dbGetQuery(conn, paste0('SELECT MAX(game_id) from ', table_name))[1,1]))
                       }
  )
  
  output$shot_number <- renderText({
    combination <- max(data()$combination_id, na.rm = TRUE) 
    case_when(
      combination == 0 ~ "FIRST SHOT",
      combination == 1 ~ "SECOND SHOT",
      combination == 2 ~ "THIRD SHOT",
      combination == 3 ~ "FOURTH SHOT",
      combination == 4 ~ "FIFTH SHOT",
      combination == 5 ~ "SIXTH SHOT"
    )
  })
  
  output$tbl <- renderTable({
    newest_combination <- max(data()$combination_id, na.rm = TRUE)
    combination_timestamp <- data() %>%
      filter(combination_id == newest_combination) %>%
      select(time) %>%
      arrange(time)
    combination_start_time <- combination_timestamp[1,1]
    data() %>%
      filter(time >= combination_start_time) %>%
      arrange(desc(id)) %>%
      head(25)
  })
  output$plot1 <- renderPlot({
    newest_combination <- max(data()$combination_id, na.rm = TRUE)
    combination_timestamp <- data() %>%
      filter(combination_id == newest_combination) %>%
      select(time) %>%
      arrange(time)
    combination_start_time <- combination_timestamp[1,1]
    game_combination <- data() %>%
      filter(time >= combination_start_time) 
    
    white_ball <- filter(game_combination, id == max(id)) %>%
      select(x, y)
    
    balls <- game_combination %>%
      filter(!is.na(ball_id)) %>%
      group_by(ball_id) %>%
      filter(id == max(id)) %>%
      select(x, y, ball_id)
    
    df <- game_combination %>% 
      filter(x != "") %>%
      filter(is.na(ball_id))
    
    ggplot() +
      annotation_custom(rasterGrob(pool_img,
                                   width = unit(1, "npc"),
                                   height = unit(1,"npc")),
                                   xmin = -0.065,
                                   xmax = 1.065,
                                   ymin = -0.23,
                                   ymax = 1.23) +
      geom_path(data = df, aes(x =x, y = y), size = 2, color = "white") +
      geom_point(data = white_ball, aes(x = x, y = y), size = 15, color = "white") +
      geom_point(data = balls, aes(x = x, y = y, color = as.factor(ball_id)), size = 15, show.legend = FALSE) +
      geom_text(data = balls, aes(x = x, y = y, label = ball_id), size = 3.5) +
      ylim(-.10, 1.10) +
      xlim(-.10, 1.10) +
      theme_transparent() +
      theme(plot.background = element_rect(fill = "black"))
  })
  
}

shinyApp(ui, server)