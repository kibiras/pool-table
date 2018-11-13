library(shiny)
library(RMariaDB)
library(dplyr)
library(magrittr)
library(grid)
library(DBI)
library(png)
library(shinythemes)
library(ggpubr)

ui <- fluidPage(theme = shinytheme("cyborg"),
  align="center",
  strong(h1(textOutput("shot_number"), style="color:green")),
  plotOutput("plot1", height = "600px", width = "75%"),
  splitLayout(style = "border: 2px solid green;",
    cellWidths = c("20%", "20%", "60%"),
              tableOutput("stats"),
              tableOutput("total"),
              tableOutput("tbl")),
  tags$style(type="text/css", "#plot1.recalculating { opacity: 1.0; }"),
  tags$style(type="text/css", "#tbl.recalculating { opacity: 1.0; }"),
  tags$style(type="text/css", "#stats.recalculating { opacity: 1.0; }"),
  tags$style(type="text/css", "#total.recalculating { opacity: 1.0; }")
)

server <- function(input, output, session) {
  table_name <- 'event'
  pool_img <- png::readPNG("pool.png")
  conn <- dbConnect(
    drv = RMariaDB::MariaDB(), 
    username = 'seb',
    password = 'seb',
    dbname = 'seb',
    host = '127.0.0.1',
    port = '3306'
  ) 
  
  data <- reactivePoll(500, session,
                         checkFunc = function() {
                          (dbGetQuery(conn, paste0('SELECT id FROM ', table_name, ' ORDER BY id DESC LIMIT 2')))
                       },
                         valueFunc = function() {
                         ((dbReadTable(conn, table_name)) %>%
                           filter(game_id == dbGetQuery(conn, paste0('SELECT game_id from ', table_name, ' ORDER BY time DESC LIMIT 1'))[1,1]) %>%
                           mutate(x = x, y = 1 - y)) 
                           # arrange(id))
                       }
  )
  
  output$shot_number <- renderText({
    combination <- max(data()$combination_id, na.rm = TRUE) 
    case_when(
      combination == 0 ~ "1ST COMBINATION",
      combination == 1 ~ "2ND COMBINATION",
      combination == 2 ~ "3RD COMBINATION",
      combination == 3 ~ "4TH COMBINATION",
      combination == 4 ~ "5TH COMBINATION",
      combination == 5 ~ "6TH COMBINATION"
    )
  })
  
  output$stats <- renderTable({
    data() %>%
      filter(!is.na(combination_id) & name %in% c("combinationEnd", "combinationStart")) %>%
      select(status, combination_id) %>%
      group_by(combination_id) %>%
      summarise(success = max(status, na.rm = TRUE)) %>%
      mutate(success = ifelse(success == 1, "YES", ifelse(success == 0, "NO", "N/A"))) %>%
      mutate(combination_id = as.integer(as.integer(combination_id) + 1)) %>%
      rename(combination_number = combination_id)
   })
  
  output$total <- renderTable({
    data() %>%
      summarise(current_game = as.integer(max(game_id, na.rm = TRUE)), events_per_game = n()) %>%
      mutate(total_events = as.integer(dbGetQuery(conn, paste0('SELECT count(*) FROM ', table_name))[1,1]))
  })
  
  output$tbl <- renderTable({
    # Select newest combination
    newest_combination <- max(data()$combination_id, na.rm = TRUE)
    combination_timestamp <- data() %>%
      filter(combination_id == newest_combination) %>%
      select(time) %>%
      arrange(time) %>%
      head(1)
    combination_start_time <- combination_timestamp[1,1]
    data() %>%
      filter(time >= combination_start_time) %>%
      arrange(desc(id)) %>%
      head(6)
  })
  output$plot1 <- renderPlot({
    # get newest combination id
    newest_combination <- max(data()$combination_id, na.rm = TRUE)
    # get combination start timestamp
    combination_timestamp <- data() %>%
      filter(combination_id == newest_combination & name == "combinationStart") %>%
      select(time) %>%
      arrange(desc(time)) %>%
      head(1)
    combination_start_time <- combination_timestamp[1,1]
    # data for game combination
    game_combination <- data() %>%
      filter(time > combination_start_time) 
      # filter(x <= 1 & x >= 0 & y <= 1 & y >= 0) 
      # arrange(id)
    # get last white ball position
    white_ball <- game_combination %>%
      filter(ball_id == 0) %>%
      filter(!is.na(x)) %>%
      filter(time == max(time)) %>%
      select(x, y) %>%
      head(1)
    # get position for other balls
    balls <- game_combination %>%
      filter(!is.na(ball_id) & ball_id != 0) %>%
      group_by(ball_id) %>%
      filter(id == max(id)) %>%
      select(x, y, ball_id)
    # get data for white ball path
    df <- game_combination %>% 
      # filter for specific events?
      filter(!is.na(x)) %>%
      filter((ball_id == 0)) %>%
      arrange(time, id)
    # plot pool table
    ggplot() +
      annotation_custom(rasterGrob(pool_img,
                                   width = unit(1, "npc"),
                                   height = unit(1,"npc")),
                                   xmin = -0.09,
                                   xmax = 1.09,
                                   ymin = -0.29,
                                   ymax = 1.29) +
      # ball position as points
      geom_point(data = balls, aes(x = x, y = y, color = as.factor(ball_id)), size = 15, show.legend = FALSE) +
      scale_color_manual(values=c("lightblue", "blue", "blue", "blue", "blue")) + 
      # add text to ball position
      geom_text(data = balls, aes(x = x, y = y, label = ball_id), size = 5) +
      # white ball position
      geom_point(data = white_ball, aes(x = x, y = y), size = 15, color = "white") +
      # draw path for white ball
      geom_point(data = df, aes(x = x, y = y), size = 2, linetype = 2, color = "blue") +
      ylim(0, 1) +
      xlim(0, 1) +
      theme_transparent() +
      theme(plot.background = element_rect(fill = "black"))
  })
  
}

shinyApp(ui, server)