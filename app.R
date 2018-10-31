library(shiny)
library(DBI)
library(RMariaDB)
library(tidyverse)
library(grid)
library(DBI)
library(png)

ui <- fluidPage(
  titlePanel("SEB pool game"),
  plotOutput("plot1", width = "100%", height = "600px"),
  tableOutput("tbl")
)

server <- function(input, output, session) {
  table_name <- 'pool'
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
  
  output$tbl <- renderTable({
    # on.exit(dbDisconnect(conn), add = TRUE)
    
    data() %>%
      # select(id, game_id, ball_id, name) %>%
      # mutate(id = as.integer(id), ball_id = as.integer(ball_id), game_id = as.integer(game_id), x = as.integer(x), y = as.integer(y)) %>%
      # filter(NAME %in% c("gameStart", "combinationStart", "combinationEnd", "ballInTheHole")) %>%
      # filter(game_id == new_game) %>%
      arrange(desc(id))
  })
  output$plot1 <- renderPlot({
    df <- filter(data(), x != "")
    white_ball <- filter(df, id == max(id)) %>%
      select(x, y)
    ggplot() +
      annotation_custom(rasterGrob(pool_img,
                                   width = unit(1, "npc"),
                                   height = unit(1,"npc")),
                                   xmin = -30,
                                   xmax = 230,
                                   ymin = -30,
                                   ymax = 130) +
      geom_path(data = df, aes(x =x, y = y), size = 2) +
      geom_point(data = white_ball, aes(x = x, y = y), size = 15, color = "white") +
      ylim(-10, 110) +
      xlim(-10, 210)
  })
  
}

shinyApp(ui, server)