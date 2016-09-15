
# For the Data Source and details: https://www.kaggle.com/hugomathien/soccer
# code: https://www.kaggle.io/svf/296694/56e206c05dc60d6cfc1d777ffb730249/__results__.html

packages2 <- c("RSQLite", "DT", "DescTools", "qtlcharts", "ggvis", "radarchart", "tidyr")
install.packages(packages2)


con <- dbConnect(SQLite(), dbname="database.sqlite")

# list all tables
dbListTables(con)

player       <- tbl_df(dbGetQuery(con,"SELECT * FROM player"))
player_stats <- tbl_df(dbGetQuery(con,"SELECT * FROM player_stats"))

player_stats <-  player_stats %>%
  rename(player_stats_id = id) %>%
  left_join(player, by = "player_api_id")

# Browsing data
## see what’s available in my joined table:
str(player_stats)

latest_ps <- 
  player_stats %>% 
  group_by(player_api_id) %>% 
  top_n(n = 1, wt = date_stat) %>%
  as.data.frame()

#only interested in top 20 players so I choose them from the latest observation based on overall_rating:
top20 <- 
  latest_ps %>% 
  arrange(desc(overall_rating)) %>% 
  head(n = 20) %>%
  as.data.frame()


library(DT)
top20 %>% 
  select(player_name, birthday, height, weight, preferred_foot, overall_rating) %>% 
  datatable(., options = list(pageLength = 10))

library(DescTools)
Desc(top20$overall_rating, plotit = TRUE)

library(qtlcharts)
iplotCorr(top20[,10:42], reorder=TRUE)


library(ggvis)
measures <- names(top20[,10:42])

top20 %>% 
  ggvis(x = input_select(measures, label = "Choose the x-axis:", map = as.name)) %>% 
  layer_points(y = ~overall_rating, fill = ~player_name)


library(radarchart)
library(tidyr)

radarDF <- top20 %>% select(player_name, 10:42) %>% as.data.frame()

radarDF <- gather(radarDF, key=Label, value=Score, -player_name) %>%
  spread(key=player_name, value=Score)

chartJSRadar(scores = radarDF, maxScale = 100, showToolTipLabel = TRUE)
