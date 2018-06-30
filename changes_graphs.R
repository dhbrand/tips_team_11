suppressPackageStartupMessages(library(tidyverse))
library(googlesheets)
# devtools::install_github("dgrtwo/gganimate")
library(gganimate)
library(viridis)
# devtools::install_github("jaredhuling/jcolors")
library(jcolors)

# gs_ls() %>% glimpse()
tips <-  gs_title("Tips Project 2019 - Blue Team 11")
# tips %>% gs_browse(ws = "2017")
# tips_2017 <- gs_read(tips, ws = "2017", col_names = FALSE)[1:13,2:3] 
# tips_2017 <- cbind(tips_2017, year = rep(2017, 13)) 
# colnames(tips_full) <- c("categories", "count", "year")
# tips_2011 <- gs_read(tips, ws = "2011", col_names = FALSE)[1:13,2:3]
# tips_2011 <- cbind(tips_2011, year = rep(2011, 13)) 
# check <- bind_rows(tips_2017, tips_2011)
# colnames(check) <- c("categories", "count", "year")

years_a <- c("2011", "2015", "2016", "2017", "2018")
years_b <- c("2012", "2013", "2014")
tips_by_year <- list()
for (i in years_a) {
  tips_by_year[[i]] <- gs_read(tips, ws = i, col_names = FALSE)[1:13,2:3] %>% select(categories = X2, count = X3 ) %>% mutate(year = rep(i, 13))
}
for (i in years_b) {
  tips_by_year[[i]] <- gs_read(tips, ws = i, col_names = FALSE)[1:13,3:4] %>% select(categories = X3, count = X4)  %>% mutate(year = rep(i, 13))
}

tips_full <- bind_rows(tips_by_year)

# colourCount = n_distinct(check$categories)
# getPalette = colorRampPalette(brewer.pal(12, "Accent"))

bar <- tips_full %>% ggplot(aes(categories, count)) + 
  geom_bar(aes(fill = categories), stat = "identity") +  
  facet_wrap(~year) +
  scale_fill_viridis(discrete = TRUE) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), panel.background = element_rect(fill = "white"))
bar
pie <- bar + coord_polar() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())
pie
bar2 <- tips_full %>% ggplot(aes(categories, count, frame = year)) + 
  geom_bar(aes(fill = categories), stat = "identity", position = "identity") +  
  scale_fill_viridis(discrete = TRUE, option = "magma") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), legend.position = "none")
gganimate(bar2, interval = .5)
pie2 <- bar2 + coord_polar() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"), plot.title = element_text(size = rel(4)))
gganimate(pie2, interval = .5)
gganimate(pie2, "pie2.mp4", interval = .5)
gganimate(bar2, "bar2.mp4")
