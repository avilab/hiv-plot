
#'rebound on VL tõus üle cut-off-i peale algset VL langust ravi algusest. 
#'ehk selles artiklis - viiruse väljumine ravi kontrolli alt peale seda kui ta on korra kontrolli alla saadud

library(tidyverse)
library(grid)
library(gridExtra)
library(gtable)
library(viridis)

# NB! adjust path if file not in data folder
(rb <- readr::read_csv2("data/rebound_sample_vol1.csv"))

rb_gathered <- select(rb, year, `rep per y`, `inter per y`) %>% 
  gather(key, value, -year)

#' plotgrob
p <- ggplot(data = rb_gathered, mapping = aes(x = year, y = value, color = key, group = key)) +
  geom_point() +
  geom_line() +
  theme(axis.title.y = element_blank(),
        legend.position = c(0.8, 0.8),
        legend.title = element_blank())
pg <- ggplotGrob(p)

#' table grob
rb_sideways <- select(rb, `viral rebound`, `person years`) %>% 
  t() %>% 
  as_data_frame()
rb_sw <- bind_cols(data_frame(at_risk = c("Viral rebound", "Person years")), rb_sideways)
tbl <- tableGrob(rb_sw, cols = NULL, rows = NULL)

tbl$widths <- unit(rep(1/ncol(rb_sw), ncol(rb_sw)), "npc")

#' other bits and pices
rect <- rectGrob(gp = gpar(fill = 0, col = 0))

#' Plot y-axis title
y.title <- grobTree(rect, textGrob("Viral rebound", rot = 90))

#' Table first column title
firstcol.title <- gtable_row("title", grobs = list(grobTree(rect, textGrob("Number at risk")), rect),
                             widths = unit(c(1 / ncol(rb_sw), 1 - (1 / ncol(rb_sw))), "npc"))

gplot <- gtable_matrix("plot", grobs = matrix(list(y.title, pg), nrow = 1), 
                    widths = unit(c(1 / (ncol(rb_sw) + 5), 1 - (1 / (ncol(rb_sw) + 5))), "npc"),
                    heights = unit(1, "npc"))

gtab <- gtable_matrix("table", grobs = matrix(list(firstcol.title, tbl), nrow = 2), 
                       widths = unit(1, "npc"),
                       heights = unit(c(1/4, 3/4), "npc"))

gg <- gtable_matrix(name = "gg", grobs = matrix(list(gplot, gtab), nrow = 2), 
                    widths = unit(1, "npc"),
                    heights = unit(c(8/10, 2/10), "npc"))

grid.draw(gg)
