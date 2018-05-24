
#'rebound on VL tõus üle cut-off-i peale algset VL langust ravi algusest. 
#'ehk selles artiklis - viiruse väljumine ravi kontrolli alt peale seda kui ta on korra kontrolli alla saadud

library(tidyverse)
library(grid)
library(gridExtra)
library(gtable)
library(viridis)

(rb <- read_csv2("data/rebound_sample_vol1.csv"))
rb <- mutate(rb, time = parse_number(year))


rb_gathered <- filter(rb, !is.na(time)) %>% 
  select(time, `rep per y`, `inter per y`) %>% 
  gather(key, value, -time)

#' plotgrob
p <- ggplot(data = rb_gathered, mapping = aes(x = time, y = value, color = key)) +
  geom_point() +
  geom_line() +
  labs(x = "Years from baseline") +
  scale_x_continuous(breaks = unique(rb_gathered$time)) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.background = element_blank(),
        legend.key = element_rect(colour = 0, inherit.blank = TRUE),
        legend.position = c(0.8, 0.8),
        legend.title = element_blank())
pg <- ggplotGrob(p)

#' table grob
rb_sideways <- select(rb, `viral rebound`, `person years`) %>% 
  as.matrix() %>% t() %>% 
  as_data_frame() 
colnames(rb_sideways) <- rb$year
rb_sw <- bind_cols(data_frame(at_risk = c("viral rebound", "person years")), rb_sideways)
tbl <- tableGrob(rb_sw, cols = NULL, rows = NULL, theme = ttheme_minimal(padding = unit(c(8, 4), "mm")))
tbl$widths <- unit(rep(1/ncol(rb_sw), ncol(rb_sw)), "npc")

#' other bits and pices
rect <- rectGrob(gp = gpar(fill = 0, col = 0))
y.title <- grobTree(rect, textGrob("Viral rebound?", rot = 90, vjust = 2))
firstcol.title <- grobTree(rect, textGrob("Number at risk", vjust = 1))
lastcol.title <- grobTree(rect, textGrob("Other", vjust = 1))
mat <- matrix(list(y.title, pg, rect, firstcol.title, rect, lastcol.title), nrow = 2, byrow = TRUE)

g <- gtable_matrix(name = "plot", grobs = mat, 
                   widths = unit(c(1/ncol(rb_sw), (ncol(rb_sw) - 2)/ncol(rb_sw), 1/ncol(rb_sw)), "npc"), 
                   heights = unit(c(9/10, 1/10), c("npc")))

gg <- gtable_matrix(name = "tab", grobs = matrix(list(g, tbl), nrow = 2), 
                    widths = unit(1, "npc"),
                    heights = unit(c(9/10, 1/10), "npc"))

grid.draw(gg)
