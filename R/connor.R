
#'rebound on VL tõus üle cut-off-i peale algset VL langust ravi algusest. 
#'ehk selles artiklis - viiruse väljumine ravi kontrolli alt peale seda kui ta on korra kontrolli alla saadud

library(tidyverse)
library(gridExtra)
library(viridis)

(rb <- read_csv2("data/rebound_sample_vol1.csv"))
rb <- mutate(rb, year = parse_number(year))


rb_gathered <- filter(rb, !is.na(time)) %>% 
  select(time, `rep per y`, `inter per y`) %>% 
  gather(key, value, -time)


ggplot(data = rb_gathered, mapping = aes(x = time, y = value, color = key)) +
  geom_point() +
  geom_line() +
  labs(x = "Years") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8),
        legend.title = element_blank())


