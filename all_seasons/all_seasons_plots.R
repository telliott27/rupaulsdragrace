library(tidyverse)
library(Rtelliott27)
library(here)
library(lubridate)
library(showtext)
library(ggtext)

showtext_auto()

seasons.df <- read_csv(here("all_seasons/drag_race_seasons.csv"))

title <- '<span style="color:#80d4d4">When <span style="color:#f892f7">Drag Race</span> Seasons Aired</span>'

seasons.df <- seasons.df %>% 
  arrange(first.air.date) %>% 
  mutate(order = row_number(),
         label = str_c(name, " - Season ", season))

ggplot(data = seasons.df) +
  geom_rect(aes(xmin = first.air.date, xmax = last.air.date, ymin = order-0.5, ymax = order+0.5),
            fill = "#f892f7", color = "black") +
  geom_text(aes(x = first.air.date, y = order, label = label), color = "#80d4d4", hjust = 1, nudge_x = -10) +
  scale_y_reverse() +
  scale_x_date(breaks = seq(ymd("2009-01-01"), today(), by = "6 month"),
               date_labels = "%b '%y") +
  labs(
    title = title,
    y = NULL,
    x = NULL
  ) +
  expand_limits(x = ymd("2008-02-01")) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "white"),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    plot.title = element_textbox_simple(size=17, halign=0.5)
  )

ggsave(here("all_seasons/drag_race_seasons.png"), 
       width = 24, height = 6, units = "in")
