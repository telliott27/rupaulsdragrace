library(tidyverse)
library(Rtelliott27)
library(here)
library(lubridate)
library(showtext)
library(ggtext)
library(ggrepel)

showtext_auto()

seasons.df <- read_csv(here("all_seasons/drag_race_seasons.csv"))

title <- '<span style="color:#80d4d4">When <span style="color:#f892f7">Drag Race</span> Seasons Aired</span>'

seasons.df <- seasons.df %>% 
  arrange(first.air.date) %>% 
  mutate(order = row_number(),
         label = str_c(name, " - Season ", season),
         previous_air.date = lag(first.air.date),
         label_date = case_when(
           as.numeric(first.air.date - previous_air.date) < 21 ~ previous_air.date + days(21),
           TRUE ~ first.air.date
         ))
pink <- "#f892f7"
teal <- "#80d4d4"

ggplot(data = seasons.df) +
  geom_rect(aes(xmin = first.air.date, xmax = last.air.date, ymin = order-0.5, ymax = order+0.5),
            fill = "#f892f7", color = "black") +
  geom_text(aes(x = first.air.date, y = order, label = label), color = "#80d4d4", hjust = 1, 
            nudge_x = -10, size = 3) +
  scale_y_reverse() +
  scale_x_date(breaks = seq(ymd("2009-01-01"), today(), by = "6 month"),
               date_labels = "%b '%y") +
  labs(
    title = title,
    y = NULL,
    x = NULL
  ) +
  expand_limits(x = ymd("2008-06-01")) +
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

library(scales)
reverse2_trans <- function() {
  trans_new(
    "reverse2",
    function(x) -1 * as.numeric(x), # Force values to be numeric for Date objects
    function(x) -1 * as.numeric(x)
  )
}

seasons.df %>% 
  ggplot() +
  geom_rect(aes(xmin = order-0.5, xmax = order+0.5, ymin = first.air.date, ymax = last.air.date),
            fill = pink, color = "black") +
  geom_text(aes(y = label_date, x = order, label = label), color = "#80d4d4", 
            hjust = 0, vjust = 1,
            size = 3, nudge_x = 0.75) +
  scale_y_continuous(
    trans = c("date", "reverse2"),
    breaks = seq(ymd("2009-01-01"), today(), by = "6 months"),
    labels = ~ format(.x, "%b '%y")
  ) +
  labs(
    title = title,
    y = NULL,
    x = NULL
  ) +
  expand_limits(x = 60) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "white"),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    plot.title = element_textbox_simple(size=17, halign=0.5)
  )
ggsave(here("all_seasons", "drag_race_seasons_long.png"),
       width = 8, height=24, units = "in")

seasons.df %>% 
  mutate(year = year(first.air.date)) %>% 
  count(year) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_col(fill = pink) +
  geom_text(aes(x = year, y = 0.1, label = n), color = "black",
            vjust = 0, fontface = "bold") +
  scale_x_continuous(breaks = seq(2009,2022, by = 1)) +
  labs(
    title = "Number of Drag Race Seasons Aired Per Year",
    x = NULL,
    y = NULL
  ) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(color = teal, face = "bold"),
    axis.text.x = element_text(color = "white")
  )

ggsave(here("all_seasons/seasons_per_year.png"), width = 8, height = 6, units = "in",
       dpi = 400)


seasons.df %>% 
  mutate(days_aired = as.numeric(last.air.date - first.air.date)) %>% 
  arrange(desc(days_aired)) %>% 
  ggplot(aes(y = fct_reorder(label,days_aired), x = days_aired)) +
  geom_col(fill = pink) +
  geom_text(aes(y = label, x = days_aired, label = str_c(days_aired, " days")),
            hjust = 1, color = "black", fontface = "bold", size = 3, nudge_x = -1) +
  labs(
    title = "Number of Days Aired Per Season of Drag Race",
    x = NULL,
    y = NULL
  ) +
  scale_x_continuous(breaks = seq(0,120,20)) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(color = teal, face = "bold"),
    axis.text.y = element_text(color = "white"),
    axis.text.x = element_text(color = "white")
  )
