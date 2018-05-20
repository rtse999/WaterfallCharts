# ------------------------------------------------------------------------
# Creating Waterfall Charts using R & ggplot
# Link: https://learnr.wordpress.com/2010/05/10/ggplot2-waterfall-charts/
#
# First created: 20:50 - Friday 30 March 2018
# Last modified: 17:27 - Tuesday 15 May 2018
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time 
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# ------------------------------------------------------------------------
# Session Info
# ------------------------------------------------------------------------
devtools::session_info()

# ------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------
library(tidyverse)

# ------------------------------------------------------------------------
# BACKLOG
# ------------------------------------------------------------------------
# Sort in/out columns by magnitude or by sort column
# Fixing the colours when the chart doesn't have all 3 "in", "out", "net"
# Change colour of text to white if over dark bar

# ------------------------------------------------------------------------
# Sample Data
# ------------------------------------------------------------------------

balance <- data.frame(description = c("Starting Cash", "Sales", "Refunds", 
                                      "Payouts", "Court Losses", "Court Wins", 
                                      "Contracts", "End Cash"), 
                      amount = c(2000, 3400, -1100, -100, -6600, 3800,
                                 + 1400, 2800))

balance <- balance %>% 
  mutate(description = as.factor(description),
         id = row_number(),
         type = factor(ifelse(amount > 0, "in", "out"),
                       levels = c("in", "out", "net")),
         end = cumsum(amount)
         )

balance[balance$description %in% c("Starting Cash", "End Cash"), "type"] <- "net"
balance$end <- c(head(balance$end, -1), 0)
balance$start <- c(0, head(balance$end, -1))
balance$type <- factor(balance$type, levels = c("out", "in", "net"))
balance <- balance[, c(3, 1, 4, 6, 5, 2)]

glimpse(balance)

# ------------------------------------------------------------------------
# Create Waterfall Chart function
# ------------------------------------------------------------------------
create_waterfall_chart <- function(waterfall_data_set, 
                           chart_title = NULL, 
                           scale_format = scales::dollar_format(), 
                           value_format = scales::dollar,
                           net_colour = "dark grey",
                           in_colour = "dark green",
                           out_colour = "red")  {
#
# waterfall_data_set needs the following fields:
# - id:           a numeric ID field 
# - description:  labels for graph
# - type:         has values "in" for positive values
#                 "out" for negative values
#                 "net" for starting and end values
# - start:        balance at start of period / activity
# - end:          balance at end of period / activity
# - amount:       difference in balance between end and start of period / activity
#
# chart_title      a string
# scale_format     scales::comma_format() or scales::dollar_format()
# value_format     scales::comma or scales::dollar
#
# net_colour       colour for starting and ending value bars
# in_colour        colour for positive bars
# out_colour       colour for negative bars
#
  
  strwr <- function(str) gsub(" ", "\n", str)
  
  ggplot(waterfall_data_set, aes(fill = type)) +
    geom_rect(aes(fct_inorder(description), xmin = id - 0.45, xmax = id + 0.45, ymin = end,
                  ymax = start)) +
    scale_y_continuous("", labels = scale_format) +
    scale_x_discrete("", breaks = levels(waterfall_data_set$description),
                     labels = strwr(levels(waterfall_data_set$description))) +
    theme(legend.position = "none") +
    geom_text(data = filter(waterfall_data_set, type == "in"), 
              aes(id, end, label = value_format(amount)), 
              vjust = -0.5, size = 3) +
    geom_text(data = filter(waterfall_data_set, type == "out"), 
              aes(id, end, label = value_format(amount)), 
              vjust = -0.5, size = 3) +
    geom_text(data = filter(waterfall_data_set, type == "net" & id == min(id)), 
              aes(id, end, label = value_format(amount)), 
              vjust = -0.5, size = 3) +
    geom_text(data = filter(waterfall_data_set, type == "net" & id == max(id)), 
              aes(id, start, label = value_format(amount)), 
              vjust = -0.5, size = 3) +
    labs(
      title = paste(chart_title)
    ) +
    scale_fill_manual(values = alpha(c(out_colour, in_colour, net_colour, 0.3)))
}

# ------------------------------------------------------------------------
# Call Waterfall Chart function
# ------------------------------------------------------------------------
create_waterfall_chart(balance, "Chart title for waterfall chart")

