# ------------------------------------------------------------------------
# Creating Waterfall Charts using R & ggplot
# Link: https://learnr.wordpress.com/2010/05/10/ggplot2-waterfall-charts/
#
# Location: /Users/raymondtse/Dropbox/Analysis/Waterfall Charts/waterfall.r
# First created: 20:50 - Friday 30 March 2018
# Last modified: 23:44 - Sunday 13 May 2018m
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
# Change colours of bars - dark grey (net), dark green (in), red (out)
# Change colours - all grey
# Change colours - parameterised
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
waterfallChart <- function(waterfallDataSet, chartTitle = NULL, 
                           scaleFormat = scales::dollar_format(), 
                           valueFormat = scales::dollar) {
#
# waterfallDataSet needs the following fields:
# - id:           a numeric ID field 
# - description:  labels for graph
# - type:         has values "in" for positive values
#                 "out" for negative values
#                 "net" for starting and end values
# - start:        balance at start of period / activity
# - end:          balance at end of period / activity
# - amount:       difference in balance between end and start of period / activity
#
# chartTitle      a string
# scaleFormat     scales::comma_format() or scales::dollar_format()
# valueFormat     scales::comma or scales::dollar
#
  
  strwr <- function(str) gsub(" ", "\n", str)
  
  ggplot(waterfallDataSet, aes(fill = type)) +
    geom_rect(aes(fct_inorder(description), xmin = id - 0.45, xmax = id + 0.45, ymin = end,
                  ymax = start)) +
    scale_y_continuous("", labels = scaleFormat) +
    scale_x_discrete("", breaks = levels(waterfallDataSet$description),
                     labels = strwr(levels(waterfallDataSet$description))) +
    theme(legend.position = "none") +
    geom_text(data = filter(waterfallDataSet, type == "in"), aes(id, end, label = valueFormat(amount)), vjust = -0.5, size = 3) +
    geom_text(data = filter(waterfallDataSet, type == "out"), aes(id, end, label = valueFormat(amount)), vjust = -0.5, size = 3) +
    geom_text(data = filter(waterfallDataSet, type == "net" & id == min(id)), aes(id, end, label = valueFormat(amount)), vjust = -0.5, size = 3) +
    geom_text(data = filter(waterfallDataSet, type == "net" & id == max(id)), aes(id, start, label = valueFormat(amount)), vjust = -0.5, size = 3) +
    labs(
      title = paste(chartTitle)
    )
}

# ------------------------------------------------------------------------
# Call Waterfall Chart function
# ------------------------------------------------------------------------
waterfallChart(balance, "Chart title for waterfall chart")
