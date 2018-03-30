# ------------------------------------------------------------------------
# Creating Waterfall Charts using R & ggplot
# Link: https://learnr.wordpress.com/2010/05/10/ggplot2-waterfall-charts/
#
# Location: /Users/raymondtse/Dropbox/Analysis/Waterfall Charts/waterfall.r
# First created: 20:50 - Friday 30 March 2018
# Last modified: 20:50 - Friday 30 March 2018
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time 
# ------------------------------------------------------------------------
format(Sys.time(), "Fri Mar 30 20:49:29 2018")

# ------------------------------------------------------------------------
# Session Info
# ------------------------------------------------------------------------
devtools::session_info()

# ------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------
library(tidyverse)

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
balance <- balance[, c(3, 1, 4, 6, 5, 2)]

glimpse(balance)

strwr <- function(str) gsub(" ", "\n", str)

ggplot(balance, aes(fill = type)) + 
  geom_rect(aes(xmin = id - 0.45, xmax = id + 0.45, 
                ymin = end, ymax = start)) +
  scale_x_discrete("", breaks = levels(balance$description), 
                   labels = (balance$description))


+
         scale_x_discrete("", breaks = levels(balance$desc), 
                          labels = strwr(levels(balance$desc))) +
         theme(legend.position = "none"))


