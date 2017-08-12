#!/usr/bin/Rscript

# Activate required libraries
library(ggplot2)
library(rjson)
library(scales)

# Import data from JSON (http://json.org/) file
transactions <- fromJSON(file= "sample.txt")

# Quick statistics
summary(transactions)

# Get transactions' values
values <- sapply(transactions, function(x) x$complemento$valorTotal)
meanValue <- mean(values)
print(meanValue)
n=length(values)

# Get transactions' table numbers
tables <- sapply(transactions, function(x) x$infAdic$infCpl)
#tables <- gsub("(\\b\\d)","0\\1", tables)
tables <- gsub("Mesa\\b","", tables)
tables <- as.numeric(tables)

# Get transactions' timestamps
timestamps <- sapply(transactions, function(x) x$ide$dhEmi$"$date")
timestamps <- as.POSIXct(timestamps,format="%Y-%m-%dT%H:%M:%OS")

# Sample data
data <- data.frame(values=values, tables=tables, timestamps=timestamps)

# Figure 1 (see below) shows all transactions over time. Each circle corresponds to a table 
# transaction and its size is scaled according to the corresponding transaction value.
#
# The restaurant opens for lunch from Monday to Saturday. Furthermore, it seems
# that it usually opens for dinner from Monday to Wednesday. However, according to the
# historical data, the restaurant was also opened for dinner on Thursday during the last week.
#
# Given the lack of historical data to confirm check whether the restaurant's schedule was 
# definitely changed or not, I'll assume that this was an unusual week. 
#
# Plot data
p <- ggplot(data, aes(x=timestamps, y=tables, size=values)) + 
      geom_point(shape=21) +
      ggtitle("Figure 1: Transactions over time") +
      labs(x = "Day of the week", y = "Tables") +
      scale_x_datetime(breaks=date_breaks("12 hours"),
                       labels=date_format("%a-%H:%M:%S", tz="UTC"), 
                       limits=c(min(timestamps), max(timestamps)),
                       expand=c(0.03,0)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)

# Figure 2 (see below) is a zoomed in version of Figure 1 showing the transactions over
# the first day of historical data.
#
# Note that customers from tables 01:99 are leaving the restaurant 
# in a non-arbitrary fashion. In particular, the graph suggests that customers seated
# in table 01 are more likelly to leave first than those in table 02 and so on.
#
# By supposing an average meal interval T spent by all customers, it is reasonable to
# assume that - in this unusual restaurant - customers prefer to sit at tables with
# low numbers. I think it's a steakhouse and table 01 is the closest to the barbacue grill ;-)
#
# Plot data
end=as.POSIXct("2016-01-05 23:59:59 BRST");
p <- ggplot(data[timestamps <= end, ], aes(x=timestamps, y=tables, size=values)) + 
  geom_point(shape=21) +
  ggtitle("Figure 2: Transactions over time") +
  labs(x = "Day of the week", y = "Tables") +
  scale_x_datetime(breaks=date_breaks("1 hour"),
                   labels=date_format("%a-%H:%M:%S", tz="UTC"), 
                   limits=c(min(timestamps), end),
                   expand=c(0.03,0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)

