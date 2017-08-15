#!/usr/bin/Rscript

# Activate required libraries
library(ggplot2)
library(rjson)
library(scales)

# Import data from JSON (http://json.org/) file
transactions <- fromJSON(file= "sample.txt")

# Get transactions' values
values <- sapply(transactions, function(x) as.numeric(x$complemento$valorTotal))

# Quick statistics
avgValue <- mean(values)
sdValue <- sd(values)
minValue <- min(values)
maxValue <- max(values)
message(sprintf("Average value spent by customers: $%.2f", avgValue))
message(sprintf("Standard deviation of the value spent by customers: $%.2f", sdValue))
message(sprintf("Minimum value spent by a customer: $%.2f", minValue))
message(sprintf("Maximum value spent by a customer: $%.2f", maxValue))

# Get transactions' table numbers
tables <- sapply(transactions, function(x) x$infAdic$infCpl)
#tables <- gsub("(\\b\\d)","0\\1", tables)
tables <- gsub("Mesa\\b","", tables)
tables <- as.numeric(tables)

# Get transactions' timestamps
timestamps <- sapply(transactions, function(x) x$ide$dhEmi$"$date")
timestamps <- as.POSIXct(timestamps,format="%Y-%m-%dT%H:%M:%OS")

# Create a dataframe containing all transactions
data <- data.frame(values=values, tables=tables, timestamps=timestamps)

# Get all transaction items
items <- lapply(transactions, function(x) lapply(x$dets, function(y) list(product=y$prod$xProd, price=as.numeric(y$prod$vProd))))

# Rearrange all transaction items within a single dataframe
allitems <- unlist(items, recursive = FALSE)
products <- sapply(allitems, function(x) x$product)
prices <- sapply(allitems, function(x) x$price)
allitems <- data.frame(products=products, prices=prices)

# Get restaurant menu
menu <- unique(products)

# Compute the sales contribution of each item within the restaurant's menu
menucontrib <- aggregate(prices ~ products, allitems, sum)

# Figure 1 (see below) shows a histogram of the values spent by all customers.
# The data resembles an Inverse Gamma (or a Rayleigh) distribution with a long tail.
#
# Plot data
p <- ggplot(data, aes(values)) + 
  geom_histogram(binwidth=5, color="black", fill="white") +
  geom_vline(aes(xintercept=mean(values)),
             color="blue", linetype="dashed", size=1)
print(p)

# Compute the density points from the reciprocal of the data
den <- density(1/values, from=0.0)
dat <- data.frame(x = den$x, y = den$y)

# Fit the reciprocal of the data to a Gamma distribution
fit <- fitdistr(1/values,"gamma", lower = c(0, 0))

# Get the distributioncorresponding inverse Gamma parameters
alpha=fit$estimate["shape"] 
beta=fit$estimate["rate"]

message(sprintf("Inverse Gamma shape parameter: $%.2f", alpha))
message(sprintf("Inverse Gamma scale parameter: $%.2f", beta))
message(sprintf("Mean value spent by a customer: $%.2f",beta/(alpha-1)))

# Figure 2 presents the fitted Gamma distribution versus the density points computed
# from the reciprocal of the data. The reciprocal of the data is multimodal. Thus, it's
# worth to check if it can be better approximated by a mixture of Gamma distributions 
# (e.g. people may spend slightly more at night).
# 
# Plot data with mean (blue) and mode (red)
p <- ggplot(data = dat, aes(x = x, y = y)) +
  geom_point(size = 1) +
  ggtitle("Figure 2: Fitting to a Gamma distribution") +
  geom_line(aes(x=dat$x,y=dgamma(dat$x,alpha,beta)),color="red") +
  labs(x = "1/Values", y = "p.d.f.") +
  geom_vline(aes(xintercept=(alpha-1)/beta),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=alpha/beta),
             color="blue", linetype="dashed", size=1)
print(p)

# Figure 3 presents the sales contribution of each menu item.
# As expected, the buffet accounts for most of the sales.
#
# Plot data
p <- ggplot(menucontrib, aes(x="", y=100*prices/sum(prices), fill=products)) +
  geom_bar(width = 1, stat = "identity") + 
  ggtitle("Figure 3: Sales contribution of menu items") +
  labs(x = "", y = "") +
  coord_polar("y", start=0)
print(p)

# Figure 4 shows all transactions over time. Each circle corresponds to a table 
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
      ggtitle("Figure 4: Transactions over time") +
      labs(x = "Day of the week", y = "Tables") +
      scale_x_datetime(breaks=date_breaks("12 hours"),
                       labels=date_format("%a-%H:%M:%S", tz="UTC"), 
                       limits=c(min(timestamps), max(timestamps)),
                       expand=c(0.03,0)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)

# Figure 5 is a zoomed in version of Figure 2 showing the transactions over
# the first day of historical data.
#
# Note that tables are used only once during lunch and dinner, i.e. it seems that tables 
# are left dirty until the next shift :-( Note also that customers from tables 01:99 are 
# leaving the restaurant in a non-arbitrary fashion. In particular, the graph suggests 
# that customers seated in table 01 are more likelly to leave first than those in table 
# 02 and so on.
#
# By supposing an average meal interval T spent by all customers, it is reasonable to
# assume that - in this unusual restaurant - customers prefer to sit at tables with
# low numbers. I think it's a Brazilian steakhouse with all tables aligned such that 
# table 01 is the closest to the barbacue grill ;-)
#
# Plot data
end=as.POSIXct("2016-01-05 23:59:59 BRST");
p <- ggplot(data[timestamps <= end, ], aes(x=timestamps, y=tables, size=values)) + 
  geom_point(shape=21) +
  ggtitle("Figure 5: Transactions over time") +
  labs(x = "Day of the week", y = "Tables") +
  scale_x_datetime(breaks=date_breaks("1 hour"),
                   labels=date_format("%a-%H:%M:%S", tz="UTC"), 
                   limits=c(min(timestamps), end),
                   expand=c(0.03,0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)

