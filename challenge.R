#!/usr/bin/Rscript

#########################################################################
# Activate required libraries
#########################################################################

library(ggplot2)
library(rjson)
library(scales)

#########################################################################
# Auxiliary functions
#########################################################################

# Filter data by period of the day
filterDataByPeriod <- function(df, from, to, day=NULL) {
  df <- df[format(df$timestamps,"%H:%M:%S") >= from & format(df$timestamps,"%H:%M:%S") <= to, ]
  if(!is.null(day)) {
    df <- df[as.numeric(format(df$timestamps,"%d")) == day, ]
  }
  df
}

# Extract all transaction values
extractTransactionValues <- function(transactions) {
  # Get transactions' values
  values <- sapply(transactions, function(x) as.numeric(x$complemento$valorTotal))
  
  # Quick statistics
  avgValue <- mean(values)
  sdValue <- sd(values)
  minValue <- min(values)
  maxValue <- max(values)
  message(sprintf("Average value spent by all customers: $%.2f", avgValue))
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
  data.frame(values=values, tables=tables, timestamps=timestamps)
}

# Extract all transaction items
extractTransactionItems <- function(transactions) {
  # Get all transaction items
  items <- lapply(transactions, function(x) lapply(x$dets, function(y, z) list(product=y$prod$xProd, price=y$prod$vProd, timestamp=z), z=x$ide$dhEmi$"$date"))
  
  # Rearrange all transaction items within a single dataframe
  items <- unlist(items, recursive = FALSE)
  products <- sapply(items, function(x) x$product)
  prices <- sapply(items, function(x) x$price)
  prices <- as.numeric(prices)
  timestamps <- sapply(items, function(x) x$timestamp)
  timestamps <- as.POSIXct(timestamps,format="%Y-%m-%dT%H:%M:%OS")
  
  # Create a dataframe containing all items
  data.frame(products=products, prices=prices, timestamps=timestamps)
}

# Summarize revenues items by period
summarizeRevenuesByPeriod <- function(allitems, from, to, days) {
  
  # Get restaurant menu
  menu <- sort(unique(allitems$products))
  
  df = data.frame()
  for (day in days) {
    # Select data
    items <- filterDataByPeriod(allitems, from, to, day)
      
    # Compute the sales contribution of each item within the restaurant's menu
    contrib <- aggregate(prices ~ products, items, sum)
    
    # Partial contribution of the menu items
    menucontrib <- as.numeric(rep(0, length(menu)))
    names(menucontrib) <- menu
    menucontrib[contrib$products] <- contrib$prices

    # Total period revenue
    revenue <- sum(contrib$prices)
    
    # Add to data frame
    df <- rbind(df,cbind(data.frame(revenue=revenue, day=day),as.data.frame(as.list(menucontrib))))
  }
  df
}

# Fit values to an Inverse Gamma distribution
fitValuesToInvGammaDist <- function(data) {
  # Fit the reciprocal of the data to an Inverse Gamma distribution
  fit <- fitdistr(1/data$values, "gamma", lower = c(0, 0))
  
  # Get the corresponding Inverse Gamma distribution parameters
  alpha=fit$estimate["shape"] 
  beta=fit$estimate["rate"]
  
  message(sprintf("Inverse Gamma shape parameter: %.2f", alpha))
  message(sprintf("Inverse Gamma scale parameter: %.2f", beta))
  
  list(alpha=alpha, beta=beta)
}

# Fit values to a Gamma distribution
fitValuesToGammaDist <- function(data) {
  # Fit ththe data to a Gamma distribution
  fit <- fitdistr(data$values, "gamma", lower = c(0, 0))
  
  # Get the corresponding Gamma distribution parameters
  alpha=fit$estimate["shape"] 
  beta=fit$estimate["rate"]
  
  message(sprintf("Gamma shape parameter: %.2f", alpha))
  message(sprintf("Gamma scale parameter: %.2f", beta))

  list(alpha=alpha, beta=beta)
}

#########################################################################
# Script body
#########################################################################

# Import data from JSON (http://json.org/) file
transactions <- fromJSON(file= "sample.txt")

# Extract transaction values
data <- extractTransactionValues(transactions)

# Extract transaction items
allitems <- extractTransactionItems(transactions)
  
#########################################################################
# Figure 1 (see below) presents the sales contribution of each menu item.
# As expected, buffet accounts for most of the sales.
#########################################################################

# Compute the sales contribution of each item within the restaurant's menu
menucontrib <- aggregate(prices ~ products, allitems, sum)

# Plot data
p <- ggplot(menucontrib, aes(x="", y=100*prices/sum(prices), fill=products)) +
  geom_bar(width = 1, stat = "identity") + 
  ggtitle("Figure 1: Sales contribution of menu items") +
  labs(x = "", y = "") +
  coord_polar("y", start=0)
print(p)

#########################################################################
# Figure 2 shows all transactions over time. Each circle corresponds to a 
# table transaction and its size is scaled according to the corresponding 
# transaction value.
#
# The restaurant opens for lunch from Monday to Saturday. Furthermore, it 
# usually opens for dinner from Monday to Wednesday. However, according to 
# the historical data, the restaurant was also opened for dinner on Thursday
# during the last week.
#
# Given the lack of historical data to check whether the restaurant's schedule
# was definitely changed or not, I'll assume that this was an unusual week.
#########################################################################

# Plot data
p <- ggplot(data, aes(x=timestamps, y=tables, size=values)) + 
      geom_point(shape=21) +
      ggtitle("Figure 2: Transactions over time") +
      labs(x = "Day of the week", y = "Tables") +
      scale_x_datetime(breaks=date_breaks("12 hours"),
                       labels=date_format("%a-%H:%M:%S", tz="UTC"), 
                       limits=c(min(data$timestamps), max(data$timestamps)),
                       expand=c(0.03,0)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)

#########################################################################
# Figure 3 is a zoomed in version of Figure 2 showing the transactions over
# the first day of historical data.
#
# Note that tables are used only once during lunch and dinner, i.e. it seems
# that tables are left dirty until the next shift :-( Note also that customers
# from tables 01:99 are leaving the restaurant in a non-arbitrary fashion. 
# In particular, the graph suggests that customers seated in table 01 are more 
# likelly to leave first than those in table 02 and so on.
#
# By supposing an average meal interval T spent by all customers, it is 
# reasonable to assume that - in this unusual restaurant - customers prefer 
# to sit at tables with low numbers. I think it's a Brazilian steakhouse with
# all tables aligned such that table 01 is the closest to the barbacue grill ;-)
#
# Finally, it seems that customers spend more at dinner.
#########################################################################

# Plot data
end=as.POSIXct("2016-01-05 23:59:59 BRST");
p <- ggplot(data[data$timestamps <= end, ], aes(x=timestamps, y=tables, size=values)) + 
  geom_point(shape=21) +
  ggtitle("Figure 3: Transactions over the first day") +
  labs(x = "Day of the week", y = "Tables") +
  scale_x_datetime(breaks=date_breaks("1 hour"),
                   labels=date_format("%a-%H:%M:%S", tz="UTC"), 
                   limits=c(min(data$timestamps), end),
                   expand=c(0.03,0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)

#########################################################################
# Figure 4 shows a histogram of the values spent by all customers. 
# The data resembles an Inverse Gamma distribution with a long tail.
#########################################################################

# Filter data by period
lunchData <- filterDataByPeriod(data, "11:00:00", "16:00:00")
dinnerData <- filterDataByPeriod(data, "18:00:00", "23:59:59")

# Plot data
p <- ggplot(lunchData, aes(lunchData$values)) + 
  geom_histogram(binwidth=5, color="black", fill="white") +
  ggtitle("Figure 4: Individual sales histogram during lunch") +
  geom_vline(aes(xintercept=mean(values)),
             color="blue", linetype="dashed", size=1)
print(p)

p <- ggplot(dinnerData, aes(dinnerData$values)) + 
  geom_histogram(binwidth=5, color="black", fill="white") +
  ggtitle("Figure 5: Individual sales histogram during dinner") +
  geom_vline(aes(xintercept=mean(dinnerData$values)),
             color="blue", linetype="dashed", size=1)
print(p)

#########################################################################
# Figure 5 in turn presents the reciprocal of the values spent by customers
# at lunch versus the corresponding (fitted) Gamma distribution.
#########################################################################

# Compute the density points from the reciprocal of the lunch data
den <- density(1/lunchData$values, from=0.0)
cloud <- data.frame(x = den$x, y = den$y)

# Fit lunc data to an Inverse Gamma distribution
lunchFit <- fitValuesToInvGammaDist(lunchData)

# Expectated value spent by customers at lunch conditioned on the Inverse Gamma distribution
message(sprintf("Expectated value spent by a customer at lunch: $%.2f",lunchFit$beta/(lunchFit$alpha-1)))

# Plot data with mean (blue) and mode (red)
p <- ggplot(data = cloud, aes(x = x, y = y)) +
  geom_point(size = 1) +
  ggtitle("Figure 5: Fitting to a Gamma distribution (lunch)") +
  geom_line(aes(x=cloud$x,y=dgamma(cloud$x,lunchFit$alpha,lunchFit$beta)),color="red") +
  labs(x = "1/Values", y = "p.d.f.") +
  geom_vline(aes(xintercept=(lunchFit$alpha-1)/lunchFit$beta),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=lunchFit$alpha/lunchFit$beta),
             color="blue", linetype="dashed", size=1)
print(p)

#########################################################################
# Figure 6 in turn presents the reciprocal of the values spent by customers
# at dinner versus the corresponding (fitted) Gamma distribution.
#########################################################################

# Compute the density points from the reciprocal of the dinner data
den <- density(1/dinnerData$values, from=0.0)
cloud <- data.frame(x = den$x, y = den$y)

# Fit dinner data to an Inverse Gamma distribution
dinnerFit <- fitValuesToInvGammaDist(dinnerData)

# Expectated value spent by customers at dinner conditioned on the Inverse Gamma distribution
message(sprintf("Expectated value spent by a customer at dinner: $%.2f",dinnerFit$beta/(dinnerFit$alpha-1)))

# Plot data with mean (blue) and mode (red)
p <- ggplot(data = cloud, aes(x = x, y = y)) +
  geom_point(size = 1) +
  ggtitle("Figure 6: Fitting to a Gamma distribution (dinner)") +
  geom_line(aes(x=cloud$x,y=dgamma(cloud$x,dinnerFit$alpha,dinnerFit$beta)),color="red") +
  labs(x = "1/Values", y = "p.d.f.") +
  geom_vline(aes(xintercept=(dinnerFit$alpha-1)/dinnerFit$beta),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=dinnerFit$alpha/dinnerFit$beta),
             color="blue", linetype="dashed", size=1)
print(p)

#########################################################################
# Figure 7 and 8 show histograms for the lunch and dinner daily revenues, 
# respectively.
# Note that the lunch revenues are better approximated by a Gaussian p.d.f.
# On the other hand, dinner revenues resembles a long tail Inverse Gamma distribution.
#########################################################################

# Summarize lunch revenues
lunchDays <- c(5,6,7,8,9,11,12,13,14,15,16,18,19,20,21,22,23)
lunchRevenues <- summarizeRevenuesByPeriod(allitems, "11:00:00", "16:00:00", lunchDays)

# Summarize dinner revenues
dinnerDays <- c(5,6,11,12,13,18,19,20,21)
dinnerRevenues <- summarizeRevenuesByPeriod(allitems, "18:00:00", "23:59:59", dinnerDays)

# Plot data
p <- ggplot(lunchRevenues, aes(lunchRevenues$revenue)) + 
  geom_histogram(binwidth=250, color="black", fill="white") +
  ggtitle("Figure 7: Daily lunch revenues histogram") +
  geom_vline(aes(xintercept=mean(revenue)),
             color="blue", linetype="dashed", size=1)
print(p)

# Plot data
p <- ggplot(dinnerRevenues, aes(dinnerRevenues$revenue)) + 
  geom_histogram(binwidth=250, color="black", fill="white") +
  ggtitle("Figure 8: Daily dinner revenues histogram") +
  geom_vline(aes(xintercept=mean(revenue)),
             color="blue", linetype="dashed", size=1)
print(p)

#########################################################################
# Figure 9 shows the daily revenues over time.
# It also presents the forecast of both lunch and dinner revenues for the 
# next week based on Generalized Linear Models. It's a preliminary attempt
# to create a model which allows one to predict next week revenues.
#########################################################################

# Create a GLM
#lunchModel <- glm(data = lunchRevenues, revenue ~ AGUA + BUFFET + CERVEJA + 
#                    CHA + REFRIGERANTE + SUCO + day, family = gaussian())
lunchModel <- glm(data = lunchRevenues, revenue ~ day, family = gaussian())

# Predict
forecastLunchDays <- c(25,26,27,28,29)
lunchForecast <- predict(lunchModel,data.frame(day=forecastLunchDays), type="response")

# Create a GLM
#dinnerModel <- glm(data = dinnerRevenues, revenue ~ AGUA + BACARDI + BUFFET + BULE.CHA + 
#                    CAFE.EXPRESSO + CAIPIRINHA + CAIPIROSKA + CERVEJA + CERVEJA.LATA + 
#                    CHA + DOCINHOS + HARUMAKI + LIMONADA + REFRIGERANTE + SAKE + 
#                    SASHIMI + SOBREMESA + SUCO + SUSHI.ESPECIAL + TEMAKI + URAMAKI +
#                    VINHO + WHISKY + YAKISSOBA+ day, family = Gamma(link = "inverse"))
dinnerModel <- glm(data = dinnerRevenues, revenue ~ day, family = Gamma(link = "inverse"))

# Predict
forecastDinnerDays <- c(25,26,27)
dinnerForecast <- predict(dinnerModel,data.frame(day=forecastDinnerDays), type="response")

# Create a data frame
df <- rbind(data.frame(revenue=lunchRevenues$revenue, day=lunchDays, shift=rep("lunch",length(lunchDays))),
            data.frame(revenue=dinnerRevenues$revenue, day=dinnerDays, shift=rep("dinner",length(dinnerDays))),
            data.frame(revenue=lunchForecast, day=forecastLunchDays, shift=rep("lunch-forecast",length(forecastLunchDays))),
            data.frame(revenue=dinnerForecast, day=forecastDinnerDays, shift=rep("dinner-forecast",length(forecastDinnerDays))))

# Plot data
p <-ggplot(df, aes(day, revenue)) +
  ggtitle("Figure 9: Daily revenue over time") +
  geom_bar(stat = "identity", aes(fill = shift)) +
  labs(x = "Days of month", y = "Revenue $$$")
print(p)
