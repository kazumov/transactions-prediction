#
# Future transactions prediction algorithm
# Author: Ruben R. Kazumov <kazumov@gmail.com>
# License: MIT
#

library(tidyverse)
library(lubridate)
library(ggplot2)
library(randomForest)
library(crayon)

# * Reading the data ----
# Three data sources of the single CC account

chaseData <- list(
  report2017 = read.csv("data/Chase7898_Activity20170617_20180131_20190617.CSV", header = TRUE, stringsAsFactors = FALSE),
  report2018 = read.csv("data/Chase7898_Activity20180101_20181231_20190617.CSV", header = TRUE, stringsAsFactors = FALSE), 
  report2019 = read.csv("data/Chase7898_Activity20190101_20190617_20190617.CSV", header = TRUE, stringsAsFactors = FALSE)
)

# * The data preparation ----

# * * Joining reports ----

transactions <- rbind(
  chaseData$report2017, chaseData$report2018, chaseData$report2019
)

rm(chaseData)

cat(paste("Imported:", red(dim(transactions)[1]), "raw records\n"))

# * * Deleting duplicates ----

transactions <- transactions %>% 
  distinct()
id <- seq(1, dim(transactions)[1], 1)
transactions[, "id"]  <- id # mutate/append column
transactions %>% head()
rm(id)

cat(paste("Imported unique:", red(dim(transactions)[1]), "records\n"))

# * * Re-naming the variables naming conventions
transactions %>% str()

transactions <- transactions %>%
  mutate(transDate = lubridate::mdy(Transaction.Date),
         postDate = lubridate::mdy(Post.Date)) %>%
  select(id, 
         transDate,
         postDate,
         category = Category,
         type = Type,
         amount = Amount,
         description = Description)

saveRDS(transactions, "transactions.Rds")

# * * Cancelled transactions ----
# We will follow the pattern:
# The same Description
# Two lines:
# The Amount < 0
# The Amount > 0

# * * * Find cancelled transactions ----

# transactions with the + sign
transToCancelIds <- transactions %>%
  filter(amount > 0) %>%
  arrange(desc(postDate)) %>%
  .$id

# * * * Clear cancelled transactions ----
# local variable trans would be rewritten many times
trans <- transactions

cancelTransaction <- function(transId){
  toCancel <- trans %>%
    filter(id == transId)
  
  pDate <- toCancel$postDate
  amt <- toCancel$amount
  descr <- toCancel$description
  
  pairs <- trans %>%
    filter(description == descr,
           postDate <= pDate,
           amount < 0,
           abs(amount) > abs(amt))
  
  if (dim(pairs)[1] == 0) {
    return(list(
      id = transId,
      description = descr,
      amount = amt,
      result = "There are no possible transaction to cancel!",
      cancellation = FALSE
    ))
  }
  
  # we have at least one row in `pairs`
  pair <- pairs %>%
    mutate(difference = amount + amt) %>%
    arrange(difference)
  
  pair <- pair[1, ] # workaround, because top_n(1) is not working properly
  
  pairId <- pair$id
  pairAmt <- pair$amount
  
  # cancellation
  
  trans <<- trans %>% 
    mutate(amount = ifelse(id == pairId, amount + amt, amount))
  
  trans <<- trans %>%
    mutate(amount = ifelse(id == transId, 0, amount))
  
  return(
    list(
      id = transId,
      description = descr,
      posted = pDate,
      amount = amt,
      clearedWithId = pairId,
      pairAmount = pairAmt,
      result = "Successfully cancelled!",
      cancellation = TRUE
    )
  )
}

cancelationReport <- lapply(X = transToCancelIds, cancelTransaction)

# remove transactions less than 0.00
# we will preserve records with small amounts to fit the expenses distribution form
trans <- trans %>% 
  filter(abs(amount) > 0.00)

# show the difference between raw and cleared transactions
plot(x = transactions$transDate, y = transactions$amount)
plot(x = trans$transDate, y = trans$amount)

saveRDS(trans, file = "clearedTransactions.Rds")

transactions <- trans

rm(transToCancelIds, trans, cancelTransaction, cancelationReport)


# * Transactions structure ----

# Transactions:
# 1 Payments
# 2 Expences
# - 2.1 Routines
# -- 2.1.1 (SUBS) Subscriptions
# -- 2.1.2 (RAND) Randoms
# - 2.2 Outliers
# -- 2.2.1 (SINGLE) One time
# -- 2.2.2 (PERIODIC) Periodic


# SUBS Extracting subscriptions ----

transactions <- readRDS(file = "clearedTransactions.Rds")

subscriptions <- transactions %>%
  group_by(description, amount) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  ungroup() %>%
  filter(amount < 0)

subscriptions <- subscriptions %>%
  mutate(id = 1:dim(subscriptions)[1])

# The total amount spent by susbsctiption
spentBySubscription <- subscriptions %>%
  mutate(total = count * amount) %>%
  .$total %>%
  sum()


transactions <- merge(x = transactions, 
           y = subscriptions,
           by = c("description", "amount"),
           all.x = TRUE)

transactions <- transactions %>%
  mutate(bySubscription = ifelse(is.na(id.y), FALSE, TRUE)) %>%
  rename(id = id.x, subscriptionId = id.y) %>%
  select(-count)

saveRDS(subscriptions, "subscriprions.Rds")
saveRDS(transactions, "transactionsClearedSubscriptions.Rds")

# at this point all subscriptions is difined and marked in transactions

# subscritions payment timeline
transactions %>%
  filter(amount < 0, bySubscription == TRUE) %>%
  mutate(amount = abs(amount)) %>%
  ggplot(aes(x = transDate, y = factor(description), color = amount)) +
  geom_point(size = 3) +
  scale_color_continuous(low = "lightgray", high = "black")

# for all the subscriptions we should find a probability of the future payments continuing
expenses <- transactions %>%
  filter(amount < 0) %>%
  mutate(amount = abs(amount))

expensesBySubscription <- expenses %>%
  filter(bySubscription == TRUE)

subscriptionPaymentPeriods <- expensesBySubscription %>%
  group_by(subscriptionId) %>%
  summarise(dateBegin = min(transDate),
         dateEnd = max(transDate),
         count = n()) %>%
  ungroup() %>%
  mutate(subscriptionInterval = lubridate::interval(dateBegin, dateEnd))


# plot the first possible transaction by subscription date
future <- subscriptionPaymentPeriods %>%
  mutate(nextPossibleTransactionDate = dateEnd + as.duration(subscriptionInterval) / count) %>%
  select(id = subscriptionId, transDate = nextPossibleTransactionDate)

future <- merge(x = future, 
                y = subscriptions, by.x = "id") %>%
  mutate(future = TRUE, bySubscription = TRUE) %>% 
  select(-count, subscriptionId = id)

current <- expensesBySubscription %>% 
  mutate(future = FALSE) %>%
  select(-postDate, -category, - type, -id)

rbind(current, future) %>%
  ggplot(aes(x = transDate, y = factor(description), color = future)) +
  geom_point(aes(size = future)) +
  scale_size_manual(values = c(1, 3))
  
# *** Current Stop Line *** ----
# TODO: Check the periods for future events id:1
# The periods for the future events looks erroneous for me
#

# RAND ----
# SINGLE ----
# PERIODIC ----


names(current)
names(future)

str(future)



subscriptions




hist(t %>% filter(bySubscription == FALSE, amount < 0, amount > -500) %>% .$amount, breaks = 500)

View(t)

str(subscriptions)

tOld <- transactions # 
transactions <- tOld

transactions <- transactions %>% 
  transform(subscriptionIndex = as.character(paste(description, amount, sep = "")))

str(transactions)

transactionsBySubscription <- merge(x = transactions,
                                    y = subscriptions,
                                    by.x = subscriptionIndex,
                                    by.y = id) %>%
  select(-subscriptionId)





















# * Category of transactions ----

# useless to predict of current expenses
unique(transactions$category)

# * Expenses ----

expenses <- transactions %>% 
  filter(amount < 0)

saveRDS(expenses, file = "expenses.Rds")

expensesPlot <- expenses %>% 
  mutate(amount = -round(amount)) %>%
  ggplot(aes(x = transDate, y = amount)) +
  geom_point(alpha = 0.4)

expensesPlotYLog10 <- expenses %>%
  mutate(amount = -round(amount)) %>%
  ggplot(aes(x = transDate, y = amount)) +
  scale_y_continuous(trans = "log10") +
  geom_point(alpha = 0.4)
  
saveRDS(expensesPlot, file = "expensesPlot.Rds")
saveRDS(expensesPlotYLog10, file = "expensesPlotYLog10.Rds")

expensesPlot
expensesPlotYLog10

rm(expensesPlot, expensesPlotYLog10)

# expences analysis ----

# * Routine expenses vs outliers ----
# * * Routine expenses ----

expensesHistogram <- expenses %>%
  mutate(amount = -amount) %>%
  ggplot(aes(x = amount)) +
  geom_histogram(binwidth = 1, color = "white") +
  scale_x_continuous(limits = c(0, 300)) +
  labs(x = "Amount", y = "Count",
       title = "Histogram of expenses",
       subtitle = "The amount parameter is limited by the value $300. One bin is equal $1.")

suppressWarnings(print(expensesHistogram))

saveRDS(expensesHistogram, "expensesHistogram.Rds")

rm(expensesHistogram)

# * * Routine expenses distribution ----

# We suspect the possible distributions:
# - poison
# - Z
# - T
# - Chi-squared
# - Mad

# We will find the distribution with the minimal sum of of outliers
totalExpenses = abs(sum(expenses$amount))
totalExpenseTransactions = length(expenses$amount)

# We will work with simplified non-negative expenses model:

distDat <- expenses %>%
  mutate(amount = abs(amount)) %>%
  select(x = transDate, y = amount)

# * * * Poison distribution ----
m <- mean(distDat$y)

pDis <- distDat %>%
  mutate(score = 1 - ppois(y, m)) %>%
  mutate(outlier = score < 0.05/1000)

# plot outliers
outliersPoissonPlot <- pDis %>% 
  ggplot(aes(x = x, y = y, color = outlier)) +
  geom_point(aes(size = outlier), alpha = 1) +
  scale_color_manual(values = c("grey", "red")) +
  scale_y_log10() +
  scale_size_manual(values = c(1, 2)) +
  labs(title = "outliers in Poisson distribution")

outliersPoissonPlot

saveRDS(outliersPoissonPlot, "outliersPoissonPlot.Rds")

# protocol
distProtocolPoisson <- pDis %>%
  filter(outlier == FALSE) %>%
  mutate(method = "poisson") %>%
  group_by(method) %>%
  summarise(totalAmountFitted = sum(y), nonOutliers = n()) %>%
  mutate(outliers = totalExpenseTransactions - nonOutliers)

distProtocolPoisson


# * * * Z, T, Chisq, and Mad distributions tests ----

library(outliers)
# methodology from https://www.sqlservercentral.com/articles/scoring-outliers-in-non-normal-data-with-r

tests <- c("z", "t", "chisq", "mad")

expInv <- distDat

results <- bind_rows(
    lapply(tests, function (t) {
      expInv %>%
        mutate(outlier = scores(log(y), type = t, prob = 0.95), # log function 
               method = t)
    })
)

outliersZTChiMadPlot <- results %>%
  ggplot(aes(x = x, y = y, color = outlier)) +
  geom_point(alpha = 1, aes(size = outlier)) + 
  facet_wrap( ~ method) +
  theme_minimal() + xlab("time") +
  scale_colour_manual(values = c("grey", "red")) +
  scale_size_manual(values = c(1, 2)) +
  scale_y_log10() +
  labs(title = "Outliers in Z, T, Chi square, and Mad distributions")

outliersZTChiMadPlot

saveRDS(outliersZTChiMadPlot, "outliersZTChiMadPlot.Rds")

distProtocolZTChiMad <- results %>% 
  filter(outlier == FALSE) %>%
  group_by(method) %>%
  summarise(totalAmountFitted = sum(y),
            nonOutliers = n()) %>%
  mutate(outliers = totalExpenseTransactions - nonOutliers) %>%
  select(method, totalAmountFitted, nonOutliers, outliers)

distProtocol <- rbind(distProtocolPoisson, distProtocolZTChiMad) %>%
  arrange(desc(totalAmountFitted)) %>%
  mutate(rate = 1:5, fitRatio = paste(round(totalAmountFitted * 100 / totalExpenses), "%", sep = "")) %>%
  rename(Method = method,
         `Fitted amount` = totalAmountFitted,
         `Non outliers` = nonOutliers,
         `Outliers` = outliers,
         `Rate` = rate,
         `Fitting proportion` = fitRatio)

distProtocol

saveRDS(distProtocol, "distProtocol.Rds")

# the best results with "Chi-squared" distribution

# * * Outliers in expenses ---- 

outliers <- results %>%
  filter(method == "chisq", outlier == TRUE)

outliersHistPlot <- outliers %>%
  ggplot(aes(x = y)) +
  geom_histogram(binwidth = 1, color = "white") +
  scale_x_continuous(trans = "sqrt") +
  labs(title = "Structure of outliers",
       subtitle = "Sufficient number of outliers around $0",
       x = "Outlier amount value")

outliersHistPlot

saveRDS(outliersHistPlot, "outliersHistPlot.Rds")

# see the small amounts

# total amount small outliers
smallOutliersSummary <- outliers %>%
  filter(y < 10) %>%
  summarise(Count = n(), Sum = sum(y))

smallOutliersSummary

saveRDS(smallOutliersSummary, "smallOutliersSummary.Rds")

# list big outliers
largeOutliers <- outliers %>%
  filter(y > 10) 

largeOutliersPlot <- largeOutliers %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  labs(title = "Large outliers")

largeOutliersPlot

saveRDS(largeOutliersPlot, file = "largeOutliersPlot.Rds")

# why we cannot smooth or trend the outliers: small amount big disperse


























# * Dependency ----

# * * Routine expenses ----

hist(routineExpenses$y, breaks = round(max(routineExpenses$y)))

n = length(routineExpenses$y)

chi_df <- fitdistr(
  x = routineExpenses$y,
  densfun = "chi-squared",
  start = list(df = 3),
  method = "BFGS"
)

chi_k <- chi_df[[1]][1]

chi_hist <- hist(routineExpenses$y, 
                 breaks = 100, 
                 col = "gray")
curve(dchisq(x, df = chi_k) * 1000 * diff(chi_hist$breaks)[1],
      add = TRUE, col = "green")






# anomalies
anomalies <- transactions %>% 
  group_by(amount) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(count >= 10)

# the most anomalies are subscriptions  
merge(x = anomalies, y = transactions, by.x = "amount") %>%
  select(amount, description) 
  

# we are working with chi square distribution in routine payments
routineChisq <- chisq.test(routineExpenses)

# degree of freedom
routineChisq$parameter["df"]
# 1417 

# X-squared
routineChisq$statistic["X-squared"]
# 60754.73 

qchisq(.95, df = routineChisq$parameter["df"])
# [1] 1505.687

hist(routineExpenses$y, breaks = 273)

boxplot(routineExpenses$y)





routineExpenses <- results %>%
  filter(method == "chisq", outlier == FALSE) %>%
  select(x, y)

routineExpenses %>% 
  summary()
#       x                    y         
# Min.   :2017-06-15   Min.   :  2.65  
# 1st Qu.:2017-10-13   1st Qu.: 14.06  
# Median :2018-06-23   Median : 28.99  
# Mean   :2018-06-10   Mean   : 43.53  
# 3rd Qu.:2019-01-05   3rd Qu.: 55.88  
# Max.   :2019-06-15   Max.   :272.60  


# single transaction average
mean(routineExpenses$y)
# [1] 43.52968

#single transaction SD
sd(routineExpenses$y)
# [1] 43.2014

# daily average
routineExpenses %>%
  group_by(x) %>%
  summarise(dailyTotal = sum(y)) %>%
  .$dailyTotal %>%
  mean()
# [1] 107.7227

# we know it is Chi squred distribution

# * * * Whole timeline ----
routineExpenses %>%
  ggplot(aes(x = x, y = y)) +
  geom_point()

# * * * Weekly sums ----
routineExpenses %>%
  mutate(wk = lubridate::round_date(x, unit = "week")) %>%
  group_by(wk) %>%
  summarise(sum = sum(y)) %>%
  ggplot(aes(x = wk, y = sum)) +
  geom_point() +
  geom_line() +
  geom_smooth()

# * * * Monthly ----
routineExpences %>%
  mutate(m = lubridate::round_date(x, unit = "month")) %>%
  group_by(m) %>%
  summarise(sum = sum(y)) %>%
  ggplot(aes(x = m, y = sum)) +
  geom_point() +
  geom_line() +
  geom_smooth()

# * * * Month ----
routineExpences %>%
  mutate(m = factor(lubridate::month(x, label = TRUE))) %>%
  ggplot(aes(x = m, y = y)) +
  geom_jitter(alpha = 0.2) + 
  geom_violin()

# averages
routineExpences %>%
  mutate(m = factor(lubridate::month(x, label = TRUE))) %>%
  group_by(m) %>%
  summarise(avg = mean(y)) %>%
  ggplot(aes(x = m, y = avg)) +
  geom_col()

# * * * Day of a week
# distribution
routineExpences %>%
  mutate(wd = factor(lubridate::wday(x, label = TRUE))) %>%
  ggplot(aes(x = wd, y = y)) +
  geom_jitter(alpha = 0.2) + 
  geom_violin()

# averages
routineExpences %>%
  mutate(wd = factor(lubridate::wday(x, label = TRUE))) %>%
  group_by(wd) %>%
  summarise(avg = mean(y)) %>%
  ggplot(aes(x = wd, y = avg)) +
  geom_col() 
  

# * * Outliers time dependency ----

# * * * Whole timeline ----
outlierExpences %>%
  filter(y > 10) %>% # drop the values less than 10
  ggplot(aes(x = x, y = y)) +
  geom_point()

outlierExpences %>%
  filter(y > 10) %>%
  summary()
# 
#       x                    y           score            method         
# Min.   :2017-06-19   Min.   : 298.6   Mode:logical   Length:32         
# 1st Qu.:2017-11-11   1st Qu.: 376.5   TRUE:32        Class :character  
# Median :2018-07-07   Median : 434.6                  Mode  :character  
# Mean   :2018-06-04   Mean   : 981.7                                    
# 3rd Qu.:2018-12-29   3rd Qu.:1043.2                                    
# Max.   :2019-04-08   Max.   :5573.4  

# * * * Month ----
# distribution
outlierExpences %>%
  filter(y > 10) %>%
  mutate(m = factor(lubridate::month(x, label = TRUE))) %>%
  ggplot(aes(x = m, y = y)) +
  geom_point() 

# averages
outlierExpences %>%
  filter(y > 10) %>%
  mutate(m = factor(lubridate::month(x, label = TRUE))) %>%
  group_by(m) %>%
  summarise(avg = mean(y)) %>%
  ggplot(aes(x = m, y = avg)) +
  geom_col()


transactions %>%
  filter(Amount < -1000)


# TODO: Fix red spots id: 6


