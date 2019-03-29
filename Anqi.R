setwd('/Users/anqitu/Workspaces/NTU/TC2')
g <- read.csv("german_credit.csv")
summary(g)

# factorise Credibility
g$Creditability <- factor(g$Creditability, levels=c(0, 1), labels = c("Bad risk", "Good risk"))

# factorise account status
g$Account.Balance <- factor(g$Account.Balance, 
                            levels = c(1, 2, 3, 4), 
                            labels = c(" ... < 0 DM",
                                      " 0 <= ... < 200 DM",
                                      " ... >= 200 DM",
                                      " no checking account"))

# rename and factorise credit history i.e. payment.status.of.previous.credit
g$Payment.Status.of.Previous.Credit <- factor(g$Payment.Status.of.Previous.Credit, 
                            levels = c(0, 1, 2, 3, 4), 
                            labels = c(" no credits taken/all credits paid back duly",
                                       " all credits at this bank paid back duly",
                                       " existing credits paid back duly till now",
                                       "delay in paying off in the past", 
                                       " critical account/other credits existing (not at this bank)"))

colnames(g)[colnames(g)=="Duration.of.Credit..month."] <- "Duration.of.Credit.month"

# factorise purpose
g$Purpose <- factor(g$Purpose, 
                    levels = c(0,1,2,3,4,5,6,7,8,9,10),
                    labels = c(" car (new)",
                               " car (used)",
                               " furniture/equipment", 
                               " radio/television",
                               " domestic appliances",
                               " repairs",
                               " education",
                               " (vacation - does not exist?)",
                               " retraining",
                               " business",
                               " others"))

# factorise Savings account/bonds
g$Value.Savings.Stocks <- factor(g$Value.Savings.Stocks, 
                                levels = c(1,2,3,4,5),
                                labels = c(" ... < 100 DM",
                                           " 100 <= ... < 500 DM",
                                           " 500 <= ... < 1000 DM", 
                                           " .. >= 1000 DM",
                                           " unknown/ no savings account"))

# factorise length of current employment
g$Length.of.current.employment <- factor(g$Length.of.current.employment, 
                                 levels = c(1,2,3,4,5),
                                 labels = c(" unemployed",
                                            " ... < 1 year",
                                            " 1 <= ... < 4 years", 
                                            " 4 <= ... < 7 years",
                                            " .. >= 7 years"))

# factorise Sex.Marital.Status
g$Sex...Marital.Status <- factor(g$Sex...Marital.Status, 
                                 levels = c(1,2,3,4,5),
                                 labels = c(" male : divorced/separated",
                                            " female : divorced/separated/married",
                                            " male : single", 
                                            " male : married/widowed",
                                            " female : single"))
colnames(g)[colnames(g)=="Sex...Marital.Status"] <- "Sex.Marital.Status"

# factorise Guarantors
g$Guarantors <- factor(g$Guarantors, 
                       levels = c(1,2,3),
                       labels = c(" none",
                                  " co-applicant",
                                  " guarantor"))

# factorise property
g$Most.valuable.available.asset <- factor(g$Most.valuable.available.asset, 
                       levels = c(1,2,3,4),
                       labels = c(" real estate",
                                  " building society savings agreement/life insurance",
                                  " car or other", 
                                  " unknown / no property"))


# rename Age.Years
colnames(g)[colnames(g)=="Age..years"] <- "Age.years"

# factorise other installment plan
g$Concurrent.Credits <- factor(g$Concurrent.Credits, 
                              levels = c(1,2,3),
                              labels = c(" bank",
                                         " stores",
                                         " none"))

# factorise housing
g$Type.of.apartment <- factor(g$Type.of.apartment, 
                              levels = c(1,2,3),
                              labels = c(" rent",
                                         " own",
                                         " for free"))


# factorise occupation
g$Occupation <- factor(g$Occupation, 
                      levels = c(1,2,3,4),
                      labels = c(" unemployed/ unskilled - non-resident",
                                 " unskilled - resident",
                                 " skilled employee / official",
                                 " management/ self-employed/highly qualified employee/ officer"))

# factorise telephone
g$Telephone <- factor(g$Telephone, 
                       levels = c(1,2),
                       labels = c(" none",
                                  " yes, registered under the customers name"))
# factorise foreign worker
g$Foreign.Worker <- factor(g$Foreign.Worker, 
                      levels = c(1,2),
                      labels = c("yes",
                                 "no"))

summary(g)

# Mars ----------------------------------------
library(earth)
mars1 <- earth(Creditability~.-Creditability, nfold=10,  data=g, glm=list(family=binomial), degree=2)
# mars1 <- earth(g$Creditability~., nfold=10,  data=g, glm=list(family=binomial))
summary(mars1)
plotd(mars1)
prob.mars1 <- predict(mars1, type = 'response')
max(prob.mars1)
# Find best thresholds
count <- 100
costs <- vector("numeric", count)
thresholds <- c(1:count)
for (threshold in thresholds) {
  predict.mars1 = factor(prob.mars1 > threshold / 100, levels = c(FALSE, TRUE), labels = c('Bad risk','Good risk'))
  predict.mars1
  cm <-  table(Actual = g$Creditability, Predicted = predict.mars1)
  TN <-  cm[1]
  FN <-  cm[2]
  FP <-  cm[3]
  TP <-  cm[4]
  cost <-  FN * 1 + FP * 5
  costs[threshold] <-  cost
  }
plot(x = thresholds / 100, y = costs, col ="blue", lwd = 1, type = 'l',
     xlab = "thresholds", ylab = "Cost", main = "Cost vs Probability Threshold")

best_threhold = thresholds[which.min(costs)] / 100
predict.mars1 = factor(prob.mars1 > best_threhold, levels = c(FALSE, TRUE), labels = c('Bad risk','Good risk'))
predict.mars1
cm <-  table(Actual = g$Creditability, Predicted = predict.mars1)
cm
cost <-  cm[2] * 1 + cm[3] * 5
print(cost)

# Most important factors
summary(mars1, details = TRUE)
coef(mars1)
mars1$coefficients
mars1$glm.coefficients
ev <- evimp (mars1)
ev
rownames((ev))
plot(ev, cex.var = 0.1, cex.legend = 0.7) #ranks variables

#(ev) (If don't limit degree of interaction=2)
# 1. Account Balance
# 2. Duration of credit month
# 3. Payment status of previous credit 
cat(format(mars1, style="bf"))  #gives basis function--does this give interactions?


imp2 <- drop1(mars1)
imp2

# Interation
plotmo(mars1, ngrid1 = 11, ngrid2 = 2)
unique(g$Type.of.apartment)


mars1 <- earth(Creditability~Account.Balance, nfold=10,  data=g, glm=list(family=binomial), degree=2)
# mars1 <- earth(Creditability~Account.Balance, nfold=10,  data=g, degree=2)
summary(mars1, details = TRUE)
coef(mars1)
predict(mars1, type = 'response')[0:10]
predict(mars1)[0:10]

exp(0.02919915)/(1+exp(0.02919915))
