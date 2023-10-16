library(readxl)
loan <- read_excel("C:/Users/whitl/OneDrive/Desktop/R Files/loan.xlsx")
View(loan)

is.numeric(loan$id)
is.numeric(loan$loan_amnt)
is.numeric(loan$term)
is.numeric(loan$int_rate)
is.numeric(loan$installment)
is.numeric(loan$grade)
is.numeric(loan$emp_length)
is.numeric(loan$home_ownership)
is.numeric(loan$annual_inc)
is.numeric(loan$verification_status)
is.numeric(loan$loan_status)

range(loan$loan_amnt)
mean(loan$loan_amnt)
median(loan$loan_amnt)
sd(loan$loan_amnt)
quantile(loan$loan_amnt, probs = c(0, .25, .5, .75))

range(loan$int_rate)
mean(loan$int_rate)
median(loan$int_rate)
sd(loan$int_rate)
quantile(loan$int_rate, probs = c(0, .25, .5, .75))

cor(loan$int_rate, loan$installment)

table(loan$term)
names(sort(table(loan$term), decreasing = TRUE))[1]

prop.table(table(loan$loan_status))
names(sort(-prop.table(table(loan$loan_status))))[1]

is.factor(loan$term)
is.factor(loan$loan_status)
loan$term <- as.factor(loan$term)
loan$loan_status <- as.factor(loan$loan_status)
xtabs(~ term + loan_status, data=loan)
xtab.term.loan_status <- xtabs(~ term + loan_status, data = loan) 
prop.table(xtab.term.loan_status, margin = 1) 
prop.table(xtab.term.loan_status, margin = 2)

loan$term <- as.factor(loan$term)
loan$grade <- as.factor(loan$grade)
loan$emp_length <- as.factor(loan$emp_length)
loan$home_ownership <- as.factor(loan$home_ownership)
loan$verification_status <- as.factor(loan$verification_status)
loan$loan_status <- as.factor(loan$loan_status)
summary(loan)
