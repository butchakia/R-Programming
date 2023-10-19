library(readxl)
loan <- read_excel("C:/Users/whitl/OneDrive/Desktop/R Files/loan.xlsx")
View(loan)

hist(loan$loan_amnt,
     main = "Loan Amount",
     prob = TRUE,
     xlab = "Loan Amount", ylab = "Number of Borrowers")
lines(density(loan$loan_amnt),
      lwd = 2,
      col = "red")

library(ggplot2)
ggplot(data=loan, aes(x=loan_amnt))+ 
  geom_histogram(aes(y=after_stat(density)), colour='purple', fill='pink')+
  geom_density(alpha=.2, fill="cyan")+geom_vline(aes(xintercept=mean(loan_amnt)),
                                                 color="green", linetype="solid", linewidth=1)
plot(loan$annual_inc, loan$loan_amnt,
     main = "Loan amount related to annual income",
     xlab = "Annual Income", ylab = "Loan Amount")
abline(lm(loan_amnt~annual_inc, data = loan),
       lwd = 2, col = "blue")

ggplot(data=loan, aes(x=annual_inc, y=loan_amnt))+geom_point()+geom_smooth()

as.factor(loan$term)
as.factor(loan$grade)
tbl.term.grade <- xtabs(~term+grade, data=loan)
barplot(tbl.term.grade, main = "Term and Grade",
        col= c("black", "red"), legend=rownames(tbl.term.grade))

as.factor(loan$term)
as.factor(loan$grade)
ggplot(data=loan, aes(x=grade, y= ..count..)) + geom_bar(aes(fill = term), position = "dodge")

jpeg("C:\\Users\\whitl\\Documents\\loanterm.jpg")


boxplot(loan_amnt ~ term, data= loan, notch=TRUE, 
        
        col = c("blue","cyan"),
        
        main="Loan amount and Term", xlab="Term", ylab="Amount")#close the file


dev.off() 

ggplot(data=loan, aes(x=term, y=loan_amnt)) +
  geom_boxplot(aes(col=term), notch=TRUE)
ggsave("C:\\Users\\whitl\\Documents\\loanterm2.jpg", width=20, height=15, units="cm")




