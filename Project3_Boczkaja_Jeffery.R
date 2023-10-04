library(readxl)
BlackFriday <- read_excel("C:/Users/whitl/OneDrive/Desktop/R Files/BlackFriday.xlsx")
View(BlackFriday)

sum <- 0
for (idx in c(1:nrow(BlackFriday))){
  sum <- sum + BlackFriday[idx, "Purchase" ]
}
avg <- sum/nrow(BlackFriday)
print(avg)

sum <- 0
idx <- 1
while (idx <= nrow(BlackFriday)){
  sum <- sum + BlackFriday[idx, "Purchase" ]
  idx <- idx + 1
}
avg <- sum/nrow(BlackFriday)
print(avg)

sum <- 0
idx <- 1
repeat {
  sum <- sum + BlackFriday[idx, "Purchase" ]
  idx <- idx + 1
  if (idx > nrow(BlackFriday)){
    break
  }
}
avg <- sum/nrow(BlackFriday)
print(avg)

femalesum <- 0
femalecount <- 0
for (idx in c(1:nrow(BlackFriday))){
  if (BlackFriday[idx, "Gender" ] == 'M')
    next
  femalesum <- femalesum + BlackFriday[idx, "Purchase" ]
  femalecount <- femalecount + 1
}
femalesum <- femalesum/femalecount
print(femalesum)

sum <- 0
idx <- 1
while (idx <= nrow(BlackFriday)){
  if (BlackFriday[idx, "Gender" ] == 'F')
    sum <- sum + BlackFriday[idx, "Purchase" ]
  idx <- idx + 1
}
avg <- sum/nrow(BlackFriday[BlackFriday$Gender == 'F', ])
print(avg)

sum <- 0
idx <- 1
repeat {
  if (BlackFriday[idx, "Gender" ] == 'F')
    sum <- sum + BlackFriday[idx, "Purchase" ]
  idx <- idx + 1
  if (idx > nrow(BlackFriday)){
    break
  }
}
avg <- sum/nrow(BlackFriday[BlackFriday$Gender == 'F', ])
print(avg)

sum <- 0
idx <- 1
repeat {
  if (BlackFriday[idx, "Gender" ] == 'M')
    sum <- sum + BlackFriday[idx, "Purchase" ]
  idx <- idx + 1
  if (idx > nrow(BlackFriday)){
    break
  }
}
avg <- sum/nrow(BlackFriday[BlackFriday$Gender == 'M', ])
print(avg)

9338.949-8550.195
