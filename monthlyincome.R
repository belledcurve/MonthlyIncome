library(tidyverse)

income <- read_csv("income.csv")

income.raw <- income %>%
  select(month, category, hours)

attach(income.raw)
income.raw

income.raw %>%
  mutate("total" = case_when (category == ".hagwon" ~ (hours * 17000)
                             ,category == ".co" ~ (hours * 30000)
                             ,category == ".tutoring" ~ (hours * 30000 * (1/0.967)))) %>%
  mutate("before.tax" = total, "after.tax" = total * 0.967) %>%
  group_by(month) %>% 
  mutate("monthly.total" = sum(after.tax, na.rm = TRUE))

AddWork <- function(time, work, plus) {
  
  function.income <- income.raw
  for (a in 1:length(function.income$month)) {
    if (identical(time, function.income$month[a]) & identical(work, function.income$category[a])) {
      function.income$hours[a] <- sum(function.income$hours[a], plus)
      break
    }
  }

  return.value <- function.income %>%
    mutate("total" = case_when (category == ".hagwon" ~ (hours * 17000)
                                ,category == ".co" ~ (hours * 30000)
           ,category == ".tutoring" ~ (hours * 30000 * (1/0.967)))) %>%
    mutate("before.tax" = total, "after.tax" = total * 0.967) %>%
    group_by(month) %>% 
    mutate("monthly.total" = sum(after.tax, na.rm = TRUE))
  
  income.raw <<- return.value    
  return(return.value)
}

reset <- function() {
  return(
  income.raw <<- income %>%
    select(month, category, hours)
  )
}

SaveIncome <- function() {
  write_csv(income.raw, "income.csv")
  return(income.raw)
}

AddMonth <- function(month, work) {
  new <- c(month, work, 0)
  income.raw <<- rbind(income.raw, new)
  return(
    income.raw
  )
}


library(tidyverse)

== testings ==
function.income <- income.raw
function.income

income.raw$hours[1]
income.raw$hours[1] = income.raw$hours[1] + 6
income.raw$hours[1]


identical(4, income.raw$month[1]) & identical(".hagwon", income.raw$category[1])


x <- matrix(c(1:12),4,3)
x
x[3,1] <- 6


?write_csv
write_csv(income.raw, "income.csv")

