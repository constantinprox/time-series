library(tidyverse)
library(rio)
library(stringr)

# Import and cleaning
dfCPU <- import("CPU benchmark list.csv")
dfCPU <- dfCPU %>% rename("cpu_mark" = "CPU Mark",
                          "cpu_name" = "CPU Name",
                          "date_str" = "Test Date5")

dfCPU$cpu_mark <- dfCPU$cpu_mark %>% str_replace(",","") %>% as.numeric()

# Identify CPUs by brand; create brand dummies
dfCPU <- dfCPU %>% mutate(brand = case_when(
  str_starts(cpu_name, "Intel") ~ "Intel",
  str_starts(cpu_name, "AMD") ~ "AMD",
  str_starts(cpu_name, "Qualcomm") ~ "Qualcomm"
))
dfCPU$brand <- dfCPU$brand %>% as.factor()

# Transform time variable
dfCPU$date_str <- str_c("01-",dfCPU$date_str) #Assume tested on first of month
dfCPU$testdate <- dfCPU$date_str %>% as.Date(format="%d-%b-%y")

# Create best cpu mark per manufacturer and test month
dfCPU_best <- dfCPU$cpu_mark %>% aggregate(by = list(dfCPU$brand, dfCPU$testdate), FUN = max)
colnames(dfCPU_best) <- c("brand","testdate", "best_mark_day")
dfCPU_best <- dfCPU_best %>% spread("brand", "best_mark_day")

dfCPU_best <- dfCPU_best %>% mutate(AMD_best = cummax(ifelse(is.na(AMD),0,AMD)),
                                    Intel_best = cummax(ifelse(is.na(Intel),0,Intel)),
                                    Qualcomm_best = cummax(ifelse(is.na(Qualcomm),0,Qualcomm)))


# Create AMD-Intel distance in benchmark scores
dfCPU_best <- dfCPU_best %>% mutate(AMD_advantage = log(AMD_best) - log(Intel_best))



# Summary statistic: cpu marks increase
ggplot(dfCPU, aes(x=testdate, y=cpu_mark)) + geom_point()
ggplot(dfCPU_best, aes(x=testdate, y=AMD_best)) + geom_point()
ggplot(dfCPU_best, aes(x=testdate, y=AMD_advantage)) + geom_point()


