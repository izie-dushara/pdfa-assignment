### Install Packages ###
library(tidyverse)

### Import CSV File ###
df <- read.csv("data.csv")

### Check Default Data ###
df %>% 
  View()

#### Overview ###
glimpse(df)
# Lower case all names
names(df) <- tolower(names(df))

### Select Variable ###
df1 <- df %>% 
  select(credit_history_age, 
         outstanding_debt, 
         num_credit_inquiries, 
         type_of_loan, 
         credit_score) 
View(df1)

### credit_history_age ###
# Observation
class(df1$credit_history_age)
# Character
unique(df1$credit_history_age)


# Convert into months and integer
year_converter <- function(credit_history_age) {
  if (is.na(credit_history_age)) {
    return(NA)
  } else {
    # Get years
    years <- as.integer(str_extract(credit_history_age, "\\d+(?= Years)"))
    # Get months
    months <- as.integer(str_extract(credit_history_age, "\\d+(?= Months)"))
  
    # Sum into months
    sum_of_months <- (years * 12) + months
    return(sum_of_months)
  }
}

df1 <- df1 %>% 
  mutate(credit_history_age = sapply(credit_history_age, 
                                     year_converter))

# Observation
class(df1$credit_history_age)
# Character
unique(df1$credit_history_age)
View(df1)

