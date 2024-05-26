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







### delay_from_due_date ###
# Observation
class(df$delay_from_due_date)
# Integer
unique(df$delay_from_due_date)

# Quite Clean




### num_of_delayed_payment ###
# Observation
class(df$num_of_delayed_payment)

unique(df$num_of_delayed_payment)
  

df <- df %>% 
  mutate(num_of_delayed_payment = parse_integer(num_of_delayed_payment))
# Observation
class(df$num_of_delayed_payment)
 
unique(df$num_of_delayed_payment)


### num_of_delayed_payment ###
# Observation
class(df$changed_credit_limit)
# Numeric
unique(df$changed_credit_limit)

df <- df %>% 
  mutate(changed_credit_limit = parse_number(changed_credit_limit))

# Observation
class(df$changed_credit_limit)
# Numeric
unique(df$changed_credit_limit)




### num_credit_inquiries ###
# Observation
class(df$num_credit_inquiries)
# Numeric
unique(df$num_credit_inquiries)

# Quite Clean




### credit_mix ###

# Observation
class(df$credit_mix)
 
unique(df$credit_mix)
df <- df %>% 
  mutate(credit_mix = recode(credit_mix, 
                             "_" = "Nothing")
         )%>% 
  mutate(credit_mix = parse_factor(credit_mix)) %>% 
  mutate(credit_mix = recode_factor(credit_mix, Nothing = NA_character_)) 

df$credit_mix <- factor((df$credit_mix),
                         levels = c("Bad",
                                    "Standard",
                                    "Good"))

# Observation
class(df$credit_mix)
 
unique(df$credit_mix)


### outstanding_debt ###
  
# Observation
class(df$outstanding_debt)
 
unique(df$outstanding_debt)

df <- df %>% 
  mutate(outstanding_debt = parse_number(outstanding_debt))

# Observation
class(df$outstanding_debt)
 
unique(df$outstanding_debt)




### credit_utilization_ratio ###

# Observation
class(df$credit_utilization_ratio)
 
unique(df$credit_utilization_ratio)

df <- df %>% 
  mutate(credit_utilization_ratio = round(credit_utilization_ratio, 2))

# Observation
class(df$credit_utilization_ratio)
 
unique(df$credit_utilization_ratio)

### credit_history_age ###
# Observation
class(df$credit_history_age)
 
unique(df$credit_history_age)


# Convert into months and integer
year_converter <- function(x) {
  if (is.na(x)) {
    # Return NA
    return(NA)
  } else {
    # Get years
    years <- as.numeric(str_extract(x, "\\d+(?= Years)"))
    # Get months
    months <- as.numeric(str_extract(x, "\\d+(?= Months)"))
    
    # Sum into years
    sum_of_years <- (years + (months / 12))
    # Rounded years
    round_sum_years <- round(sum_of_years, 2)
    
    return(round_sum_years)
  }
}

df <- df %>% 
  mutate(credit_history_age = sapply(credit_history_age, 
                                     year_converter))

# Observation
class(df$credit_history_age)
 
unique(df$credit_history_age)




### payment_of_min_amount ###

# Observation
class(df$payment_of_min_amount)
 
unique(df$payment_of_min_amount)

df <- df %>% 
  mutate(payment_of_min_amount = parse_factor(payment_of_min_amount)) %>% 
  mutate(payment_of_min_amount = recode_factor(payment_of_min_amount, NM = NA_character_)) 

# Observation
class(df$payment_of_min_amount)
 
unique(df$payment_of_min_amount)


### total_emi_per_month ###

# Observation
class(df$total_emi_per_month)
 
unique(df$total_emi_per_month)

df <- df %>% 
  mutate(total_emi_per_month = round(total_emi_per_month, 2))

# Observation
class(df$total_emi_per_month)
 
unique(df$total_emi_per_month)



### total_emi_per_month ###
# Observation
class(df$amount_invested_monthly)
 
unique(df$amount_invested_monthly)

df <- df %>% 
  mutate(amount_invested_monthly = parse_number(amount_invested_monthly)) %>% 
  mutate(amount_invested_monthly = round(amount_invested_monthly, 2))

# Observation
class(df$amount_invested_monthly)
 
unique(df$amount_invested_monthly)




### payment_behavior ###

# Observation
class(df$payment_behaviour)
unique(df$payment_behaviour)

# Separating columns into two
df <- df %>% 
  # handling NA
  mutate(payment_behaviour = ifelse(payment_behaviour == "!@9#%8", NA, payment_behaviour)) %>% 
  # Separate payment_behaviour into two columns: payment_spending, payment_value
  separate(payment_behaviour, into = c("payment_spending", "payment_value"), sep = "_spent_", remove = FALSE) %>% 
  select(-payment_behaviour)

# remove extra string and recode into factor
df <- df %>%  mutate(payment_value = str_replace(payment_value, "_value_payments", "")) %>% 
  mutate(payment_value = as.factor(payment_value)) %>% 
  mutate(payment_spending = as.factor(payment_spending))

# Change Levels
df$payment_spending <- factor(df$payment_spending,
                               levels = c("Low",
                                          "High")) 

df$payment_value <- factor(df$payment_value,
                               levels = c("Small",
                                          "Medium",
                                          "Large"))

# Observation
class(df$payment_spending)
class(df$payment_value)
unique(df$payment_spending)
unique(df$payment_value)


### monthly_balance ###### 

# Observation
class(df$monthly_balance)
unique(df$monthly_balance)

df <- df %>% 
  mutate(monthly_balance = parse_number(monthly_balance)) %>% 
  mutate(monthly_balance = round(monthly_balance, 2))


# Observation
class(df$monthly_balance)
unique(df$monthly_balance)

### credit_score ###

glimpse(df)


