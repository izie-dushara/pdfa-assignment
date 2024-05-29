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

### Recoding ###

### id ###
# Observation
class(df$id) # numeric
unique(df$id)

# Check duplication
sum(duplicated(df$id)) # 0

# check NA
sum(is.na(df$id)) # 0

### customer_id ###
# Observation
class(df$customer_id) # character
unique(df$customer_id)

### month ###
# Observation
class(df$month) # character
unique(df$month)

# Change month into categorical and detecting NAs
# lowercase all the factor
df <- df %>% 
  mutate(month = parse_factor(tolower(month)))

# Observation
class(df$month) # factor
unique(df$month)

### name ###
# Observation
class(df$name) # character
unique(df$name)

# Detecting any NAs
# remove unnecessary special character from names
# lowercase all the name
df <- df %>% 
  mutate(name = str_replace_all(name, "[^a-zA-Z ]", "")) %>% 
  mutate(name = parse_character(tolower(name)))

# Observation
class(df$name) # character
unique(df$name)

### age ###
# Observation
class(df$age) # character
unique(df$age)

# remove special characters from the string
# Change age into integer and detecting any NAs
df <- df %>%
  mutate(age = str_replace_all(age, "[^0-9-]", "")) %>% 
  mutate(age = parse_integer(age))

# Observation
class(df$age) # integer
unique(df$age)

### SSN ###
# Observation
class(df$ssn) # character
unique(df$ssn)

# detect falsehood 
ssn_gibberish <- df %>%
  select(ssn) %>% 
  filter(!str_detect(ssn, "^\\d{3}-\\d{2}-\\d{4}$"))

unique(ssn_gibberish)
# replace gibberish value with NAs
df <- df %>% 
  mutate(ssn = ifelse(!str_detect(ssn, "^\\d{3}-\\d{2}-\\d{4}$"), NA_character_, ssn))

# Observation
class(df$ssn) # character
unique(df$ssn)

### occupation ###
# Observation
class(df$occupation) # character
unique(df$occupation)

# lower case all values
# replace empty value with NAs
df <- df %>% 
  mutate(occupation = tolower(occupation)) %>% 
  mutate(occupation = ifelse(occupation == "_______", NA_character_, occupation))
 
# Observation
class(df$occupation) # character
unique(df$occupation)

### annual_income ###
# Observation
class(df$annual_income) # character
unique(df$annual_income)

# Remove special characters
# Change annual_income into numeric and detecting any NAs
# Round value into two decimals place
df <- df %>% 
  mutate(annual_income = str_replace_all(annual_income, "[^0-9-.]", "")) %>% 
  mutate(annual_income = parse_number(annual_income)) %>% 
  mutate(annual_income = round(annual_income, 2))

# Observation
class(df$annual_income) # numeric
unique(df$annual_income)

### monthly_inhand_salary ###
# Observation
class(df$monthly_inhand_salary) # numeric
unique(df$monthly_inhand_salary)

# Round value into two decimals place
df <- df %>% 
  mutate(monthly_inhand_salary = round(monthly_inhand_salary, 2))

# Observation
class(df$monthly_inhand_salary) # numeric
unique(df$monthly_inhand_salary)

### num_bank_accounts ###
# Observation
class(df$num_bank_accounts) # integer
unique(df$num_bank_accounts)

### num_credit_card ###
# Observation
class(df$num_credit_card) # integer
unique(df$num_credit_card)

### interest_rate ###
# Observation
class(df$interest_rate) # integer
unique(df$interest_rate)

### num_of_loans ###
# Observation
class(df$num_of_loan) # character
unique(df$num_of_loan)

# Change num_of_loan to integer and detecting any NAs
df <- df %>%
  mutate(num_of_loan = str_replace_all(num_of_loan, "[^0-9-]", "")) %>% 
  mutate(num_of_loan = parse_integer(num_of_loan))

# Observation
class(df$num_of_loan) # integer
unique(df$num_of_loan) 

### type_of_loans ###
# Observation
class(df$type_of_loan) # character
unique(df$type_of_loan)

# Detecting any NAs
# Remove redundant words
# Adding space where neccessary
# Lowercase all words
# Detecting any NAs
df <- df %>% 
  mutate(type_of_loan = str_replace_all(type_of_loan, "\\bLoan\\b|\\band\\b|\\s+", "")) %>% 
  mutate(type_of_loan = str_replace_all(type_of_loan, "(?<=[a-z])(?=[A-Z])", " ")) %>% 
  mutate(type_of_loan = tolower(type_of_loan)) %>% 
  mutate(type_of_loan = parse_character(type_of_loan))

# Observation
class(df$type_of_loan) # character
unique(df$type_of_loan)

### delay_from_due_date ###
# Observation
class(df$delay_from_due_date) # integer
unique(df$delay_from_due_date)

### num_of_delayed_payment ###
# Observation
class(df$num_of_delayed_payment) # character
unique(df$num_of_delayed_payment)

# remove any special characters
# detecting any NAs  
df <- df %>% 
  mutate(num_of_delayed_payment = str_replace_all(num_of_delayed_payment, "[^0-9-]", "")) %>% 
  mutate(num_of_delayed_payment = parse_integer(num_of_delayed_payment))
# Observation
class(df$num_of_delayed_payment)
unique(df$num_of_delayed_payment)

### changed credit limit ###
# Observation
class(df$changed_credit_limit) # character
unique(df$changed_credit_limit)

# remove special character
# detecting any NAs
df <- df %>% 
  mutate(changed_credit_limit = str_replace_all(changed_credit_limit,"[^0-9-.]","")) %>% 
  mutate(changed_credit_limit = parse_number(changed_credit_limit))

# Observation
class(df$changed_credit_limit) # numeric
unique(df$changed_credit_limit)

### num_credit_inquiries ###
# Observation
class(df$num_credit_inquiries) # integer
unique(df$num_credit_inquiries)

### credit_mix ###
# Observation
class(df$credit_mix) # character
unique(df$credit_mix)

# Recode "_" into "Nothing"
# Change into factor and detecting NAs
df <- df %>% 
  mutate(credit_mix = recode(credit_mix, 
                             "_" = "")
         )%>% 
  mutate(credit_mix = parse_factor(credit_mix))

#
df$credit_mix <- factor((df$credit_mix),
                         levels = c("Bad",
                                    "Standard",
                                    "Good"))

# Observation
class(df$credit_mix) # factor
unique(df$credit_mix)


### outstanding_debt ###
# Observation
class(df$outstanding_debt) # character
unique(df$outstanding_debt)

# Remove special characters
# Change into numeric and detecting any NAs
df <- df %>% 
  mutate(outstanding_debt = str_replace_all(outstanding_debt, "[^0-9-.]", "")) %>% 
  mutate(outstanding_debt = parse_number(outstanding_debt))

# Observation
class(df$outstanding_debt) # numeric
unique(df$outstanding_debt)

### credit_utilization_ratio ###
# Observation
class(df$credit_utilization_ratio) # numeric
unique(df$credit_utilization_ratio)

# round into 2 decimal places
df <- df %>%
  mutate(credit_utilization_ratio = round(credit_utilization_ratio, 2))

# Observation
class(df$credit_utilization_ratio) # numeric
unique(df$credit_utilization_ratio)
##### Something is wrong here #####
### credit_history_age ###
# Observation
class(df$credit_history_age) # character
unique(df$credit_history_age)

# Convert into months and integer
year_converter <- function(x) {
  if (is.na(x)) {
    # Return NA
    return(NA)
  } else {
    # Get years from string
    years <- as.numeric(str_extract(x, "\\d+(?= Years)"))
    # Get months from string
    months <- as.numeric(str_extract(x, "\\d+(?= Months)"))
    
    # Sum into years
    sum_of_years <- (years + (months / 12))
    # Rounded years
    round_sum_years <- round(sum_of_years, 2)
    
    return(round_sum_years)
  }
}

# apply year converter
df <- df %>% 
  mutate(credit_history_age = sapply(credit_history_age, 
                                     year_converter))

# Observation
class(df$credit_history_age) # numeric
unique(df$credit_history_age)

### payment_of_min_amount ###
# Observation
class(df$payment_of_min_amount) # character
unique(df$payment_of_min_amount)

# Change into factor
# recode NM into NA_character_
df <- df %>% 
  mutate(payment_of_min_amount = parse_factor(payment_of_min_amount)) %>% 
  mutate(payment_of_min_amount = recode_factor(payment_of_min_amount, NM = NA_character_)) 

# Observation
class(df$payment_of_min_amount) # factor
unique(df$payment_of_min_amount)


### total_emi_per_month ###
# Observation
class(df$total_emi_per_month) # numeric
unique(df$total_emi_per_month)

# round into 2 decimal places
df <- df %>% 
  mutate(total_emi_per_month = round(total_emi_per_month, 2))

# Observation
class(df$total_emi_per_month) # numeric
unique(df$total_emi_per_month)

### amount_invested_monthly ###
# Observation
class(df$amount_invested_monthly) # character
unique(df$amount_invested_monthly)

# remove special characters
# Change into numeric and detecting any NAs
# round into 2 decimal places
df <- df %>% 
  mutate(amount_invested_monthly = str_replace_all(amount_invested_monthly, "[^0-9-.]", "")) %>% 
  mutate(amount_invested_monthly = parse_number(amount_invested_monthly)) %>% 
  mutate(amount_invested_monthly = round(amount_invested_monthly, 2))

# Observation
class(df$amount_invested_monthly)
unique(df$amount_invested_monthly)

### payment_behavior ###
# Observation
class(df$payment_behaviour) # character
unique(df$payment_behaviour)

# Separating columns into two
df <- df %>% 
  # Replacing any field without alphabets into empty string
  # Detecting any NAs
  # Separate payment_behaviour into two columns: payment_spending, payment_value
  # remove payment_behaviour
  mutate(payment_behaviour = str_replace_all(payment_behaviour, "[^A-Za-z]", "")) %>% 
  mutate(payment_behaviour = parse_character(payment_behaviour)) %>% 
  separate(payment_behaviour, into = c("payment_spending", "payment_value"), sep = "spent", remove = FALSE) %>% 
  select(-payment_behaviour)

# remove extra string 
# recode into factor
# lower case factor
df <- df %>%  mutate(payment_value = str_replace(payment_value, "valuepayments", "")) %>% 
  mutate(payment_value = as.factor(tolower(payment_value))) %>% 
  mutate(payment_spending = as.factor(tolower(payment_spending)))

# Change Levels
df$payment_spending <- factor(df$payment_spending,
                               levels = c("low",
                                          "high")) 

df$payment_value <- factor(df$payment_value,
                               levels = c("small",
                                          "medium",
                                          "large"))

# Observation
class(df$payment_spending) # factor
class(df$payment_value) # factor
unique(df$payment_spending)
unique(df$payment_value)

### monthly_balance ###### 
# Observation
class(df$monthly_balance) # character
unique(df$monthly_balance)

# remove any special characters
# change into numeric and detecting NAs
# round into 2 decimal places
df <- df %>% 
  mutate(monthly_balance = str_replace_all(monthly_balance, "[^0-9-.]", "")) %>% 
  mutate(monthly_balance = parse_number(monthly_balance)) %>% 
  mutate(monthly_balance = round(monthly_balance, 2))

# Observation
class(df$monthly_balance) # numeric
unique(df$monthly_balance)

### credit_score ###

glimpse(df)

# Tolerated missing values
delete.na <- function(df, n = 0) {
  df[rowSums(is.na(df)) <= n, ]
}

df = delete.na(df, 3)
glimpse(df)
colSums(is.na(df))
summary(df)
View(df)
# write.csv(df, "data_recoded.csv")

#==========================================================#

### Handling NAs ###

### Setup for monthly_balance ###
# Outlier too small that it messed with the mean value
df <- df %>% 
  mutate(monthly_balance = ifelse(monthly_balance < 0, NA_character_, monthly_balance)) %>% 
  mutate(monthly_balance = as.numeric(monthly_balance))

### Replace NAs of factor with the mode of respective customer account ###

### payment_spending ###
df <- df %>% 
  group_by(customer_id) %>%
  mutate(payment_spending = as.character(payment_spending)) %>% 
  mutate(payment_spending = ifelse(is.na(payment_spending), names(sort(-table(payment_spending))[1]), payment_spending)) %>% 
  mutate(payment_spending = parse_factor(payment_spending))
sum(is.na(df$payment_spending)) # 0

### payment_value ###  

df <- df %>% 
  group_by(customer_id) %>% 
  mutate(payment_value = as.character(payment_value)) %>% 
  mutate(payment_value = ifelse(is.na(payment_value), names(sort(-table(payment_value))[1]), payment_value)) %>% 
  mutate(payment_value = parse_factor(payment_value)) 
sum(is.na(df$payment_value))

### fill for value that will not likely vary ###
# group by customer_id 
# Fill NAs with information that not going have a drastic changes
df <- df %>% 
  group_by(customer_id) %>% 
  fill(name, .direction = "downup") %>% 
  fill(ssn, .direction = "downup") %>% 
  fill(occupation, .direction = "downup") %>% 
  fill(monthly_inhand_salary, .direction = "downup") %>% 
  fill(num_credit_inquiries, .direction = "downup") %>% 
  fill(credit_mix, .direction = "downup") %>% 
  fill(payment_of_min_amount, .direction = "downup") %>% 
  fill(credit_history_age, .direction = "downup")


### Impude possible changing value using the mean of respective customer ###
df <- df %>% 
  group_by(customer_id) %>% 
  mutate(type_of_loan = ifelse(is.na(type_of_loan), "not specified", type_of_loan)) %>% 
  mutate(num_of_delayed_payment = ifelse(is.na(num_of_delayed_payment), mean(num_of_delayed_payment, na.rm = TRUE), num_of_delayed_payment)) %>% 
  mutate(num_of_delayed_payment = as.integer(num_of_delayed_payment)) %>% 
  mutate(changed_credit_limit = ifelse(is.na(changed_credit_limit), mean(changed_credit_limit, na.rm = TRUE), changed_credit_limit)) %>% 
  mutate(monthly_balance = ifelse(is.na(monthly_balance), mean(monthly_balance, na.rm = TRUE), monthly_balance)) %>% 
  mutate(amount_invested_monthly = ifelse(is.na(amount_invested_monthly), mean(amount_invested_monthly, na.rm = TRUE), amount_invested_monthly))
  
summary(df) 
View(df)
# write.csv(df, "na_handles.csv")
#==========================================================#

### Handling the Outliers ###

### age ###
summary(df$age)

# The bound before the outlier begin
upper_winsor <- quantile(df$age, 0.98)
# The bound where 18 is
lower_winsor <- quantile(df$age, 0.09)

# Turn outlier into NAs
df <- df %>%
  group_by(customer_id) %>% 
  mutate(age = ifelse(between(age, lower_winsor, upper_winsor), age, NA)) 
  
# Fill some data, suspected to be input error
df <- df %>% 
  group_by(customer_id) %>% 
  fill(age, .direction = "updown")

# Remove the outlier
df <- df %>% 
  group_by(customer_id) %>% 
  filter(between(age, lower_winsor, upper_winsor))

boxplot(df$age)
summary(df$age)
View(df)

### annual_income ###
summary(df$annual_income)
boxplot(df$annual_income)

# The bound before the outlier begin
upper_winsor <- quantile(df$annual_income, 0.95)

# Turn outlier into NAs
df <- df %>%
  group_by(customer_id) %>% 
  mutate(annual_income = ifelse(between(annual_income, min(annual_income), upper_winsor), annual_income, NA)) 

# Fill some data, suspected to be input error
df <- df %>% 
  group_by(customer_id) %>% 
  fill(annual_income, .direction = "updown")

# Remove the outlier
df <- df %>% 
  group_by(customer_id) %>% 
  mutate(annual_income = ifelse(is.na(annual_income), upper_winsor, annual_income))

boxplot(df$annual_income)
summary(df$annual_income)
View(dfT)


### monthly_inhand_salary ###
summary(df$monthly_inhand_salary)
boxplot(df$monthly_inhand_salary)

# The bound before the outlier begin
upper_winsor <- quantile(df$monthly_inhand_salary, 0.95)

# Remove the outlier
df <- df %>% 
  group_by(customer_id) %>% 
  mutate(monthly_inhand_salary = ifelse(monthly_inhand_salary >= upper_winsor, upper_winsor, monthly_inhand_salary))


boxplot(df$monthly_inhand_salary)
summary(df$monthly_inhand_salary)
View(df)

### num_bank_accounts ###
summary(df$num_bank_accounts)
boxplot(df$num_bank_accounts)

# The bound before the outlier begin
upper_winsor <- quantile(df$num_bank_accounts, 0.98)
lower_winsor <- quantile(df$num_bank_accounts, 0.05)

# Turn outlier into NAs
df <- df %>%
  group_by(customer_id) %>% 
  mutate(num_bank_accounts = ifelse(between(num_bank_accounts, min(num_bank_accounts), upper_winsor), num_bank_accounts, NA)) %>%   
  mutate(num_bank_accounts = ifelse(num_bank_accounts <= lower_winsor, lower_winsor, num_bank_accounts))
summary(dfT$num_bank_accounts)
# Fill some data, suspected to be input error
df <- df %>% 
  group_by(customer_id) %>% 
  fill(num_bank_accounts, .direction = "updown")

boxplot(df$num_bank_accounts)
summary(df$num_bank_accounts)
View(df)


### num_credit_card ###
summary(df$num_credit_card)
boxplot(df$num_credit_card)

# The bound before the outlier begin
upper_winsor <- quantile(df$num_credit_card, 0.97)

# Turn outlier into NAs
df <- df %>%
  group_by(customer_id) %>% 
  mutate(num_credit_card = ifelse(between(num_credit_card, min(num_credit_card), upper_winsor), num_credit_card, NA))
summary(df$num_credit_card)
# Fill some data, suspected to be input error
df <- df %>% 
  group_by(customer_id) %>% 
  fill(num_credit_card, .direction = "updown")

boxplot(df$num_credit_card)
summary(df$num_credit_card)
View(df)

### interest_rate ###
summary(df$interest_rate)
boxplot(df$interest_rate)
# The bound before the outlier begin
upper_winsor <- quantile(dfT$interest_rate, 0.95)


# Turn outlier into NAs
df <- df %>%
  group_by(customer_id) %>% 
  mutate(interest_rate = ifelse(interest_rate >= upper_winsor, NA, interest_rate))

# Fill some data, suspected to be input error
df <- df %>% 
  group_by(customer_id) %>% 
  fill(interest_rate, .direction = "updown")

summary(df$interest_rate)

# Remove the outlier
df <- df %>% 
  group_by(customer_id) %>% 
  mutate(interest_rate = ifelse(is.na(interest_rate), upper_winsor, interest_rate))

boxplot(df$interest_rate)
summary(df$interest_rate)
View(df)

### num_of_loan ###

summary(df$num_of_loan)
boxplot(df$num_of_loan)
View(df)

# The bound before the outlier begin
upper_winsor <- quantile(df$num_of_loan, 0.95)
lower_winsor <- quantile(df$num_of_loan, 0.05)


# Turn outlier into NAs
df <- df %>%
  group_by(customer_id) %>% 
  mutate(num_of_loan = ifelse(between(num_of_loan, lower_winsor, upper_winsor), num_of_loan, NA))

# Fill some data, suspected to be input error
df <- df %>% 
  group_by(customer_id) %>% 
  fill(num_of_loan, .direction = "updown")

summary(df$num_of_loan)

# Remove the outlier
df <- df %>% 
  group_by(customer_id) %>% 
  mutate(num_of_loan = ifelse(is.na(num_of_loan), upper_winsor, interest_rate))

boxplot(df$num_of_loan)
summary(df$num_of_loan)
View(df)

### delay_from_due_date ###
dfT = df
summary(dfT$delay_from_due_date)
boxplot(dfT$delay_from_due_date)
View(dfT)

# The bound before the outlier begin
upper_winsor <- quantile(dfT$delay_from_due_date, 0.95)


# Turn outlier into NAs
dfT <- dfT %>%
  group_by(customer_id) %>% 
  mutate(delay_from_due_date = ifelse(delay_from_due_date < 0, NA, delay_from_due_date)) 

# Fill some data, suspected to be input error
dfT <- dfT %>% 
  group_by(customer_id) %>% 
  fill(delay_from_due_date, .direction = "updown")

summary(dfT$delay_from_due_date)

# Remove the outlier
dfT <- dfT %>% 
  group_by(customer_id) %>% 
  mutate(delay_from_due_date = ifelse(delay_from_due_date >= upper_winsor, NA, delay_from_due_date)) %>% 
  mutate(delay_from_due_date = ifelse(is.na(delay_from_due_date), upper_winsor, delay_from_due_date))

boxplot(dfT$delay_from_due_date)
summary(dfT$delay_from_due_date)
View(dfT)


### delay_from_due_date ###
dfT = df
summary(dfT$num_of_delayed_payment)
boxplot(dfT$num_of_delayed_payment)
View(dfT)

# The bound before the outlier begin
upper_winsor <- quantile(dfT$num_of_delayed_payment, 0.989)

# Turn outlier into NAs
dfT <- dfT %>%
  group_by(customer_id) %>% 
  mutate(num_of_delayed_payment = ifelse(num_of_delayed_payment < 0, 0, num_of_delayed_payment)) 

summary(dfT$num_of_delayed_payment)

# Remove the outlier
dfT <- dfT %>% 
  group_by(customer_id) %>% 
  mutate(num_of_delayed_payment = ifelse(num_of_delayed_payment >= upper_winsor, NA, num_of_delayed_payment)) %>% 
  mutate(num_of_delayed_payment = ifelse(is.na(num_of_delayed_payment), upper_winsor, num_of_delayed_payment))

boxplot(dfT$num_of_delayed_payment)
summary(dfT$num_of_delayed_payment)
View(dfT)

### changed_credit_limit ###
dfT = df
summary(dfT$changed_credit_limit)
boxplot(dfT$changed_credit_limit)
View(dfT)

# The bound before the outlier begin
upper_winsor <- quantile(dfT$changed_credit_limit, 0.95)

# Turn outlier into NAs
dfT <- dfT %>%
  group_by(customer_id) %>% 
  mutate(changed_credit_limit = ifelse(changed_credit_limit < 0, NA, changed_credit_limit)) 

# Fill some data, suspected to be input error
dfT <- dfT %>% 
  group_by(customer_id) %>% 
  fill(changed_credit_limit, .direction = "updown")

summary(dfT$changed_credit_limit)

# Remove the outlier
dfT <- dfT %>% 
  group_by(customer_id) %>% 
  mutate(changed_credit_limit = ifelse(changed_credit_limit >= upper_winsor, NA, changed_credit_limit)) %>% 
  mutate(changed_credit_limit = ifelse(is.na(changed_credit_limit), upper_winsor, changed_credit_limit))

boxplot(dfT$changed_credit_limit)
summary(dfT$changed_credit_limit)
View(dfT)

### num_credit_inquiries ###
dfT = df
summary(dfT$num_credit_inquiries)
boxplot(dfT$num_credit_inquiries)
View(dfT)

# The bound before the outlier begin
upper_winsor <- quantile(dfT$num_credit_inquiries, 0.98)

# Turn outlier into NAs
dfT <- dfT %>%
  group_by(customer_id) %>% 
  mutate(num_credit_inquiries = ifelse(num_credit_inquiries >= upper_winsor, NA, num_credit_inquiries)) 

# Fill some data, suspected to be input error
dfT <- dfT %>% 
  group_by(customer_id) %>% 
  fill(num_credit_inquiries, .direction = "updown")

summary(dfT$num_credit_inquiries)

boxplot(dfT$num_credit_inquiries)
summary(dfT$num_credit_inquiries)
View(dfT)

### outstanding_debt ###
dfT = df
summary(dfT$outstanding_debt)
boxplot(dfT$outstanding_debt)
View(dfT)

# The bound before the outlier begin
upper_winsor <- quantile(dfT$outstanding_debt, 0.935)

# Turn outlier into NAs
dfT <- dfT %>%
  group_by(customer_id) %>% 
  mutate(outstanding_debt = ifelse(outstanding_debt >= upper_winsor, NA, outstanding_debt)) 

# Fill some data, suspected to be input error
dfT <- dfT %>% 
  group_by(customer_id) %>% 
  fill(outstanding_debt, .direction = "updown")

summary(dfT$outstanding_debt)

# Remove the outlier
dfT <- dfT %>% 
  group_by(customer_id) %>%  
  mutate(outstanding_debt = ifelse(is.na(outstanding_debt), upper_winsor, outstanding_debt))

boxplot(dfT$outstanding_debt)
summary(dfT$outstanding_debt)
View(dfT)



### credit utilization ratio ###
dfT = df
summary(dfT$credit_utilization_ratio)
boxplot(dfT$credit_utilization_ratio)
View(dfT)


# The bound before the outlier begin
upper_winsor <- quantile(dfT$credit_utilization_ratio, 0.95)

# Turn outlier into NAs
dfT <- dfT %>%
  group_by(customer_id) %>% 
  mutate(credit_utilization_ratio = ifelse(credit_utilization_ratio >= upper_winsor, upper_winsor, credit_utilization_ratio)) 

# Fill some data, suspected to be input error
dfT <- dfT %>% 
  group_by(customer_id) %>% 
  fill(outstanding_debt, .direction = "updown")

summary(dfT$outstanding_debt)

# Remove the outlier
dfT <- dfT %>% 
  group_by(customer_id) %>%  
  mutate(outstanding_debt = ifelse(is.na(outstanding_debt), upper_winsor, outstanding_debt))

boxplot(dfT$credit_utilization_ratio)
summary(dfT$credit_utilization_ratio)
View(dfT)


### credit utilization ratio ###
dfT = df
summary(dfT$total_emi_per_month)
boxplot(df$total_emi_per_month)
View(dfT)


# The bound before the outlier begin
upper_winsor <- quantile(dfT$total_emi_per_month, 0.95)

# Turn outlier into NAs
dfT <- dfT %>%
  group_by(customer_id) %>% 
  mutate(total_emi_per_month = ifelse(total_emi_per_month >= upper_winsor, NA, total_emi_per_month)) 

summary(dfT$total_emi_per_month)
# Fill some data, suspected to be input error
dfT <- dfT %>% 
  group_by(customer_id) %>% 
  fill(total_emi_per_month, .direction = "updown")

summary(dfT$total_emi_per_month)

# Remove the outlier
dfT <- dfT %>% 
  group_by(customer_id) %>%  
  mutate(total_emi_per_month = ifelse(is.na(total_emi_per_month), upper_winsor, total_emi_per_month))

boxplot(dfT$total_emi_per_month)
summary(dfT$total_emi_per_month)
View(dfT)

### amount_invested_monthly ###
dfT = df
summary(dfT$amount_invested_monthly)
boxplot(df$amount_invested_monthly)
View(dfT)


# The bound before the outlier begin
upper_winsor <- quantile(dfT$amount_invested_monthly, 0.89)

# Turn outlier into NAs
dfT <- dfT %>%
  group_by(customer_id) %>% 
  mutate(amount_invested_monthly = ifelse(amount_invested_monthly >= upper_winsor, upper_winsor, amount_invested_monthly)) 

summary(dfT$amount_invested_monthly)
# Fill some data, suspected to be input error
dfT <- dfT %>% 
  group_by(customer_id) %>% 
  fill(amount_invested_monthly, .direction = "updown")

summary(dfT$amount_invested_monthly)

# Remove the outlier
dfT <- dfT %>% 
  group_by(customer_id) %>%  
  mutate(total_emi_per_month = ifelse(is.na(total_emi_per_month), upper_winsor, total_emi_per_month))

boxplot(dfT$amount_invested_monthly)
summary(dfT$total_emi_per_month)
View(dfT)






### monthly_balance ###
dfT = df
summary(dfT$monthly_balance)
boxplot(df$monthly_balance)
View(dfT)


# The bound before the outlier begin
upper_winsor <- quantile(dfT$monthly_balance, 0.925)

# Turn outlier into NAs
dfT <- dfT %>%
  group_by(customer_id) %>% 
  mutate(monthly_balance = ifelse(monthly_balance >= upper_winsor, upper_winsor, monthly_balance)) 

summary(dfT$amount_invested_monthly)
# Fill some data, suspected to be input error
dfT <- dfT %>% 
  group_by(customer_id) %>% 
  fill(amount_invested_monthly, .direction = "updown")

summary(dfT$amount_invested_monthly)

# Remove the outlier
dfT <- dfT %>% 
  group_by(customer_id) %>%  
  mutate(total_emi_per_month = ifelse(is.na(total_emi_per_month), upper_winsor, total_emi_per_month))

boxplot(dfT$monthly_balance)
summary(dfT$total_emi_per_month)
View(dfT)
