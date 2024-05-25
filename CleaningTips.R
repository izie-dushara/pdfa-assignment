# Explore
# Clean
# Manipulate
# Describe and Summarise
# Visualize
# Analysis

# Clean data:
# Correct variable types
# Select and filter
# Find and deal with missing data
# Find and deal with duplicates
# Recoding values

# Install packages
library(tidyverse)
# Check default data
data()
View(starwars)

# Variable types

# Overview of the whole datasets
glimpse(starwars)

# Give class of specific field
class(starwars$gender)
# What can be found inside the field
unique(starwars$gender)

# Change the variable class
starwars$gender <- as.factor(starwars$gender)
class(starwars$gender)

levels(starwars$gender)

# To change the order of categorical
starwars$gender <- factor((starwars$gender),
                          levels = c("masculine",
                                     "feminine"))
levels(starwars$gender)

# Select variables
names(starwars)

starwars %>% 
  # ends_with: get shit that end with color
  select(name, height, ends_with("color")) %>% 
  # display the name
  names()

# Filter observation
unique(starwars$hair_color) 
starwars%>% 
  filter(hair_color %in% c("blond", "brown") &
           height < 180)

# Missing data
mean(starwars$height, na.rm = TRUE)

starwars %>% 
  select(name, gender, hair_color, height)

# not recommended
starwars %>% 
  select(name, gender, hair_color, height) %>% 
  na.omit()

# filter and review before dropping
# require understanding of data
starwars %>% 
  select(name, gender, hair_color, height) %>% 
  # Filter stuff that are not complete cases
  filter(!complete.cases(.)) %>% 
  # Drop only one that makes sense to drop
  drop_na(height)

# filter and mutate to a new value
starwars %>% 
  select(name, gender, hair_color, height) %>% 
  # Filter stuff that are not complete cases
  filter(!complete.cases(.)) %>%
  mutate(hair_color = replace_na(hair_color, "none"))

# Duplicate
Names <- c("Peter", "John", "Andrew", "Peter")
Age <- c(22, 33, 44, 22)

friends <- data.frame(Names, Age)

# deals with it with tidyverse
friends %>% 
  distinct()

# Recoding the rows value
starwars %>%  select(name, gender)

starwars %>% 
  select(name, gender) %>% 
  mutate(gender = recode(gender,
                         "masculine" = 1,
                         "feminine" = 2
  )) 

class(starwars$gender)
