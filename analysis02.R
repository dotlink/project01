library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)

#3.2.0 read dataset refine from .csv file
titanic_original <- read.csv("titanic.csv", stringsAsFactors = FALSE, header=TRUE)

#3.2.1 replace in embarked column an "S" if value is missing in embarked variable. Using diplr
titanic_clean <- titanic_original%>% 
  mutate(embarked = replace(embarked, embarked == "" | is.na(embarked), "S"))

#3.2.2 Calculate the mean of age variable. Use the mean result for missing values in age column
titanic_clean <- titanic_clean%>%
  mutate(age  = replace(age, is.na(age), mean(as.numeric(age), na.rm = TRUE)))

#3.2.3 Replace missing values "" in boat column with "none"
titanic_clean <- titanic_clean%>%
  mutate(boat = replace(boat, boat == "" | is.na(boat), "none"))

#3.2.4 Create new column named has_cabin_number. Populate with a value of 1 if cabin column has a value and 0 if cabin column is missing
titanic_clean <- titanic_clean%>%
  mutate(has_cabin_number = ifelse(cabin %in% "", 0, 1))


# write titanic_original to project folder. titanic_original is the original dataset
write.csv(titanic_original, "~/Desktop/data/project01/titanic_original.csv")


# write titanic_clean to project folder. titanic_clean is the cleaned dataset
write.csv(titanic_clean, "~/Desktop/data/project01/titanic_clean.csv")

  
 







