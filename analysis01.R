library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)

#3.1.0 read dataset refine from .csv file
refine_original <- read.csv ("refine.csv", stringsAsFactors = FALSE, header=TRUE)


#3.1.1 convert $company lower case, find inconsistent values in company column i.e misspelling, 0 instead o o in company and replace with consistent values philips, akzo, van houten, unilever
refine_clean <- refine_original %>% 
  mutate(company = tolower(company))%>%
  mutate(company = ifelse(grepl("^phil|^fil|^phl", company), "philips", company),
         company = ifelse(grepl("^ak", company), "akzo", company),
         company = ifelse(grepl("^van", company), "van houten", company),
         company = ifelse(grepl("^uni", company), "unilever", company))


#3.1.2 separate product code and number, creating two new variables product_code, product_number, separated at "-"
refine_clean <- refine_clean %>% 
  separate(Product.code...number, c("product_code","product_number"), sep = "-")


#3.1.3 Add product categories column, when product_code is equal to "p" add value of "Smartphone" to product categories, "v" = "Tv", "x" equals "Laptop, "q" equals "Tablet. 
refine_clean<- refine_clean %>%
  mutate(product_category = case_when(
    product_code == "p" ~ "Smartphone",
    product_code == "v" ~ "Tv",
    product_code == "x" ~ "Laptop",
    product_code == "q" ~ "Tablet"))

#3.1.4 Add full address for geocoding. Add new column full_address that concatenates address, city, country values.
refine_clean <- refine_clean%>% 
  unite("full_address", address, city, country, sep = ",", remove = FALSE)

#3.1.5 create dummy variables for company and product category to create categorical variables that contain binary values to use for analysis.
refine_clean <- refine_clean %>% 
  mutate(company_philips = ifelse(company %in% "philips", 1, 0),
         company_akzo = ifelse(company %in% "akzo", 1, 0),
         company_van_houten = ifelse(company %in% "van houten", 1, 0),
         company_unilever = ifelse(company %in% "unilever", 1,  0),
         product_smartphone = ifelse(product_category %in% "Smartphone", 1, 0),
         product_tv = ifelse(product_category %in% "Tv", 1,  0),
         product_laptop = ifelse(product_category %in% "Laptop",  1, 0),
         product_tablet = ifelse(product_category %in% "Tablet", 1,  0)) 

# write refine_original to project folder. refine_original is the original dataset
write.csv(refine_original, "~/Desktop/data/project01/refine_original.csv")


# write refine_clean to project folder. refine_clean is the cleaned dataset
write.csv(refine_clean, "~/Desktop/data/project01/refine_clean.csv")

