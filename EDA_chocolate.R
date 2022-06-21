library(tidyverse)
library(janitor)

# Import dataset
tuesdata <- tidytuesdayR::tt_load('2022-01-18')
chocolate <- tuesdata$chocolate

# stats summary
glimpse(chocolate)
summary(chocolate)

## Problems in the data
# Nulls --- Rename to -> Not known -- DONE
# Text cleaning (ingredients, characteristics) -- 
# Duplicates -- DONE
# Percentage chr > dbl (percent) -- DONE

get_dupes(chocolate)# No duplicates

# Rename Nulls
lapply(chocolate,function(x) { length(which(is.na(x)))}) 

chocolate$cocoa_percent <- (as.double(sub("%", "", chocolate$cocoa_percent)))/100 # Percentage chr > dbl

chocolate$ingredients = substr(chocolate$ingredients, 4, nchar(chocolate$ingredients))
chocolate$ingredients

chocolate %>% 
  separate(ingredients, c("B", "S", "S*", "C", "V", "L", "Sa"), ",") -> chocolate

chocolate %>% 
  mutate(beans = as.integer(!is.na(B)), sugar = as.integer(!is.na(S)), 
         sweetener = as.integer(!is.na(`S*`)), cocoa_butter= as.integer(!is.na(C)), 
         vanilla = as.integer(!is.na(V)),  lecithin = as.integer(!is.na(L)),  salt = as.integer(!is.na(Sa))) -> chocolate
  
chocolate %>% 
  select(-B, -S, -`S*`, -C, -V, -L, -Sa) -> chocolate

## EDA

# Explain what blend is
chocolate %>% 
  filter(country_of_bean_origin == "Blend") -> Blend


# correlation matrix
chocolate %>% 
  select(rating, cocoa_percent, beans, sugar, 
         sweetener, cocoa_butter, vanilla, lecithin, salt) -> num_chocolate

corrplot(cor(num_chocolate), method = "color", type = "lower", is.corr = TRUE, mar=c(0,0,1,0),
         title = 'Correlation Map for Dataset Variables',addCoef.col = TRUE,
         tl.cex = 0.6, tl.col = 'black', number.cex=0.5)

# Bar chart chocolate origin frequency (top 3-5) (country (3-5), count(bean_origin))
# Sara
ggplot(chocolate, aes(x = country_of_bean_origin)) + 
  geom_histogram(stat = "count") + 
  coord_flip() 

# Nourah
chocolate %>% 
  group_by(country_of_bean_origin) %>% # Group by origin
  filter(n() > 50) %>% # Limit to those with at least 50 observations
  mutate(count = n()) %>% # Add the count column
  ggplot(aes(x = reorder(country_of_bean_origin, count))) + 
  geom_bar() + 
  coord_flip() + 
  theme_minimal() + 
  labs(x = 'Bean origin', y = 'Count', title = 'Most frequently used broad bean origins', caption = "only countires with more than 50 observations")
  

# Pie chart

# chocolate %>% 
#   group_by(country_of_bean_origin) %>% # Group by origin
#   filter(n() > 50) %>% # Limit to those with at least 50 observations
#   mutate(count = n()) %>% # Add the count column
#   ggplot(aes(x = reorder(country_of_bean_origin, count))) + 
#   geom_bar() + 


# Frequency of company location's Hanadi


# Average of ratings per years Hanadi


# Company locations per high quality chocolate (bar chart x = countries, y = mean(rating)) Sara

# Company location vs cocoa percent Refal

## Research Questions
# -----------------------
# What contributes to a high rating? 
      # Country of origin Sara
      # Cocoa Percent Nourah
      # Ingredients Nourah
      
# How did the taste of customers change overtime? (ingredients per years) Refal

# How did the cocoa persent change overtime? Hanadi

# Check for outliers... Refal

# ------------------------

# Hypothesis testing:
# H0 => rating has no relation with cocoa percent
# H0 => rating has no relation with ingredients
# H0 => rating has no relation with cocoa origin
# H0 => rating doesn't change overtime