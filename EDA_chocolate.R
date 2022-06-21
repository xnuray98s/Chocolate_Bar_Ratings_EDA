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

chocolate %>% 
  group_by(company_location) %>%
  filter(n() > 100) %>% 
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(company_location, count))) + 
  geom_bar() + 
  coord_flip() + 
  theme_minimal() +
  labs(x = 'Company location', y = 'Count', title = 'Top 4 company locations')


# Average of ratings per years Hanadi

  ggplot(chocolate, aes(review_date, rating)) +
  geom_jitter(width = 0.15, shape = 16, alpha = 0.25) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), col = "blue") +
  labs(x = 'Year', y = 'Rating', title = 'Average rating over the years')
  
  #How can I show all the years at the x axes?

# Company locations per high quality chocolate (bar chart x = countries, y = mean(rating)) Sara

# Company location vs cocoa percent Refal

## Research Questions
# -----------------------
# What contributes to a high rating? 
      # Country of origin Sara
      # Cocoa Percent Nourah
      # Ingredients Nourah
      
# How did the taste of customers change overtime? (ingredients per years) Refal

# How did the cocoa percent change overtime? Hanadi
  ggplot(chocolate, aes(review_date, cocoa_percent)) +
    geom_jitter(alpha = 0.3) +
    labs(x = 'Year', y = 'Cocoa%', title = 'Amount of cocoa % over the years')
  
  #Average cocoa% over the years?
  ggplot(chocolate, aes(review_date, cocoa_percent)) +
    geom_jitter(width = 0.2, shape = 1) +
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), col = "red") +
    labs(x = 'Year', y = 'Cocoa%', title = 'Amount of cocoa % over the years')
  ### Both don't seem very informative, basically not much change over the years.
  
# Check for outliers... Refal

# ------------------------

# Hypothesis testing:
# H0 => rating has no relation with cocoa percent
# H0 => rating has no relation with ingredients
# H0 => rating has no relation with cocoa origin
# H0 => rating doesn't change overtime