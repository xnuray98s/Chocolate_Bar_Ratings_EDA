library(tidyverse)
library(janitor)
library(corrplot)

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
         tl.cex = 0.6, tl.col = 'black', number.cex=0.1)

# Bar chart chocolate origin frequency (top 3-5) (country (3-5), count(bean_origin))
# Sara


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

chocolate %>%
  group_by(country_of_bean_origin) %>% # Group by origin
  filter(n() > 50) %>% # Limit to those with at least 50 observations
  mutate(count = n()) %>% # Add the count column
  ggplot(aes(x = reorder(country_of_bean_origin, count))) +
  geom_bar() +
  NULL





# Frequency of company location's Hanadi


# Average of ratings per years Hanadi


# Company locations per high quality chocolate (bar chart x = company_location, y = mean(rating)) Sara
# Top ten  
chocolate %>% 
  group_by(company_location) %>% # Grouping data by company_location
  summarize(avg_rating = mean(rating)) -> company_loc # Calculating average rating for each company_location
                                                      # Assigning the value to a variable 'company_loc' for further handling
company_loc <- arrange(company_loc, desc(avg_rating)) # Arranging values in a descending order by the average rating
ggplot(company_loc[1:10,], aes(x = reorder(company_location, avg_rating, decreasing = TRUE), y = avg_rating)) + 
  geom_bar(stat="identity", width = 0.8, position = position_dodge(width=5)) 

# Bottom Five
chocolate %>% 
  group_by(company_location) %>% # Grouping data by company_location
  summarize(avg_rating = mean(rating)) -> company_loc # Calculating average rating for each company_location
# Assigning the value to a variable 'company_loc' for further handling
company_loc <- arrange(company_loc, avg_rating) # Arranging values in a descending order by the average rating
ggplot(company_loc[1:5,], aes(x = reorder(company_location, avg_rating, decreasing = FALSE), y = avg_rating)) + 
  geom_bar(stat="identity", width = 0.8, position = position_dodge(width=5)) 

# Here, using max rating which is not more informative than avg 
# ---------------
# chocolate %>% 
#   group_by(company_location) %>% # Grouping data by company_location
#   summarize(max_rating = max(rating)) -> company_loc # Calculating average rating for each company_location
# # Assigning the value to a variable 'company_loc' for further handling
# 
# company_loc <- arrange(company_loc, desc(max_rating)) # Arranging values in a descending order by the average rating
# 
# ggplot(company_loc[1:10,], aes(x = reorder(company_location, max_rating, decreasing = TRUE), y = max_rating)) + 
#   geom_bar(stat="identity", width = 0.8, position = position_dodge(width=5)) 



# Company location vs cocoa percent Refal

## Research Questions
# -----------------------
# What contributes to a high rating? 
      # Country of origin Sara
# Barplot
chocolate %>% 
  group_by(country_of_bean_origin) %>% # Grouping data by company_location
  summarize(avg_rating = mean(rating)) -> origin_rating # Calculating average rating for each company_location
# Assigning the value to a variable 'company_loc' for further handling

origin_rating <- arrange(country_of_bean_origin, avg_rating) # Arranging values in a descending order by the average rating

ggplot(origin_rating[1:5,], aes(x = reorder(country_of_bean_origin, avg_rating, decreasing = TRUE), y = avg_rating)) + 
  geom_bar(stat="identity", width = 0.8, position = position_dodge(width=5)) 

# Boxplot (not likely to use)
chocolate %>% 
  group_by(country_of_bean_origin) %>%
  ggplot(aes(factor(country_of_bean_origin), `rating`)) +
  geom_boxplot()
  

      # Cocoa Percent Nourah
      # Ingredients Nourah
      
# How did the taste of customers change overtime? (ingredients per years) Refal

# How did the cocoa persent change overtime? Hanadi

# Check for outliers... Refal

# ------------------------

# Hypothesis testing:
# H0 => rating has no relation with cocoa percent -- cont ~ cont / OLS
# H0 => rating has no relation with ingredients -- cont ~ cat /1 or 2-sample t-test + one way ANOVA
# H0 => rating has no relation with cocoa origin -- cont ~ cat /1 or 2-sample t-test + one way ANOVA
# H0 => rating doesn't change overtime -- cont ~ cont / OLS