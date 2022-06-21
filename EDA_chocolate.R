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
chocolate$ingredients <- replace_na(chocolate$ingredients, "Unknown")

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

## Descriptive Stats
# Bar chart chocolate origin frequency (top 3-5) (country (3-5), count(bean_origin))

chocolate %>% 
  group_by(country_of_bean_origin) %>% 
  summarize(n = n(), avg_rating = mean(rating)) %>%
  mutate(freq = n/sum(n)) -> bean_origin

arrange(bean_origin, desc(n))

ggplot(chocolate, aes(x = country_of_bean_origin)) + 
  geom_histogram(stat = "count") + 
  coord_flip() 

ggplot(bean_origin[1:5,], aes(x = country_of_bean_origin, y = freq)) + 
  geom_histogram(stat="identity") 

# hist(chocolate$country_of_bean_origin)

# Distribution of ratings (Pie chart showing ratings + percentages)

# Highest production (company locations) of high quality chocolate (bar chart x = countries, y = rating count)



## Research Questions
# What contributes to a high rating? 
      # Country of origin 
      # Cocoa Percent
      
# Chocolate characteristics and what contributes to each characteristic? 

# How did the taste of customers change overtime? (people used to like nuts now they like caramel)

#     __________________

 






# sum(is.na(chocolate$ingredients))
unique(chocolate$company_location)
