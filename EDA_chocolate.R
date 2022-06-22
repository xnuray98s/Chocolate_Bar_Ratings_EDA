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

# Check for nulls
lapply(chocolate,function(x) { length(which(is.na(x)))})

chocolate[is.na(chocolate)] = 0

chocolate$cocoa_percent <- (as.double(sub("%", "", chocolate$cocoa_percent))) # Percentage chr > dbl

chocolate$ingredients = substr(chocolate$ingredients, 3, nchar(chocolate$ingredients))
chocolate$ingredients = trimws(chocolate$ingredients, which = "both")
chocolate %>% 
  separate(ingredients, c("str1", "str2", "str3", "str4", "str5", "str6"), ",") -> chocolate

chocolate %>% 
  mutate(beans = as.integer(str1 == "B"), sugar = as.integer(str2 == "S"), 
         sweetener = as.integer(str2 == "S*" | str3 == "S*"), cocoa_butter= as.integer(str2 == "C" | str3 == "C" | str4 == "C"), 
         vanilla = as.integer(str2 == "V" | str3 == "V" | str4 == "V" | str5 == "V"),  lecithin = as.integer(str2 == "L" | str3 == "L" | str4 == "L" | str5 == "L" | str6 == "L"),  salt = as.integer(str2 == "Sa" | str3 == "Sa" | str4 == "Sa" | str5 == "Sa" | str6 == "Sa")) -> chocolate

chocolate %>% 
  select(-str1, -str2, -str3, -str4, -str5, -str6) -> chocolate



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
ggplot(chocolate, aes(x = country_of_bean_origin)) + 
  geom_histogram(stat = "count") + 
  coord_flip() 

# Nourah
chocolate %>% 
  group_by(country_of_bean_origin) %>% # Group by origin
  count(country_of_bean_origin) %>% 
  filter(n > 50) %>% # Limit to those with at least 50 observations
  mutate(count = n / nrow(chocolate)) %>% # Add the count column
  ggplot(aes(x = reorder(country_of_bean_origin, count))) + 
  geom_bar() + 
  coord_flip() + 
  theme_minimal() + 
  labs(x = 'Bean origin', y = 'Count', title = 'Most frequently used broad bean origins', caption = "only countires with more than 500 observations")

# Pie chart Nourah

chocolate %>%
  group_by(country_of_bean_origin) %>% # Group by origin
  filter(n() > 50) %>% # Limit to those with at least 50 observations
  mutate(count = n()) %>% 
  ggplot(aes(x = "", y = count, fill = country_of_bean_origin)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void()




# Frequency of company location's Hanadi


chocolate %>% 
  group_by(company_location) %>%
  filter(n() > 50) %>% 
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



# Company location vs cocoa percent Hanadi
chocolate %>% 
  group_by(company_location) %>%
  filter(n() > 50) %>% 
  ggplot(aes(company_location, cocoa_percent)) +
  geom_jitter(width = 0.15, shape = 16, alpha = 0.25) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), col = "blue") +
  labs(x = 'Company location', y = 'Cocoa%', title = 'Average cocoa% in company location')

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
  

# Cocoa Percent vs Rating Nourah
ggplot(chocolate, aes(rating, cocoa_percent)) +
  geom_jitter(width = 0.25, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)

# Ingredients vs Rating  Nourah
# this is the distribution but it doesn't say much

chocolate %>% 
  select(beans, sugar, sweetener, cocoa_butter, vanilla, lecithin, salt, cocoa_percent, review_date, rating) %>% 
  pivot_longer(cols = c(-cocoa_percent, -review_date, -rating) ,names_to = "ingredient", values_to = "is_ingredient") -> ingredients

ingredients %>% 
  group_by(ingredient) %>% 
  filter(is_ingredient == 1) %>% 
  ggplot(aes(x = rating, fill = ingredient)) +
  geom_bar()

# Outliers
ingredients %>% 
  group_by(ingredient) %>% 
  filter(is_ingredient == 1) %>% 
  ggplot(aes(x = ingredient, y = rating, fill = ingredient)) +
  geom_boxplot()

ingredients %>% 
  group_by(ingredient) %>% 
  filter(is_ingredient == 1) %>% 
  ggplot(aes(x=rating, group=ingredient, fill=ingredient)) +
  geom_density() +
  facet_wrap(~ingredient) 

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
# H0 => rating has no relation with cocoa percent -- OLS cont ~ cont 
# H1 => rating has a relation with cocoa percent -- OLS cont ~ cont
# chocolate %>% 
#   ggplot(aes(x = cocoa_percent, y = rating)) +
#   geom_point() +
#   geom_smooth(method = "lm", color = "red", se = FALSE) +
#   labs(title = "Cocoa (%) vs. Rating")

# ggplot(chocolate, aes(x = cocoa_percent, y = rating)) +
#   geom_point() +
#   geom_smooth(method = "lm", aes(color = "OLS model"), se = FALSE) +
#   geom_hline(aes(yintercept = mean(chocolate$rating), color = "Null model (y-bar)"), size = 1) +
#   labs(title = "Null vs. Fitted Model")
# 
# lm(rating ~ cocoa_percent, data = chocolate) 



# H0 => rating doesn't change overtime -- OLS cont ~ cont 
# H1 => rating changes overtime -- OLS cont ~ cont 

# chocolate %>% 
#   ggplot(aes(x = review_date, y = rating)) +
#   geom_point() +
#   geom_smooth(method = "lm", color = "red", se = FALSE) +
#   labs(title = "Review date vs. Rating")
# 
# lm(rating ~ review_date, data = chocolate) 

## 
# chocolate$rating_cat <- ifelse(chocolate$rating > 2, "High", "Low")
# 
# ggplot(chocolate, aes(x = rating_cat, y = cocoa_percent)) +
#   geom_jitter(width = 0.2, ) +
#   stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), col = "blue") +
#   coord_cartesian() +
#   labs(title = "ALL information from X that is available")
# 
# t.test(rating_cat ~ cocoa_percent, data = chocolate, var.equal = TRUE)


# The p-value is 0.001536. What does this tell me?
#   
#   We reject the null hypothesis!
#   There is evidence that the location (site) is related to the height.
# There is an association between height and site.
# We can use the location to predict height!


nutty <- chocolate[grep("nut", chocolate$most_memorable_characteristics), ]
nutty$characteristic <- rep("Nutty", times = nrow(nutty))

fruity <- chocolate[grep("frui", chocolate$most_memorable_characteristics), ]
fruity$characteristic <- rep("Fruity", times = nrow(fruity))

# H0 => rating has no relation with cocoa origin
flavors <- bind_rows( nutty, fruity) 
select(flavors, characteristic, rating) -> flavors_two_sample
t.test(rating ~ characteristic  , data = flavors_two_sample, var.equal = TRUE)




