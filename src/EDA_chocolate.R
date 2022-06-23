library(tidyverse)
library(janitor)
library(corrplot)

# Import dataset
tuesdata <- tidytuesdayR::tt_load('2022-01-18')
chocolate <- tuesdata$chocolate



# stats summary
glimpse(chocolate)
summary(chocolate)

# Problems in the data
## Nulls --- Rename to -> Not known -- DONE
## Text cleaning (ingredients, characteristics) -- DONE
## Duplicates -- DONE
## Percentage chr > dbl (percent) -- DONE

get_dupes(chocolate) # No duplicates

# Check for nulls
lapply(chocolate,function(x) { length(which(is.na(x)))}) # ingredients: 87
 


chocolate$cocoa_percent <- (as.double(sub("%", "", chocolate$cocoa_percent))) # Percentage chr > dbl

# Separate ingredients to multiple columns
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

chocolate[is.na(chocolate)] = 0 # reassign NA values to zero


## EDA

# Explain what blend is
chocolate %>% 
  filter(country_of_bean_origin == "Blend") -> Blend


# correlation matrix
chocolate %>% 
  select(rating, cocoa_percent, beans, sugar, 
         sweetener, cocoa_butter, vanilla, lecithin, salt) -> num_chocolate

corrplot(cor(num_chocolate), method = "color", type = "lower", is.corr = TRUE, mar=c(0,0,1,0),
         title = 'Correlation Map for Rating and Other Variables',addCoef.col = TRUE,
         tl.cex = 0.6, tl.col = 'black', number.cex=0.5)

# Bar chart chocolate origin frequency
# Sara
ggplot(chocolate, aes(x = country_of_bean_origin)) + 
  geom_histogram(stat = "count") + 
  coord_flip() 

# Limit to those with at least 50 observations
# Nourah
chocolate %>% 
  group_by(country_of_bean_origin) %>% 
  count(country_of_bean_origin) %>% 
  filter(n > 50) %>%
  ggplot(aes(x = country_of_bean_origin, y = n, fill = country_of_bean_origin)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_minimal() + 
  theme(legend.position = "None") +
  labs(x = 'Bean Origin', y = 'Count', title = 'Most Frequently Used Broad Bean Origins', 
       caption = "Only countires with more than 50 observations")

# Pie chart: chocolate origin frequency limited to those with at least 100 observations
#Nourah

chocolate %>%
  group_by(country_of_bean_origin) %>% # Group by origin
  filter(n() > 100) %>% # Limit to those with at least 100 observations
  mutate(count = n()) %>% 
  ggplot(aes(x = "", y = count, fill = country_of_bean_origin)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Most frequently used broad bean origins", caption = "only countires with more than 100 observations") +
  scale_fill_discrete(name="Countries")




# Frequency of company location's 
# Hanadi

chocolate %>% 
  group_by(company_location) %>%
  filter(n() > 100) %>% 
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(company_location, count), fill = company_location)) + 
  geom_bar() + 
  coord_flip() + 
  theme_minimal() +
  theme(legend.position = "None") +
  labs(x = 'Location', y = 'Count', title = 'Top 4 Companies Locations')


# Average of ratings per years Hanadi
ggplot(chocolate, aes(review_date, rating)) +
  geom_jitter(width = 0.15, shape = 16, alpha = 0.25, color = "#DD2A7B") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), col = "#4073FF") +
  scale_x_continuous(breaks = seq(2006, 2021, 2)) +
  labs(x = 'Year', y = 'Rating', title = 'Average Rating Over the Years')


# Company locations per high quality chocolate (bar chart x = company_location, y = mean(rating)) 
# Sara
# Top ten  
chocolate %>% 
  group_by(company_location) %>% # Grouping data by company_location
  summarize(avg_rating = mean(rating)) -> company_loc # Calculating average rating for each company_location
                                                      # Assigning the value to a variable 'company_loc' for further handling
company_loc <- arrange(company_loc, desc(avg_rating)) # Arranging values in a descending order by the average rating
ggplot(company_loc[1:10,], aes(x = reorder(company_location, avg_rating, decreasing = TRUE), y = avg_rating, fill = company_location)) + 
  geom_bar(stat="identity", width = 0.8, position = position_dodge(width=5)) +
  theme(legend.position = "None") + 
  labs(title = "Top 10 company locations per high quality chocolate", x = "Company location", y = "Average rating")

# Bottom Five
chocolate %>% 
  group_by(company_location) %>% # Grouping data by company_location
  summarize(avg_rating = mean(rating)) -> company_loc # Calculating average rating for each company_location
# Assigning the value to a variable 'company_loc' for further handling
company_loc <- arrange(company_loc, avg_rating) # Arranging values in a descending order by the average rating
ggplot(company_loc[1:5,], aes(x = reorder(company_location, avg_rating, decreasing = FALSE), y = avg_rating, fill = company_location)) + 
  geom_bar(stat="identity", width = 0.8, position = position_dodge(width=5)) +
  theme(legend.position = "None") + 
  labs(title = "Bottom 5 company locations per high quality chocolate", x = "Company location", y = "Average rating")


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



# Company location vs cocoa percent 
# Hanadi
chocolate %>% 
  group_by(company_location) %>%
  filter(n() > 50) %>% 
  ggplot(aes(company_location, cocoa_percent)) +
  geom_jitter(width = 0.15, shape = 16, alpha = 0.25, color = "#DD2A7B") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), col = "#4073FF") +
  labs(x = 'Company location', y = 'Cocoa percent', title = 'Average cocoa percent per company location')

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

ggplot(origin_rating[1:5,], aes(x = reorder(country_of_bean_origin, avg_rating, decreasing = TRUE), y = avg_rating, fill = country_of_bean_origin)) + 
  geom_bar(stat="identity", width = 0.8, position = position_dodge(width=5)) 

# Boxplot (not likely to use)
chocolate %>% 
  group_by(country_of_bean_origin) %>%
  ggplot(aes(factor(country_of_bean_origin), `rating`)) +
  geom_boxplot()
  

# Cocoa Percent per Rating 
# Nourah
ggplot(chocolate, aes(cocoa_percent, rating)) +
  geom_jitter(width = 0.25, alpha = 0.5, color = "#DD2A7B") +
  geom_smooth(method = "lm", se = FALSE, color = "#4073FF") + 
  labs(title = "Cocoa Percent per Rating", x = "Cocoa percent", y = "Rating")

# Ingredients vs Rating  
# Nourah
# this is the distribution but it doesn't say much

chocolate %>% 
  select(beans, sugar, sweetener, cocoa_butter, vanilla, lecithin, salt, cocoa_percent, review_date, rating) %>% 
  pivot_longer(cols = c(-cocoa_percent, -review_date, -rating) ,names_to = "ingredient", values_to = "is_ingredient") -> ingredients

ingredients %>% 
  group_by(ingredient) %>% 
  filter(is_ingredient == 1) %>% 
  ggplot(aes(x = rating, fill = ingredient)) +
  geom_bar() +
  labs(title = "Ingredient Distribution in Each Rating", x = "Rating", y = "Count", fill="Ingredients")

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
  facet_wrap(~ingredient) +
  labs(x = "Rating", y = "Density", fill="Ingredients") +
  ggtitle("Distribution of Individual Ingredients")
# How did the taste of customers change overtime? (ingredients per years):

# How did the cocoa percent change overtime? 
# Hanadi
ggplot(chocolate, aes(review_date, cocoa_percent)) +
  geom_jitter(alpha = 0.3, color = "#DD2A7B") +
  labs(x = 'Year', y = 'Cocoa percent', title = 'Amount of cocoa percent over the years')

#Average cocoa% over the years?
ggplot(chocolate, aes(review_date, cocoa_percent)) +
  geom_jitter(width = 0.2, shape = 1, color = "#DD2A7B") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), col = "#4073FF") +
  labs(x = 'Year', y = 'Cocoa percent', title = 'Amount of cocoa percent over the years') 
### Both don't seem very informative, basically not much change over the years.


# ------------------------

# Hypothesis testing:
# H0 => rating has no relation with cocoa percent 
# H0 => rating has no relation with type of sweetener
# H0 => rating has no relation with cocoa origin
# H0 => rating doesn't change overtime



# H0 => rating has no relation with type of sweetener
# no information
ggplot(chocolate, aes(x = 1, y = rating)) +
  geom_jitter(width = 0.2) +
  labs(title = "No information from X at all")

# all information
ingredients %>% 
  filter(ingredient == "sugar" | ingredient == "sweetener") %>% 
  group_by(ingredient) %>% 
  filter(is_ingredient == 1) %>% 
  select(ingredient, cocoa_percent, review_date, rating) -> type_sweetener

type_sweetener %>% 
  group_by(ingredient) %>% 
  ggplot(aes(x = ingredient, y = rating)) +
  geom_jitter(width = 0.2) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), col = "#4073FF")
  coord_cartesian() +
  labs(title = "ALL information from X that is available")

# Degree of Freedom 
type_sweetener %>% 
  count(ingredient)
type_sweetener %>%
  group_by(ingredient) %>% 
  summarise(var(rating))

# two smaple t-test
t.test(rating ~ ingredient, data = type_sweetener)
# We reject the null hypothesis!

# H0 => rating has no relation with cocoa origin
chocolate[chocolate$country_of_bean_origin == "Venezuela" | chocolate$country_of_bean_origin == "Blend",] -> bean_origin
  
select(bean_origin, country_of_bean_origin, rating) -> bean_origin_two_sample

unique(bean_origin_two_sample$country_of_bean_origin)

t.test(rating ~ country_of_bean_origin, data = bean_origin_two_sample)

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

flavors_two_sample %>% 
  count(characteristic)
flavors_two_sample %>%
  group_by(characteristic) %>% 
  summarise(var(rating))

t.test(rating ~ characteristic  , data = flavors_two_sample)


# Linear regression
## no information 
Sugar_mean <- mean(type_sweetener$rating[type_sweetener$ingredient == "sugar"])
Sugar_mean

type_sweetener %>% 
  filter(ingredient == "sugar") %>% 
  ggplot(aes(x = 1, y = rating)) +
  geom_jitter(width = 0.5) +
  geom_hline(aes(yintercept = Sugar_mean, color = "Null model (y-bar)")) +
  coord_cartesian(xlim = c(0,2)) +
  labs(title = "No information from X at all")

## All information
type_sweetener %>% 
  filter(ingredient == "sugar") %>% 
  ggplot(aes(x = cocoa_percent, y = rating)) +
  geom_jitter(width = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "ALL information from X (cocoa_percent for sugar)")

type_sweetener %>% 
  filter(ingredient == "sweetener") %>% 
  ggplot(aes(x = cocoa_percent, y = rating)) +
  geom_jitter(width = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "ALL information from X (cocoa_percent for sugar)")


type_sweetener %>% 
  filter(ingredient == "sugar") %>% 
  ggplot(aes(x = review_date, y = rating)) +
  geom_jitter(width = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "ALL information from X (review_date for sugar)")

