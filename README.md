# Chocolate Bar Ratings EDA
## Description

The data this week comes from [Flavors of Cacao](http://flavorsofcacao.com/chocolate_database.html) by way of [Georgios and Kelsey](https://github.com/rfordatascience/tidytuesday/issues/408).

[Github Pages for Chocolate Bar Ratings Report](https://xnuray98s.github.io/Chocolate_Bar_Ratings_EDA/)
## Data

|variable                         |class     |description |
|:--------------------------------|:---------|:-----------|
|ref                              |integer   |Reference ID, The highest REF numbers were the last entries made. |
|company_manufacturer             |character | Manufacturer name |
|company_location                 |character | Manufacturer region |
|review_date                      |integer   | Review date (year) |
|country_of_bean_origin           |character | Country of origin |
|specific_bean_origin_or_bar_name |character | Specific bean or bar name|
|cocoa_percent                    |character | Cocoa percent (% chocolate) |
|ingredients                      |character | Ingredients, ("#" = represents the number of ingredients in the chocolate; B = Beans, S = Sugar, S* = Sweetener other than white cane or beet sugar, C = Cocoa Butter, V = Vanilla, L = Lecithin, Sa = Salt) |
|most_memorable_characteristics   |character | Most Memorable Characteristics column is a summary review of the most memorable characteristics of that bar. Terms generally relate to anything from texture, flavor, overall opinion, etc. separated by ','|
|rating                           |double    | rating between 1-5 |

## Requirements

- ```tidyverse```
- ```dplyr```
- ```janitor```
- ```corrplot```
- ```tufte```
- ```here```
- ```reactable```

## Folder Structure

```bash
./
│   README.md
│   Chocolate_Bar_Ratings.html   
│   Chocolate_Bar_Ratings_EDA.Rproj
│
└───src/
    │   EDA_chocolate.R
    │   Chocolate_Bar_Ratings.Rmd
```
