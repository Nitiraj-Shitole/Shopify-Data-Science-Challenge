# Shopify-Data-Science-Challenge

On Shopify, we have exactly 100 sneaker shops, and each of these shops sells only one model of shoe. We want to do some analysis of the average order value (AOV). When we look at orders data over a 30 day window, we naively calculate an AOV of $3145.13. Given that we know these shops are selling sneakers, a relatively affordable item, something seems wrong with our analysis.
Think about what could be going wrong with our calculation. 

Think about a better way to evaluate this data.
What metric would you report for this dataset?
What is its value?

shopifydata <- read.csv("2019 Winter Data Science Intern Challenge Data Set - Sheet1.csv")
head(shopifydata) 

order_id	shop_id	user_id	order_amount	total_items	payment_method	created_at
1	53	746	224	2	cash	2017-03-13 12:36:56
2	92	925	90	1	cash	2017-03-03 17:38:52
3	44	861	144	1	cash	2017-03-14 4:23:56
4	18	935	156	1	credit_card	2017-03-26 12:43:37
5	18	883	156	1	credit_card	2017-03-01 4:35:11
6	58	882	138	1	credit_card	2017-03-14 15:25:01
7	87	915	149	1	cash	2017-03-01 21:37:57
8	22	761	292	2	cash	2017-03-08 2:05:38
9	64	914	266	2	debit	2017-03-17 20:56:50
10	52	788	146	1	credit_card	2017-03-30 21:08:26
11	66	848	322	2	credit_card	2017-03-26 23:36:40
12	40	983	322	2	debit	2017-03-12 17:58:30
13	54	799	266	2	credit_card	2017-03-16 14:15:34


glimpse(shopifydata)

Rows: 5,000
Columns: 7
$ order_id       <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, …
$ shop_id        <int> 53, 92, 44, 18, 18, 58, 87, 22, 64, 52, 66, 40, 54, 100…
$ user_id        <int> 746, 925, 861, 935, 883, 882, 915, 761, 914, 788, 848, …
$ order_amount   <int> 224, 90, 144, 156, 156, 138, 149, 292, 266, 146, 322, 3…
$ total_items    <int> 2, 1, 1, 1, 1, 1, 1, 2, 2, 1, 2, 2, 2, 1, 3, 2000, 1, 1…
$ payment_method <chr> "cash", "cash", "cash", "credit_card", "credit_card", "…
$ created_at     <chr> "2017-03-13 12:36:56", "2017-03-03 17:38:52", "2017-03-…
  
The data type of order_id, shop_id and user_id was integer, therefore we need to change them to character type.

shopifydata$order_id <- as.character(shopifydata$order_id)
shopifydata$shop_id <- as.character(shopifydata$shop_id)
shopifydata$user_id <- as.character(shopifydata$user_id)

glimpse(shopifydata)
                       
Rows: 5,000
Columns: 7
$ order_id       <chr> "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"…
$ shop_id        <chr> "53", "92", "44", "18", "18", "58", "87", "22", "64", "…
$ user_id        <chr> "746", "925", "861", "935", "883", "882", "915", "761",…
$ order_amount   <int> 224, 90, 144, 156, 156, 138, 149, 292, 266, 146, 322, 3…
$ total_items    <int> 2, 1, 1, 1, 1, 1, 1, 2, 2, 1, 2, 2, 2, 1, 3, 2000, 1, 1…
$ payment_method <chr> "cash", "cash", "cash", "credit_card", "credit_card", "…
$ created_at     <chr> "2017-03-13 12:36:56", "2017-03-03 17:38:52", "2017-03-…
  
# Checking any missing value.
sum(is.na(data))
[1] 0
No missing values were found in dataset.

summary(data$order_amount)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
90     163     284    3145     390  704000
Summary statistics of the order amount, showing the mean, minimum, maximum and interquartile values

mean(data$order_amount)
[1] 3145.128
The above mean value calculated indicated that the total order amount for all the shops was calculated, when in reality we were trying to calculate
the average order value (AOV), which is the amount spent on each pair of sneakers over a period of 30 days.

Rather than evaluating the AOV by dividing the revenue by the number of orders, a better method is to divide the revenue by the number of orders made by each 
sneaker shop.

#Piping
data_aov <- data %>%
        group_by(shop_id) %>%
        summarize(aov_per_shop = sum(order_amount)/sum(total_items)) %>%
 
data_aov %>%
        arrange(aov_per_shop)
 A tibble: 100 x 2
    shop_id aov_per_shop
    <chr>          <dbl>
1 92                90
2 2                 94
3 32               101
4 100              111
5 53               112
6 7                112
7 93               114
8 14               116
9 48               117
10 56               117
 … with 90 more rows


#Descending order
data_aov %>%
        arrange(desc(aov_per_shop))
 A tibble: 100 x 2
    shop_id aov_per_shop
    <chr>          <dbl>
1 78             25725
2 42               352
3 12               201
4 89               196
5 99               195
6 50               193
7 38               190
8 51               187
9 6                187
10 11               184
… with 90 more rows
      
We will exclude the values for Shop 78 as they seem very high and extreme, given that the sneakers are all relatively affordable.
      
filter(data, shop_id == 78) 
   order_id shop_id user_id order_amount total_items payment_method
1       161      78     990        25725           1    credit_card
2       491      78     936        51450           2          debit
3       494      78     983        51450           2           cash
4       512      78     967        51450           2           cash
5       618      78     760        51450           2           cash
6       692      78     878       154350           6          debit
7      1057      78     800        25725           1          debit
8      1194      78     944        25725           1          debit
9      1205      78     970        25725           1    credit_card
10     1260      78     775        77175           3    credit_card
11     1385      78     867        25725           1           cash
12     1420      78     912        25725           1           cash
13     1453      78     812        25725           1    credit_card
14     1530      78     810        51450           2           cash
15     2271      78     855        25725           1    credit_card

           created_at
1   2017-03-12 5:56:57
2  2017-03-26 17:08:19
3  2017-03-16 21:39:35
4   2017-03-09 7:23:14
5  2017-03-18 11:18:42
6  2017-03-27 22:51:43
7  2017-03-15 10:16:45
8  2017-03-16 16:38:26
9  2017-03-17 22:32:21
10  2017-03-27 9:27:20
11 2017-03-17 16:38:06
12 2017-03-30 12:23:43
13 2017-03-17 18:09:54
14  2017-03-29 7:12:01
15 2017-03-14 23:58:22
      
An order amount of $25725 seems to have been inputted for 1 sneaker. Our assumption is that the order amount was inputted incorrectly as cents instead
of dollars. The AOV for shop 78 will be converted into dollars by dividing its aov_per_shop value by 100. 25725/100 = 257.25
      
data_aov <- data_aov %>% 
                mutate(aov_per_shop = case_when(
                        aov_per_shop == 25725 ~ 257.25,
                        TRUE ~ aov_per_shop
                ))
data_aov %>%
        arrange(aov_per_shop)
 A tibble: 100 x 2
   shop_id aov_per_shop
   <chr>          <dbl>
1 92                90
2 2                 94
3 32               101
4 100              111
5 53               112
6 7                112
7 93               114
8 14               116
9 48               117
10 56               117
… with 90 more rows
     

