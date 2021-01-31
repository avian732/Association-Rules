Association-Rules
================
Avian Robinson
January 30, 2021

\#——————————– \#’ ASSOCIATION RULES \#——————————–

This project leverages Kroger sales data provided by 84.51 consultants
to conduct market basket analysis using R. Association rules are
created, which tell us how likely a customer is to buy a certain brand,
given that they have already purchased different brand(s) within the
data set. Finally, the insights gained in the analysis are considered to
design promotional strategies for Kroger.

``` r
library(dplyr)  
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(arules)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'arules'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    ## The following objects are masked from 'package:base':
    ## 
    ##     abbreviate, write

\#———————————- \#’ PART I: COLLECT THE DATA \#———————————-

``` r
transactions <- read.csv('C:/Users/ajrr1/OneDrive/Documents/R/transactions.csv')
head(transactions)
```

    ##          upc dollar_sales units time_of_transaction geography week household
    ## 1 7680850106         0.80     1                1100         2    1    125434
    ## 2 3620000470         3.59     1                1100         2    1    125434
    ## 3 1800028064         2.25     1                1137         2    1    108320
    ## 4 9999985067         0.85     1                1148         2    1    162016
    ## 5 9999985131         2.19     1                1323         2    1     89437
    ## 6 5100002794         2.19     1                1323         2    1     89437
    ##   store basket day coupon
    ## 1   244      1   1      0
    ## 2   244      1   1      0
    ## 3   244      2   1      0
    ## 4   244      3   1      0
    ## 5   244      4   1      0
    ## 6   244      4   1      0

``` r
products <- read.csv("C:/Users/ajrr1/OneDrive/Documents/R/product_lookup.csv")
head(products)
```

    ##         upc       product_description   commodity         brand product_size
    ## 1 111112360 VINCENT S ORIG MARINARA S pasta sauce     Vincent's        25 OZ
    ## 2 566300023       PINE MOUNTAIN SYRUP      syrups Pine Mountain        40 OZ
    ## 3 566300028         MILLER CANE SYRUP      syrups        Miller        19 OZ
    ## 4 566300029         MILLER CANE SYRUP      syrups        Miller        12 OZ
    ## 5 566300035       PINE MOUNTAIN SYRUP      syrups Pine Mountain        19 OZ
    ## 6 601011292 BARILLA MARINARA PSTA SCE pasta sauce       Barilla     26    OZ

\#——————————— \#’ PART II: SAMPLE THE DATA \#———————————

``` r
set.seed(8451)
sample <- sample(unique(transactions$household), 10000)
transactions_sample <- transactions %>%
  filter(household %in% sample)
head(transactions_sample)
```

    ##          upc dollar_sales units time_of_transaction geography week household
    ## 1 3620000446         2.19     1                1523         2    1    118337
    ## 2 3000005800         3.65     1                1328         2    1     74553
    ## 3 3620000471         3.59     1                1328         2    1     74553
    ## 4 3340060709         0.49     1                1419         2    1     26404
    ## 5 9999971306         1.50     1                2148         2    1    122445
    ## 6 7680828009         0.80     1                2148         2    1    122445
    ##   store basket day coupon
    ## 1   244      7   1      0
    ## 2   352     46   1      0
    ## 3   352     46   1      0
    ## 4   311     61   1      0
    ## 5   311     74   1      0
    ## 6   311     74   1      0

\#——————————– \#’ PART III: EXPLORE THE DATA \#——————————–

``` r
txn <- merge(transactions_sample, products, by = "upc")
dt_brand <-split(txn$brand, txn$basket)
dt2_brand <- as(dt_brand, "transactions")
```

    ## Warning in asMethod(object): removing duplicated items in transactions

``` r
summary(dt2_brand)
```

    ## transactions as itemMatrix in sparse format with
    ##  64365 rows (elements/itemsets/transactions) and
    ##  90 columns (items) and a density of 0.01513452 
    ## 
    ## most frequent items:
    ## Private Label          Ragu       Barilla         Prego   Aunt Jemima 
    ##         23493         11993          6216          5828          4180 
    ##       (Other) 
    ##         35962 
    ## 
    ## element (itemset/transaction) length distribution:
    ## sizes
    ##     1     2     3     4     5     6     7     8 
    ## 44743 16524  2592   439    57     7     2     1 
    ## 
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   1.000   1.000   1.362   2.000   8.000 
    ## 
    ## includes extended item information - examples:
    ##     labels
    ## 1 Al Dente
    ## 2    Alaga
    ## 3   Alessi
    ## 
    ## includes extended transaction information - examples:
    ##   transactionID
    ## 1             7
    ## 2            46
    ## 3            61

\#————————————– \#’ PART III: CREATE ASSOCIATION RULES \#————————————–

Create a set of association rules with minimal length 2 and predictive
power of at least 25% confidence

``` r
rules <- apriori(dt2_brand, parameter = list(support = 0.00005, confidence = 0.25, minlen = 2))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##        0.25    0.1    1 none FALSE            TRUE       5   5e-05      2
    ##  maxlen target  ext
    ##      10  rules TRUE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 3 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[90 item(s), 64365 transaction(s)] done [0.01s].
    ## sorting and recoding items ... [72 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.01s].
    ## checking subsets of size 1 2 3 4 done [0.00s].
    ## writing ... [116 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

\#———————————- \#’ PART IV: INTERPRET THE RULES \#———————————-

Top 10 rules in terms of support

``` r
rules %>%
  sort(by = "support") %>%
  head(n=10) %>%
  inspect()
```

    ##      lhs                      rhs             support      confidence
    ## [1]  {Ragu}                => {Private Label} 0.0540200419 0.2899191 
    ## [2]  {Prego}               => {Private Label} 0.0249980580 0.2760810 
    ## [3]  {Hunt's}              => {Private Label} 0.0164530412 0.3548928 
    ## [4]  {Aunt Jemima,Ragu}    => {Private Label} 0.0014914938 0.3076923 
    ## [5]  {Prego,Ragu}          => {Private Label} 0.0009943292 0.4025157 
    ## [6]  {Hunt's,Ragu}         => {Private Label} 0.0009632564 0.5344828 
    ## [7]  {Aunt Jemima,Prego}   => {Private Label} 0.0009321836 0.3468208 
    ## [8]  {Classico,Ragu}       => {Private Label} 0.0007302105 0.3507463 
    ## [9]  {Hungry Jack,Ragu}    => {Private Label} 0.0006991377 0.3688525 
    ## [10] {Aunt Jemima,Mueller} => {Ragu}          0.0005903830 0.2878788 
    ##      coverage    lift      count
    ## [1]  0.186327973 0.7943066 3477 
    ## [2]  0.090546104 0.7563935 1609 
    ## [3]  0.046360600 0.9723182 1059 
    ## [4]  0.004847355 0.8430007   96 
    ## [5]  0.002470287 1.1027934   64 
    ## [6]  0.001802222 1.4643503   62 
    ## [7]  0.002687796 0.9502031   60 
    ## [8]  0.002081877 0.9609579   47 
    ## [9]  0.001895440 1.0105644   45 
    ## [10] 0.002050804 1.5450111   38

Top 10 rules in terms of lift

Lift compares the proportion of target item purchases within all
predictor item baskets against the total proportion of target item
purchases among all baskets. In other words, lift indicates the
increased likelihood that a customer buys the target item, given that he
or she bought the predictor items, over the likelihood that a random
customer buys the target item.

``` r
rules %>%
  sort(by = "lift") %>%
  head(n=10) %>%
  inspect()
```

    ##      lhs                  rhs                support confidence     coverage      lift count
    ## [1]  {Hungry Jack,                                                                          
    ##       Ragu,                                                                                 
    ##       Ronzoni}         => {Aunt Jemima} 7.768197e-05  1.0000000 7.768197e-05 15.398325     5
    ## [2]  {Mrs Butterworth,                                                                      
    ##       Private Label,                                                                        
    ##       Ragu}            => {Hungry Jack} 1.087548e-04  0.2500000 4.350190e-04  9.538382     7
    ## [3]  {Mrs Butterworth,                                                                      
    ##       Ragu,                                                                                 
    ##       Ronzoni}         => {Aunt Jemima} 6.214558e-05  0.5714286 1.087548e-04  8.799043     4
    ## [4]  {No Yolks,                                                                             
    ##       Ronzoni}         => {Classico}    6.214558e-05  0.3333333 1.864367e-04  8.527424     4
    ## [5]  {Classico,                                                                             
    ##       Northwoods}      => {Ronzoni}     6.214558e-05  0.3333333 1.864367e-04  7.717626     4
    ## [6]  {Creamette,                                                                            
    ##       Mrs Butterworth,                                                                      
    ##       Private Label}   => {Aunt Jemima} 6.214558e-05  0.5000000 1.242912e-04  7.699163     4
    ## [7]  {Aunt Jemima,                                                                          
    ##       Hungry Jack,                                                                          
    ##       Ragu}            => {Ronzoni}     7.768197e-05  0.2777778 2.796551e-04  6.431355     5
    ## [8]  {Aunt Jemima,                                                                          
    ##       Mrs Butterworth,                                                                      
    ##       Ragu}            => {Ronzoni}     6.214558e-05  0.2666667 2.330459e-04  6.174101     4
    ## [9]  {Mrs Butterworth,                                                                      
    ##       Mueller,                                                                              
    ##       Ragu}            => {Aunt Jemima} 6.214558e-05  0.4000000 1.553639e-04  6.159330     4
    ## [10] {Aunt Jemima,                                                                          
    ##       Hungry Jack,                                                                          
    ##       Ronzoni}         => {Ragu}        7.768197e-05  1.0000000 7.768197e-05  5.366881     5

Conclusion: According to lift, there is a strong association between
Ronzoni and pasta sauce products. This association is intuitive, since
pasta and pasta sauce are often purchased together, but Ronzoni in
particular is featured in seven of the top ten rules. Based on my
analysis, I recommend a cross-product promotional strategy to increase
the amount of product purchased, because consumers with Ronzoni pasta in
their basket more likely to purchase pasta sauce will be willing to
purchase more product, given a deal or coupon. Therefore, Kroger should
implement a deal such as Buy 3 Ronzoni products, get 1 Classico product
free or Ragu products 5/$5 when purchased with Ronzoni product(s).
