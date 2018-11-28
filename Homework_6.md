Data Science Homework 6
================
Boya Guo
11/27/2018

### Problem 1

``` r
homicide = read_csv(file = "./data/homicide-data.csv") %>% 
  janitor::clean_names() 
```

    ## Parsed with column specification:
    ## cols(
    ##   uid = col_character(),
    ##   reported_date = col_integer(),
    ##   victim_last = col_character(),
    ##   victim_first = col_character(),
    ##   victim_race = col_character(),
    ##   victim_age = col_character(),
    ##   victim_sex = col_character(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   lat = col_double(),
    ##   lon = col_double(),
    ##   disposition = col_character()
    ## )

First, we tidy the data, modifiy and change the variable names

``` r
homicide_1 = homicide %>% 
   mutate(victim_race = fct_relevel(ifelse(victim_race == "White", "white", "non-white"), "white"),
         victim_age = ifelse(victim_age == "Unknown", NA, as.integer(victim_age)),
         victim_sex = as.factor(victim_sex),
         city_state = paste(paste0(city, ","), state),
         resolved = as.numeric(disposition == "Closed by arrest")) %>% 
  filter(!city_state %in% c("Dallas, TX", "Phoenix, AZ", "Kansas City, MO", "Tulsa, AL")) %>% 
  select(uid, victim_race, victim_age, victim_sex, city_state, resolved)
```

    ## Warning in ifelse(victim_age == "Unknown", NA, as.integer(victim_age)):
    ## Ç¿ÖÆ¸Ä±ä¹ý³ÌÖÐ²úÉúÁËNA

Next we create a logistic regression model for Baltimore and save the output of glm as an R object

``` r
baltimore_logistic = homicide_1 %>%
  filter(city_state == "Baltimore, MD") %>% 
  glm(resolved ~ victim_age + victim_sex + victim_race, data = ., family = binomial()) 

save(baltimore_logistic,file = "baltimore_logistic.RData") 

baltimore_logistic
```

    ## 
    ## Call:  glm(formula = resolved ~ victim_age + victim_sex + victim_race, 
    ##     family = binomial(), data = .)
    ## 
    ## Coefficients:
    ##          (Intercept)            victim_age        victim_sexMale  
    ##              1.18603              -0.00699              -0.88779  
    ## victim_racenon-white  
    ##             -0.81960  
    ## 
    ## Degrees of Freedom: 2826 Total (i.e. Null);  2823 Residual
    ## Null Deviance:       3676 
    ## Residual Deviance: 3597  AIC: 3605

Then, we applied broom::tidy and obtain the estimate and CI for the adjusted OR

``` r
baltimore_logistic %>% 
  broom::tidy() %>% 
    mutate(OR = exp(estimate),
           conf_low = exp(estimate - 1.96*std.error),
           conf_high = exp(estimate + 1.96*std.error)) %>% 
    filter(term == "victim_racenon-white") %>% 
    select(term, OR, conf_low, conf_high) %>% 
    knitr::kable(digits = 4) 
```

| term                    |         OR|    conf\_low|                                                                                         conf\_high|
|:------------------------|----------:|------------:|--------------------------------------------------------------------------------------------------:|
| victim\_racenon-white   |     0.4406|       0.3129|                                                                                             0.6204|
| The OR estimate for sol |  ving homi|  cides compa|  ring non-white victims to white victims is 0.4406 and the true OR lies between 0.3129 and 0.6204.|

Next step, we run glm for each of the cities and extract the adjusted OR with CI

To do this, we create a logistic function and then perform the logistic regression on all cities

``` r
city_logistic = function(x){
    homicide_1 %>% 
    filter(city_state == x) %>% 
    glm(resolved ~ victim_age + victim_sex + victim_race, data = ., family = binomial())  %>% 
    broom::tidy() %>% 
    mutate(OR = exp(estimate),
           conf_low = exp(estimate - 1.96 * std.error),
           conf_high = exp(estimate + 1.96 * std.error)) %>% 
    filter(term == "victim_racenon-white") %>% 
    select(term, OR, conf_low, conf_high)
}

cities = 
  tibble(city_state = unique(homicide_1$city_state)) %>% 
  mutate(map(.x = unique(homicide_1$city_state), ~city_logistic(.x))) %>% 
  unnest

cities
```

    ## # A tibble: 47 x 5
    ##    city_state      term                    OR conf_low conf_high
    ##    <chr>           <chr>                <dbl>    <dbl>     <dbl>
    ##  1 Albuquerque, NM victim_racenon-white 0.741   0.451      1.22 
    ##  2 Atlanta, GA     victim_racenon-white 0.753   0.432      1.31 
    ##  3 Baltimore, MD   victim_racenon-white 0.441   0.313      0.620
    ##  4 Baton Rouge, LA victim_racenon-white 0.668   0.313      1.43 
    ##  5 Birmingham, AL  victim_racenon-white 1.04    0.615      1.76 
    ##  6 Boston, MA      victim_racenon-white 0.115   0.0472     0.278
    ##  7 Buffalo, NY     victim_racenon-white 0.390   0.213      0.715
    ##  8 Charlotte, NC   victim_racenon-white 0.558   0.321      0.969
    ##  9 Chicago, IL     victim_racenon-white 0.562   0.431      0.733
    ## 10 Cincinnati, OH  victim_racenon-white 0.318   0.184      0.551
    ## # ... with 37 more rows

Create a plot that shows the estimated ORs and CIs for each city
