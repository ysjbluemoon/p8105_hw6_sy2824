hw6
================
Sijia Yue

``` r
# Read file into the project
file = read_csv("file/homicide-data.csv") %>% 
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

``` r
homicide_data = 
  file %>% 
  # Add new variable city_state
  mutate(city_state = str_c(city, ", " , state)) %>% 
  mutate(resolved = as.numeric(disposition == "Closed by arrest")) %>% 
  filter(!city_state %in% c("Dallas, TX", "Phoenix, AZ" , "Kansas City, MO" , "Tulsa, AL")) %>% 
  mutate(victim_race = ifelse(victim_race == "White", "white", "non-white"), victim_race = fct_relevel(victim_race, "white")) %>% 
  mutate(victim_age = as.numeric(victim_age))
```

    ## Warning in evalq(as.numeric(victim_age), <environment>): NAs introduced by
    ## coercion

Fit in logistics regression model

``` r
# Filter Baltimore from the list
baltimore_df = 
  homicide_data %>% 
  filter(city == "Baltimore")

# Fit into the logistic regression model with age, sex and race
fit_logistic = 
  baltimore_df %>% 
  glm(resolved ~ victim_age + victim_sex + victim_race, data = ., family = binomial())

# Estimate, CI of adjusted odds ratio
broom::tidy(fit_logistic, conf.int = TRUE, exponentiate = TRUE) %>% 
  filter(term == "victim_racenon-white") %>% 
  mutate(OR = estimate) %>%
  select(term, OR, conf.low, conf.high) %>% 
  knitr::kable(digits = 3)
```

| term                  |     OR|  conf.low|  conf.high|
|:----------------------|------:|---------:|----------:|
| victim\_racenon-white |  0.441|     0.312|       0.62|

Mapping into all cities

``` r
glm_or = function(cities){
  fit_logistic =   
  glm(resolved ~ victim_age + victim_sex + victim_race, data = cities, family = binomial())
  
  broom::tidy(fit_logistic, conf.int = TRUE, exponentiate = TRUE) %>% 
  filter(term == "victim_racenon-white") %>% 
  mutate(OR = estimate) %>%
  select(term, OR, conf.low, conf.high) 
}

glm_result = 
  homicide_data %>% 
  group_by(city_state) %>% 
  select(city_state, victim_race:victim_sex, resolved) %>% 
  nest() %>% 
  # Using the map function to do the iteration
  mutate(result = map(data, glm_or)) %>% 
  select(-data) %>% 
  unnest() %>% 
  select(-term)
```

Plot the OR and CI

``` r
glm_result %>%  
  mutate(city_state = as.factor(city_state)) %>% 
  mutate(city_state = fct_reorder(city_state, OR)) %>% 
  ggplot(aes(x = city_state, y = OR, ymin = conf.low, ymax = conf.high, color = city_state)) +
  geom_point() +
  geom_errorbar() +
  labs(
    title = "Plot of Odd Ratio and CI for each city",
    x = "Each city and its state",
    y = "Odd Ratio and its CI"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1)) 
```

![](hw6_files/figure-markdown_github/unnamed-chunk-5-1.png)
