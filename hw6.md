# MA \[46\]15 Homework 6
Daniel Sussman

## Question 1

The code below loads in the `covid_data_count` tibble and processes it
to change the counts from cumulative to semiweekly counts.

**\[\[** For each of the comments numbered 1-4, answer the question
about that line. **\]\]**

``` r
suppressPackageStartupMessages(library(tidyverse))

cdf <- 
  read_rds("covid_data_count.rds") |> 
  select(-Tests) |> 
  filter(race == "Total") |> 
  group_by(state) |>
    # 1. Why is the above line necessary for 2 to work?
  mutate(across(Cases:Hosp, \(v) v - lag(v, order_by = date))) |>
    # 2. What does the above line do?
  filter(if_all(Cases:Hosp, \(v) v >= 0)) |>
    # 3. What does the above line do?
  ungroup() |> 
  complete(state, date, race) |>
    # 4. What does the above line do?
  group_by(state, race)
```

## Question 2

**\[\[** Create a new q2 where you make a scatter plot of the
semi-weekly counts for deaths versus cases. Remove the NAs beforehand to
avoid any warnings. Use transparency to avoid overplotting. Make another
plot showing deaths versus deaths/cases, again filtering observations
beforehand to avoid warnings Comment on if these look how you would
expect. **\]\]**

## Question 3

We hypothesize that the number of Cases is actually more predictive for
future weeks than for the current week. Here, we fit models for
predicting `w` weeks into the future for `w` = 0, 1, 2, 3, 4, and 5. We
compare the models in terms of the r-squared value to determine how many
weeks into the future the number of cases best predicts the number of
deaths.

**\[\[** In `q3a` below, modify the function `fit_lagged_model` as
follows. The function will first create a new column `lagged_cases` that
has the number of cases observed `w` weeks ago, recalling that there are
2 rows for each week. Fit a model using `lm` with Deaths as the response
variable and `lagged_cases` as the predictor variable with no intercept.
Take the results of that model and pipe them into the last 3 lines of
the function, leaving them as is. (You might need to install the “broom”
package.) **\]\]**

``` r
fit_lagged_model <- function(w, covid_df){
  lm(x ~ y, data = tibble(x = 1:10, y = 2 * x + rnorm(10))) |> 
    broom::glance() |> 
    select(r.squared, sigma, nobs) |> 
    mutate(lag = w)
}
```

**\[\[** After modifying the function, keep the `q3b` as is, run it, and
observe the output. In terms of `r.squared`, how many weeks into the
future do Cases best predict the number of deaths? **\]\]**

``` r
map(seq(5), fit_lagged_model, covid_df = cdf) |> 
  list_rbind()
```

    # A tibble: 5 × 4
      r.squared sigma  nobs   lag
          <dbl> <dbl> <int> <int>
    1     0.971 0.543    10     1
    2     0.990 0.324    10     2
    3     0.979 0.463    10     3
    4     0.965 0.600    10     4
    5     0.997 0.180    10     5

## Question 4

**\[\[** Using your answer for the best number of weeks from Question 3,
fit the same model again and save the fitted model to a new variable in
a new `q4` chunk. Use `broom::tidy` on the fitted model to view a
summary of the coefficient and its statistics. Use `broom::augment` on
the fitted model and make a plot of the `.resid` versus `.fitted`.
Interpret the values of the coefficients and comment on the residual
plot. **\]\]**
