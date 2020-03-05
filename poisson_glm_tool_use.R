

library(brms)
library(tidybayes)
library(tidyverse)
library(modelr)
library(ggplot2)


data("Kline", package = "rethinking")


## create log population
Kline$logpop <- log(Kline$population)
Kline$logpop_centered <- Kline$logpop - mean(Kline$logpop)
Kline$logpop_centered


## Visualization II -- prior predictions
tools_pop_bf <- bf(total_tools ~ 1 + logpop_centered + (1|culture), family = "poisson")

# look at prior values
get_prior(tools_pop_bf, data = Kline)

# set prior values
priors <- c(set_prior("normal(0, 1)", class = "b", coef = "logpop_centered"),
            set_prior("normal(0, 10)", class = "Intercept"),
            set_prior("exponential(3)", class = "sd"))

# sample the model -- but only the prior
tools_pop_prior <- brm(tools_pop_bf, data = Kline,
                       prior = priors, chains = 1, cores =1,
                       sample_prior = "only", file = "tools_naive_prior")

tools_pop_prior

## check range of data
Kline$logpop_centered %>% range
# generate "fake" data to visualize our data
Kline_post_samp <- tibble(logpop_centered = seq(-2, 4, by = 0.5)) %>% 
  add_predicted_draws(model = tools_pop_prior,
                      re_formula = NULL,
                      allow_new_levels = TRUE, n = 9)

head(Kline_post_samp)

Kline_post_samp %>% 
  ggplot(aes(x = logpop_centered, y = .prediction)) + 
  geom_point() + 
  facet_wrap(~.draw, scales = "free_y")

## set a more possible prior -- about how many tools on average? 
#### take a guess -- and use log() to calculate the value

prior_knowledge <- c(set_prior("normal(1, 1)", class = "b", coef = "logpop_centered"),
                     set_prior("normal(4.5, 1)", class = "Intercept"),
                     set_prior("exponential(3)", class = "sd"))

# sample the model -- but only the prior
tools_pop_prior_knowledge <- brm(tools_pop_bf, data = Kline,
                       prior = prior_knowledge, chains = 1, cores =1,
                       sample_prior = "only")

# visualize again -- bonus, make this a function
Kline_post_samp <- tibble(logpop_centered = seq(-2, 4, by = 0.5)) %>% 
  add_predicted_draws(model = tools_pop_prior_knowledge,
                      re_formula = NULL,
                      allow_new_levels = TRUE, n = 9)

head(Kline_post_samp)

Kline_post_samp %>% 
  ggplot(aes(x = logpop_centered, y = .prediction)) + 
  geom_point() + 
  facet_wrap(~.draw, scales = "free_y")

## NOTICE what we *don't* do here -- look at the raw data to compare!



# visualize the real data -------------------------------------------------

Kline %>% 
  ggplot(aes(x = logpop, y = total_tools)) + geom_point()


# fit the real data -------------------------------------------------------

tools_pop_posterior_brms <- brm(tools_pop_bf, data = Kline,
                                 prior = prior_knowledge, chains = 3, cores =3,
                                 sample_prior = "yes")


### visualization for model fitting
shinystan::launch_shinystan(tools_pop_posterior_brms)



# visualize predictions ---------------------------------------------------


kline_posterior_predictions <- tibble(logpop_centered = seq(-2, 4, by = 0.2)) %>% 
  add_predicted_draws(tools_pop_posterior_brms, n = 500,
                      re_formula = NULL,
                      allow_new_levels = TRUE) %>% 
  median_hdi(.width = c(0.73, 0.89))


kline_posterior_predictions %>% 
  ggplot(aes(x = logpop_centered, y = .prediction)) + 
  geom_lineribbon() + 
  scale_fill_brewer(palette = "Greens", direction= -1) + 
  geom_point(aes(x = logpop_centered, y = total_tools), data = Kline, pch = 21,
             fill = "purple", size = 2)



# bonus -- compare w/o random effects -------------------------------------

tools_pop_noranef_bf <- bf(total_tools ~ 1 + logpop_centered, family = "poisson")

prior_knowledge_noranef <- c(set_prior("normal(1, 1)", class = "b", coef = "logpop_centered"),
                     set_prior("normal(4.5, 1)", class = "Intercept"))


tools_pop_posterior_noranef_brms <- brm(tools_pop_noranef_bf, data = Kline,
                                prior = prior_knowledge_noranef, chains = 3, cores =3,
                                sample_prior = "yes")


kline_posterior_predictions_noranef <- tibble(logpop_centered = seq(-2, 4, by = 0.2)) %>% 
  add_predicted_draws(tools_pop_posterior_noranef_brms, n = 500) %>% 
  median_hdi(.width = c(0.73, 0.89))

kline_posterior_predictions_noranef %>% 
  ggplot(aes(x = logpop_centered, y = .prediction)) + 
  geom_lineribbon() + 
  scale_fill_brewer(palette = "Greens", direction= -1) + 
  geom_point(aes(x = logpop_centered, y = total_tools), data = Kline, pch = 21,
             fill = "purple", size = 2)


