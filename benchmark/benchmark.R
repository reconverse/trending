
library(tidyverse)
library(trending)

## data simulation

x <- 1:1e4
y <- list(
    poisson_constant = rpois(1e4, lambda = 10),
    poisson_expo = rpois(1e4, lambda = exp(.1 * sqrt(x))),
    negbin_constant = rnbinom(1e4, mu = 10, size = 10),
    negbin_expo = rnbinom(1e4, mu = exp(.1 * sqrt(x)), size = 10)
)

df <- data.frame(x = x, y)

data <- lapply(y, function(y) cbind.data.frame(x, y))
names(data) <- names(models)

## define models

models <- list(
    poisson_constant = glm_model(y ~ 1, family = "poisson"),
    poisson_expo = glm_model(y ~ x, family = "poisson"),
    negbin_constant = glm_nb_model(y ~ 1),
    negbin_expo = glm_nb_model(y ~ x)
)



## fitting models to data

fits_basic <- fits_boot <- list()
for (i in seq_along(y)) {
  fits_basic[[i]] <- fit(models[[i]], data[[i]], simulate_pi = FALSE, alpha = 0.05)
  fits_boot[[i]] <- fit(models[[i]], data[[i]], simulate_pi = TRUE, alpha = 0.05)
}


## getting predictions and inspecting results

preds_basic <- lapply(fits_basic, predict)
preds_boot <- lapply(fits_boot, predict)
names(preds_basic) <- names(preds_boot) <- names(models)


preds_basic <- bind_rows(preds_basic, .id = "model") %>% tibble()
preds_boot <- bind_rows(preds_boot, .id = "model") %>% tibble()

preds <- list(
    basic = preds_basic,
    boot = preds_boot
) %>% bind_rows(.id = "pi_type")


## compare results
preds %>%
  mutate(outlier = y > lower_pi & y < upper_pi) %>%
  group_by(model, pi_type) %>%
  summarise(p_out = mean(outlier))

