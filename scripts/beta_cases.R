# beta distribution 4 cases
set.seed(2025)

params <- list(
  "alpha >1, beta >1" = c(alpha=5, beta=5),
  "alpha <1, beta <1" = c(alpha=0.5, beta=0.5),
  "alpha >1, beta <1" = c(alpha=2, beta=0.5),
  "alpha <1, beta >1" = c(alpha=0.1, beta=5)
)


# generate and summarize
results <- lapply(names(params), function(name) {
  a <- params[[name]]["alpha"]
  b <- params[[name]]["beta"]
  samp <- rbeta(50, shape1 = a, shape2 = b)
  tibble(
    case = name,
    alpha = a,
    beta = b,
    theoretical_mean = a / (a + b),
    sample_mean = mean(samp),
    theoretical_variance = (a * b) / ((a + b)^2 * (a + b + 1)),
    sample_variance = var(samp)
  )
})


results <- function(alpha, beta, n) {
  samp <- rbeta(n, shape1 = alpha, shape2 = beta)
  tibble(
    alpha = a,
    beta = b,
    theoretical_mean = a / (a + b),
    sample_mean = mean(samp),
    theoretical_variance = (a * b) / ((a + b)^2 * (a + b + 1)),
    sample_variance = var(samp)
  )
  cat(paste("Alpha:", a, "Beta:", b))
  }


results_df <- bind_rows(results)

knitr::kable(results_df, digits = 4)


# example with just one IGO 
subset <- igo_master %>%
  filter(ioname == "ASEAN", year == 2010) %>%
  pull(v2x_polyarchy)

subset <- subset[!is.na(subset)]



library(fitdistrplus)
fit <- fitdist(subset, "beta", method = "mle")
summary(fit)


alpha <- fit$estimate["shape1"]
beta <- fit$estimate["shape2"]

#sample_mean 
mean(subset)
#beta mean
alpha / (alpha + beta)

# sample_variance
var(subset)

# beta_variance 
(alpha * beta) / (((alpha + beta)^2) * (alpha + beta + 1))


