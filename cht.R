# Szükséges csomagok
library(ggplot2)
library(dplyr)

# Általános szimulációs függvény a CHT szemléltetésére
simulate_cht <- function(distribution, params, n_samples, sample_size) {
  set.seed(123)  # Fix random
  
  print(paste("Futtatás:", distribution))  # Debugging
  
  # Minták generálása
  samples <- replicate(n_samples, {
    if (distribution == "exponential") {
      rexp(sample_size, rate = params$lambda)
    } else if (distribution == "binomial") {
      rbinom(sample_size, size = params$size, prob = params$prob)
    } else if (distribution == "uniform") {
      runif(sample_size, min = params$min, max = params$max)
    } else if (distribution == "normal") {
      rnorm(sample_size, mean = params$mean, sd = params$sd)
    } else if (distribution == "poisson") {
      rpois(sample_size, lambda = params$lambda)
    } else if (distribution == "gamma") {
      rgamma(sample_size, shape = params$shape, scale = params$scale)
    } else if (distribution == "beta") {
      rbeta(sample_size, shape1 = params$alpha, shape2 = params$beta)
    } else if (distribution == "chi-squared") {
      rchisq(sample_size, df = params$df)
    } else if (distribution == "t") {
      rt(sample_size, df = params$df)
    } else if (distribution == "f") {
      rf(sample_size, df1 = params$df1, df2 = params$df2)
    } else if (distribution == "cauchy") {
      rcauchy(sample_size, location = params$location, scale = params$scale)
    } else if (distribution == "weibull") {
      rweibull(sample_size, shape = params$shape, scale = params$scale)
    } else if (distribution == "log-normal") {
      rlnorm(sample_size, meanlog = params$meanlog, sdlog = params$sdlog)
    } else {
      stop("Ismeretlen eloszlás!")
    }
  })
  
  
  # Minták átalakítása data frame-be és dplyr-rel való feldolgozás
  sample_means <- as.data.frame(samples) %>%
    rowMeans() %>%
    data.frame(means = .)
  
  # Mintaátlagok várható értéke és szórása
  mean_val <- mean(sample_means$means)
  sd_val <- sd(sample_means$means)
  
  # Normális eloszlás sűrűségfüggvénye az ábrához
  x_vals <- seq(min(sample_means$means), max(sample_means$means), length.out = 100)
  normal_curve <- data.frame(x = x_vals, y = dnorm(x_vals, mean = mean_val, sd = sd_val), type = "Normális eloszlás")
  
  # Hisztogram készítése ggplot2-vel és jelmagyarázat hozzáadása
  plot <- ggplot() +
    geom_histogram(data = sample_means,
                   aes(x = means, y = ..density.., fill = "Mintaátlagok"),
                   bins = 30, alpha = 0.7) +
    geom_density(data = sample_means,
                 aes(x = means, color = "Empirikus sűrűség"),
                 linetype = "solid", linewidth = 1) +
    geom_line(data = normal_curve,
              aes(x = x, y = y, color = "Normális eloszlás"),
              linetype = "dashed", linewidth = 1) +
    labs(title = paste("Centrális Határeloszlás-tétel -", distribution),
         x = "Mintaátlagok",
         y = "Gyakoriság",
         fill = "Adattípus",
         color = "Görbék") +
    scale_fill_manual(values = c("Mintaátlagok" = "darkgoldenrod1")) +
    scale_color_manual(values = c("Empirikus sűrűség" = "red", "Normális eloszlás" = "black")) +
    theme_minimal()
  
  # Ábra kiírása
  print(plot)
}

# Definiált szimulációk listája
simulations <- list(
  list(distribution = "exponential",
       params = list(lambda = 1),
       n_samples = 5000,
       sample_size = 100),
  list(distribution = "binomial",
       params = list(size = 10, prob = 0.3),
       n_samples = 1000,
       sample_size = 100),
  list(distribution = "uniform",
       params = list(min = 0, max = 1),
       n_samples = 5000,
       sample_size = 100)#,
#  list(distribution = "poisson",
#       params = list(lambda = 1),
#       n_samples = 5000,
#       sample_size = 100)
)

# Az összes szimuláció végrehajtása
for (sim in simulations) {
  simulate_cht(sim$distribution, sim$params, sim$n_samples, sim$sample_size)
}
