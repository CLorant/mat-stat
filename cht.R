library(ggplot2)
library(dplyr)

read_choice <- function() {
    message("1: Demo futtatása");
    message("2: Szimuláció manuális beállítása");
    
    choice <- as.numeric(readline())
    return(choice)
}

run_demo <- function() {
	# Előre definiált szimulációk listája
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
			sample_size = 100)
	)

	# Az összes szimuláció végrehajtása
	for (sim in simulations) {
		simulate_cht(sim$distribution, sim$params, sim$n_samples, sim$sample_size)
	}
}

# Eloszlások és a hozzájuk tartozó paraméterek
distribution_params <- list(
    exponential = list(params = c("lambda")),
    binomial = list(params = c("size", "prob")),
    uniform = list(params = c("min", "max")),
    normal = list(params = c("mean", "sd")),
    poisson = list(params = c("lambda")),
    gamma = list(params = c("shape", "scale")),
    beta = list(params = c("alpha", "beta")),
    chi_squared = list(params = c("df")),
    t = list(params = c("df")),
    f = list(params = c("df1", "df2")),
    cauchy = list(params = c("location", "scale")),
    weibull = list(params = c("shape", "scale")),
    log_normal = list(params = c("meanlog", "sdlog"))
)

# Paraméterek bekérése dinamikusan
set_simulation <- function() {
    cat("Válassz egy eloszlást (exponential, binomial, uniform, normal, poisson, gamma, beta, chi-squared, t, f, cauchy, weibull, log-normal): ")
    distribution <- readline()
    
    # Ellenőrizzük, hogy létezik-e az eloszlás a listában
    if (!(distribution %in% names(distribution_params))) {
        stop("Ismeretlen eloszlás!")
    }
    
    # Paraméterek bekérése a kiválasztott eloszláshoz
    params <- list()
    for (param_name in distribution_params[[distribution]]$params) {
        repeat {
            cat(paste("Add meg a", param_name, "értékét: "))
            param_value <- as.numeric(readline())
            if (!is.na(param_value) && param_value > 0) {
                params[[param_name]] <- param_value
                break
            } else {
                cat("Érvénytelen érték, próbáld újra!\n")
            }
        }
    }
    
    # Minta méretének bekérése
    repeat {
        cat("Add meg az n_samples értékét: ")
        n_samples <- as.numeric(readline())
        if (!is.na(n_samples) && n_samples > 0) break
        else cat("Érvénytelen érték, próbáld újra!\n")
    }
    
    repeat {
        cat("Add meg a sample_size értékét: ")
        sample_size <- as.numeric(readline())
        if (!is.na(sample_size) && sample_size > 0) break
        else cat("Érvénytelen érték, próbáld újra!\n")
    }
    
    # Szimuláció futtatása
    simulate_cht(distribution, params, n_samples, sample_size)
}

simulation_functions <- list(
    exponential = function(params, sample_size) rexp(sample_size, rate = params$lambda),
    binomial = function(params, sample_size) rbinom(sample_size, size = params$size, prob = params$prob),
    uniform = function(params, sample_size) runif(sample_size, min = params$min, max = params$max),
    normal = function(params, sample_size) rnorm(sample_size, mean = params$mean, sd = params$sd),
    poisson = function(params, sample_size) rpois(sample_size, lambda = params$lambda),
    gamma = function(params, sample_size) rgamma(sample_size, shape = params$shape, scale = params$scale),
    beta = function(params, sample_size) rbeta(sample_size, shape1 = params$alpha, shape2 = params$beta),
    chi_squared = function(params, sample_size) rchisq(sample_size, df = params$df),
    t = function(params, sample_size) rt(sample_size, df = params$df),
    f = function(params, sample_size) rf(sample_size, df1 = params$df1, df2 = params$df2),
    cauchy = function(params, sample_size) rcauchy(sample_size, location = params$location, scale = params$scale),
    weibull = function(params, sample_size) rweibull(sample_size, shape = params$shape, scale = params$scale),
    log_normal = function(params, sample_size) rlnorm(sample_size, meanlog = params$meanlog, sdlog = params$sdlog)
)

# Általános szimulációs függvény a CHT szemléltetésére
simulate_cht <- function(distribution, params, n_samples, sample_size) {
	set.seed(123)  # Fix random
	print(paste("Futtatás:", distribution))  # Debugging
	
	# A megfelelő szimulációs függvény kiválasztása
	simulation_func <- simulation_functions[[distribution]]
	
	# Minták generálása
	samples <- replicate(n_samples, simulation_func(params, sample_size))
	
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
					aes(x = means, y = after_stat(density), fill = "Mintaátlagok"),
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

main <- function() {
    choice <- read_choice()
    
    if (choice == 1) {
        run_demo()
    }
    else if (choice == 2) {
        set_simulation()
    }
    else {
        cat("Válassz az 1 vagy 2 opció közül!")
        read_choice()
    }
    
    message("Újra futtatod? igen / nem")
    choice <- readline()
    
    if (choice == "igen") {
        main()
    }
}

main()