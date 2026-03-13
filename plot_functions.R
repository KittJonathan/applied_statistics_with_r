# Packages ----

library(latex2exp)
library(tidyverse)
theme_set(theme_bw())

# Plot Normal PDF ----

plot_norm_pdf <- function(mu = 0, sigma = 1, 
                          sigma_range = 3, qval = NULL) {
  
  p <- ggplot(data = data.frame(
    x = c(mu - sigma_range*sigma, mu + sigma_range*sigma)),
    aes(x)
  ) +
    stat_function(
      fun = dnorm,
      args = list(mean = mu, sd = sigma),
      linewidth = 0.8
    ) +
    scale_x_continuous(
      limits = c(mu - sigma_range*sigma, mu + sigma_range*sigma),
      breaks = seq(
        from = ceiling(mu - sigma_range*sigma),
        to = ceiling(mu + sigma_range*sigma),
        by = 1
      )
    ) +
    labs(title = paste0("P.D.F. ~ N(mu = ", mu, ", sigma = ", sigma, ")"),
         y = "f(x)")
  
  if (!is.null(qval)) {
    
    p <- p +
      geom_segment(x = qval, xend = qval,
                   y = 0, yend = dnorm(x = qval, mean = mu, sd = sigma),
                   color = "#00998a", linetype = "dotted") +
      geom_segment(x = mu - sigma_range*sigma, 
                   xend = qval,
                   y = dnorm(x = qval, mean = mu, sd = sigma), 
                   yend = dnorm(x = qval, mean = mu, sd = sigma),
                   color = "#00998a", linetype = "dotted") +
      geom_point(x = qval, 
                 y = dnorm(x = qval, mean = mu, sd = sigma),
                 size = 3, color = "#00998a", fill = "white", shape = 21,
                 stroke = 2) +
      annotate(x = mu + sigma_range*sigma,
               y = max(dnorm(x = seq(mu - sigma_range*sigma, mu + sigma_range*sigma, 0.01), mean = mu, sd = sigma)), 
               geom = "text",
               label = paste0("x = ", qval),
               hjust = 1,
               vjust = 1) +
      annotate(x = mu + sigma_range*sigma,
               y = max(dnorm(x = seq(mu - sigma_range*sigma, mu + sigma_range*sigma, 0.01), mean = mu, sd = sigma)), 
               geom = "text",
               label = paste0("f(x) = ", round(dnorm(x = qval, mean = mu, sd = sigma), 3)),
               hjust = 1,
               vjust = 3)
    }
  
  p
  
  }

plot_norm_pdf()
plot_norm_pdf(qval = 0)

# Plot Normal CDF ----

plot_norm_cdf <- function(mu = 0, sigma = 1, sigma_range = 3,
                          from = NULL, to = NULL) {
  
  if(is.null(from)) {
    x1 <- mu - sigma_range*sigma
  } else {
    x1 <- from
  }
  
  if(is.null(to)) {
    x2 <- mu + sigma_range*sigma
  } else {
    x2 <- to
  }
  
  p <- ggplot(data = data.frame(
    x = c(mu - sigma_range*sigma, mu + sigma_range*sigma)),
    aes(x)
    ) +
    stat_function(
      fun = dnorm,
      args = list(mean = mu, sd = sigma),
      geom = "area",
      xlim = c(x1, x2),
      fill = "#00998a",
      alpha = 0.5
      ) +
    stat_function(
      fun = dnorm,
      args = list(mean = mu, sd = sigma),
      linewidth = 0.8
      ) +
    scale_x_continuous(
      limits = c(mu - sigma_range*sigma, mu + sigma_range*sigma),
      breaks = seq(
        from = ceiling(mu - sigma_range*sigma),
        to = ceiling(mu + sigma_range*sigma),
        by = 1
        )
      ) +
    labs(title = paste0("C.D.F. ~ N(mu = ", mu, ", sigma = ", sigma, ")"),
         y = "f(x)")
  
  if (is.null(from) & is.null(to)) {
    p <- p +
      annotate(x = mu + sigma_range*sigma,
               y = max(dnorm(x = seq(mu - sigma_range*sigma, mu + sigma_range*sigma, 0.01), mean = mu, sd = sigma)), 
               geom = "text",
               label = paste0("P(x <= ", mu + sigma_range * sigma, ") = ",
                            pnorm(q = Inf, mean = mu, sd = sigma)),
               hjust = 1)
    }
  
  if (is.null(from) & !is.null(to)) {
    p <- p +
      annotate(x = mu + sigma_range*sigma,
               y = max(dnorm(x = seq(mu - sigma_range*sigma, mu + sigma_range*sigma, 0.01), mean = mu, sd = sigma)), 
               geom = "text",
               label = paste0("P(x <= ", to, ") = ",
                              round(pnorm(q = to, mean = mu, sd = sigma), 3)),
               hjust = 1)
    }
  
  if (!is.null(from) & is.null(to)) {
    p <- p +
      annotate(x = mu + sigma_range*sigma,
               y = max(dnorm(x = seq(mu - sigma_range*sigma, mu + sigma_range*sigma, 0.01), mean = mu, sd = sigma)), 
               geom = "text",
               label = paste0("P(", from, " <= x) = ",
                              round(1 - pnorm(q = from, mean = mu, sd = sigma), 3)),
               hjust = 1)
    
    }
  
  if (!is.null(from) & !is.null(to)) {
    p <- p +
      annotate(x = mu + sigma_range*sigma,
               y = max(dnorm(x = seq(mu - sigma_range*sigma, mu + sigma_range*sigma, 0.01), mean = mu, sd = sigma)), 
               geom = "text",
               label = paste0("P(", from, " <= x <= ", to, ") = ",
                              round(pnorm(q = to, mean = mu, sd = sigma) - pnorm(q = from, mean = mu, sd = sigma), 3)),
               hjust = 1)
    }
  
  p
  
  }

plot_norm_cdf()
plot_norm_cdf(to = 1)
plot_norm_cdf(from = -2, to = 2)

# Plot Binomial PDF ----

size = 100
prob <- 0.5
qval <- 50

df <- tibble(
  x = 0:size,
  prob = dbinom(x = 0:size, size = size, prob = 0.5)
) |> 
  mutate(qval = ifelse(x == qval, 1, 0)
         )

ggplot(data = df) +
  geom_rect(aes(xmin = x - 0.2, xmax = x + 0.2,
                ymin = 0, ymax = prob,
                fill = factor(qval)),
            show.legend = FALSE
            )

dbinom(x = 0:size, size = 10, prob = 0.5)
pbinom(q = 2, size = 10, prob = 0.5)

sum(dbinom(x = 0:size, size = 10, prob = 0.5)[1:3])

ggplot(data = data.frame(
  x = 0:size),
  aes(x)
) +
  stat_function(
    fun = dbinom,
    args = list(size = size, prob = prob),
    linewidth = 0.8
  )
