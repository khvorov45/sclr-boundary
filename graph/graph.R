# Graph
# Arseniy Khvorov
# Created 2019/11/05
# Last edit 2019/11/05

library(sclr)
library(tidyverse)
library(ggdark) # devtools::install_github("khvorov45/ggdark")

graph_folder <- "graph"

graph_line <- function(par_combos, var, x_lab, x_breaks) {
  par_combos %>%
    ggplot(aes(!!sym(var), log_lik)) +
    dark_theme_bw(verbose = FALSE) +
    theme(
      panel.grid.minor = element_blank()
    ) +
    scale_x_continuous(x_lab, breaks = x_breaks) +
    scale_y_continuous(
      "Log likelihood",
      breaks = seq(-4000, 0, 500), labels = scales::comma_format()
    ) +
    geom_line()
}

save_line <- function(plot, name) {
  ggsave_dark(
    file.path(graph_folder, paste0(name, ".pdf")), plot, dark = FALSE,
    width = 10, height = 7.5, units = "cm", device = "pdf"
  )
}

base1 <- sclr_ideal_data(theta = 1e6, seed = 20191106)

# True maximum likelihood estimates
fit_lr <- glm(status ~ logHI, binomial(), base1) %>% broom::tidy()

# Newton-Raphson does not converge, falls back to gradient ascent
# MLE's reported are very close to fit_lr
fit_sclr <- sclr(status ~ logHI, base1)

par_combos <- tibble(.rows = 101) %>%
  mutate(
    theta = seq(-5, 10, length.out = n()),
    lambda = exp(theta) / (1 + exp(theta)),
    beta_0 = -fit_lr$estimate[fit_lr$term == "(Intercept)"],
    beta_logHI = -fit_lr$estimate[fit_lr$term == "logHI"],
    log_lik = pmap_dbl(
      list(theta, beta_0, beta_logHI),
      function(theta, beta_0, beta_logHI) {
        sclr_log_likelihood(
          x = fit_sclr$x, y = fit_sclr$y, pars = c(theta, beta_0, beta_logHI)
        )
      }
    )
  )

graph_line(par_combos, "lambda", bquote(lambda), seq(0, 1, 0.1))
save_line(last_plot(), "lambda1")

graph_line(par_combos, "theta", bquote(theta), seq(-5, 10, 1))
save_line(last_plot(), "theta1")
