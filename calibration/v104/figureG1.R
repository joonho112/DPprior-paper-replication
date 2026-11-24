# Author: JoonHo Lee (jlee296@ua.edu)
# Plot the newly fitted 29-point frontier using the published artwork settings.
curve <- ex_read("methods104", "worked-grid.csv")
policy <- yaml::read_yaml(rp_path("calibration", "policy.yml"))
trigger <- policy$dual$trigger_above
target <- policy$dual$soft_target
mu_K <- 5
var_K <- 10
curve$achieved_tail <- ex_tail(curve$a, curve$b, .5)
curve$eligible <- curve$soft_policy_eligible
soft <- curve[curve$selected, ]
selected_lambda <- soft$lambda
curve$role <- ifelse(curve$selected, "Selected (lambda = 0.30)",
                     ifelse(curve$eligible, "Verified eligible", "Verified, above trigger"))
base_family <- v3_font
theme_pub <- function(base_size = 10) {
  theme_minimal(base_family = base_family, base_size = base_size) +
    theme(panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "#B7BCC1", fill = NA, linewidth = .3),
          plot.background = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.title = element_text(face = "bold", size = base_size + 1.5,
                                    colour = "#202124"),
          plot.subtitle = element_text(colour = "#4F555B", size = base_size - .5),
          legend.position = "bottom")
}
role_colors <- c("Verified eligible" = "#2F6B9A",
                 "Verified, above trigger" = "#B0B7BF",
                 "Selected (lambda = 0.30)" = "#B42318")
ref_lines <- function() list(
  geom_hline(yintercept = trigger, linetype = "22", colour = "#3B4045", linewidth = .4),
  geom_hline(yintercept = target, linetype = "13", colour = "#3B4045", linewidth = .4),
  annotate("text", x = Inf, y = trigger, hjust = 1.02, vjust = -0.5, size = 3,
           colour = "#3B4045", family = base_family, label = "0.40 selection trigger"),
  annotate("text", x = Inf, y = target, hjust = 1.02, vjust = -0.5, size = 3,
           colour = "#3B4045", family = base_family, label = "0.25 soft target"))

p_g4 <- ggplot(curve, aes(x = lambda, y = achieved_tail)) +
  ref_lines() +
  geom_line(colour = "#9AA0A6", linewidth = .4) +
  geom_point(aes(colour = role), size = 2.1) +
  annotate("text", x = selected_lambda, y = soft$achieved_tail - .02,
           hjust = 0, size = 3.1, family = base_family,
           label = "largest eligible lambda = 0.30") +
  scale_colour_manual(values = role_colors, name = NULL) +
  scale_x_continuous(breaks = c(.01, .1, .2, .3, .4, .6, .8, 1)) +
  labs(title = "Fixed 29-point lambda grid for the worked example",
       subtitle = sprintf(
         "J = 50, mu_K = %d, sigma_K^2 = %d.\nSelection rule: largest verified interior lambda with tail at or below 0.40.",
         mu_K, var_K),
       x = expression(lambda),
       y = expression(paste("Pr(", W[SB] > 0.5, ")"))) +
  theme_pub()

ex_figure("osm-figure-G.1", p_g4, curve, c("data-derived/methods104/worked-grid.csv", "calibration/policy.yml"),
          width = 7.4, height = 5, tier = "fresh G.2 calibration",
          note = "All verified candidates are shown; the maximum eligible lambda is 0.30.")
