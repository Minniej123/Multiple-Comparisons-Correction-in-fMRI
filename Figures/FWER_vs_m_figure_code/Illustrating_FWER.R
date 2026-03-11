library(ggplot2)
#plot for illustrating FWER 
set.seed(1)

alpha= 0.05
m_vals= 1:100
n_sim= 10000
fwer_hat <- sapply(m_vals, function(m) {
  mean(replicate(n_sim, any(runif(m) <= alpha))) 
}) #P(at least one p <= alpha)
se_hat <- sqrt(fwer_hat * (1 - fwer_hat) / n_sim) #error

df <- data.frame(
  m = m_vals,
  fwer = fwer_hat,
  se = se_hat
)

p= ggplot(df, aes(x = m, y = fwer)) +
  geom_line(linewidth = 1) +
  geom_ribbon(
    aes(ymin = fwer - 1.96 * se, ymax = fwer + 1.96 * se),
    alpha = 0.2
  ) +
  labs(
    title = "FWER vs number of tests (m)",
    x = "Number of Tests (m)",
    y = "P(at least one Type I error)"
  )

ggsave(
  filename = "FWER_plot.png",
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)
