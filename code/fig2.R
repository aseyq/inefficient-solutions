library(tidyverse)
library(patchwork)

# get data
df_long <- read_csv("data/df_long.csv")  
glimpse(df_long)
df_long <- df_long %>% 
  mutate(net_payoff = 72 + 80 * plants_treated - cost)
# set theme
theme_set(theme_bw())

df_long_feedbacks <- df_long %>% 
  filter(period == 6)


######
treatment_names <- c("high_appeal"="High appeal", 
                     "low_appeal"="Low appeal"
)

treatment_colors <- c("high_appeal"= "#6A9FCC", 
                      "low_appeal" = "#c0c0c0"
)

### Net payoff of transmitted solutions
Fig2A <- df_long_feedbacks %>% 
  group_by(treatment_appeal, generation) %>%
  summarise(mean = mean(net_payoff),
            sd = sd(net_payoff),
            n = n(),
  )  %>%
  mutate(se = sd/sqrt(n))  %>%
  ggplot(aes(x=generation, y=mean, color=treatment_appeal, group=interaction(treatment_appeal,generation))) +
  
  geom_point(size = 4) +
  geom_linerange(aes(ymin = mean-se, ymax = mean+se, linewidth = 0.01)) +
  geom_line(aes(group=treatment_appeal),linewidth = 0.5) +

  scale_color_manual(labels = treatment_names, values = treatment_colors) +
  ylim(600, 750) +
  labs(x="Generation", y="Score of transmitted solutions")+
  guides(color = guide_legend(title = NULL), linewidth = "none")+
  
  theme_classic() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  theme(legend.position = "top")+
  scale_linewidth(range = c(0.2, 1))+
  scale_size_identity()

###############################

### Cost of transmitted solutions
Fig2B <- df_long_feedbacks %>% 
  group_by(treatment_appeal, generation) %>%
  summarise(mean = mean(cost),
            sd = sd(cost),
            n = n(),
  )  %>%
  mutate(se = sd/sqrt(n))  %>%
  ggplot(aes(x=generation, y=mean, color=treatment_appeal, group=interaction(treatment_appeal,generation))) +
  geom_hline(yintercept = 98, linetype = "dotted", color="black", linewidth=0.5, alpha = .5) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = mean-se, ymax = mean+se, linewidth = 0.01)) +
  geom_line(aes(group=treatment_appeal),linewidth = 0.5) +

  
  scale_color_manual(labels = treatment_names, values = treatment_colors) +
  ylim(0, 100) +
  labs(x="Generation", y="Cost of transmitted solutions")+
  guides(color = guide_legend(title = NULL), linewidth = "none")+
  theme_classic() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  theme(legend.position = "top")+
  scale_linewidth(range = c(0.2, 1))+
  scale_size_identity()



# Figure 2

wrap_plots(Fig2A, Fig2B) + 
  plot_layout(ncol = 2, widths = c(1, 1)) +
  plot_annotation(tag_levels = 'A') &
  theme(
    plot.margin = margin(0, 20, 0, 20),
    plot.tag = element_text(size = 16, face = "bold")  # Increase size and bold if desired
  )
  
ggsave("figures/fig2.png", width = 10, height = 5, dpi = 300)
  
  
  
