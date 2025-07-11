
library(ggplot2)
library(dplyr)

data <- data.frame(
  Category = rep(c("Landings", "Effort", "Sea days", "Fleet size", "Fuel use",
                   "Landingskm2", "Effortkm2", "Sea dayskm2", "Fleet sizekm2", "Fuel usekm2"), 3),
  Score = c(2521,128,0.46,9420,393,
            c(2521,128,0.46,9420,393)/1315,
            3876,285,1.25,15115,990,
            c(3876,285,1.25,15115,990)/1421,
            1281,123,0.22,6626,452,
            c(1281,123,0.22,6626,452)/375),
  Region = rep(c("Alaska", "NW EU", "NW Atlantic"), each = 10)
)

data$Category <- factor(data$Category, levels = unique(data$Category))

data_norm <- data %>%
  group_by(Category) %>%
  mutate(NormalizedScore = Score / max(Score)) %>%
  ungroup()

# Number of categories
n_cat <- length(levels(data$Category))

# Create data for dividers: one narrow bar per boundary, placed between categories
divider_df <- data.frame(
  Category = levels(data$Category),
  NormalizedScore = 1,    # full height
  Region = "Divider"
)

divider_df$Category <- factor(divider_df$Category, levels = levels(data$Category))

plot_data <- bind_rows(data_norm, divider_df)

plot_data$Region <- factor(plot_data$Region, levels = c("Alaska", "NW EU", "NW Atlantic", "Divider"))

pdf("../Output/Figure4_sub_operations.pdf",width = 7,height = 7)
layout(matrix(c(1:16), nrow = 4, ncol = 4, byrow = TRUE))
tr <- ggplot() +
  # Draw divider bars very narrow and full height, dodge = 0 so bars sit in the center of categories
  geom_bar(data = subset(plot_data, Region != "Divider"),
           aes(x = Category, y = NormalizedScore, fill = Region, color=Region),
           stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.5) +
  coord_polar(start = 0) +
  scale_fill_manual(values = c(
    "Alaska" = "#FF000026",
    "NW EU" = "#00000026",
    "NW Atlantic" = "#0000FF26"
  )) +
scale_color_manual(values = c(
  "Alaska" = "red",
  "NW EU" = "black",
  "NW Atlantic" = "blue"
)) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  labs(title = "Fishing operations")

tr +   geom_bar(data = subset(plot_data, Region == "Divider"),
                aes(x = Category, y = NormalizedScore),
                stat = "identity",
                fill = "gray90",
                width = 0.01,
                position = position_nudge(x = 0.5),  # shift dividers halfway between categories
                show.legend = FALSE)

dev.off()

# -----------------------------------------------


library(ggplot2)
library(dplyr)

data <- data.frame(
  Category = rep(c("Value","Landings l-1","Landings eff-1","Value/fuel cost","Income",
                   "Valuekm2"), 3),
  Score = c(2.381,6.4,20,6.8,36,
            c(2.381)/1315,
            5.021,3.9,14,6.5,30,
            c(5.021)/1421,
            4.412,2.8,10,16,24,
            c(4.412)/375),
  Region = rep(c("Alaska", "NW EU", "NW Atlantic"), each = 6)
)

data$Category <- factor(data$Category, levels = unique(data$Category))

data_norm <- data %>%
  group_by(Category) %>%
  mutate(NormalizedScore = Score / max(Score)) %>%
  ungroup()

# Number of categories
n_cat <- length(levels(data$Category))

# Create data for dividers: one narrow bar per boundary, placed between categories
divider_df <- data.frame(
  Category = levels(data$Category),
  NormalizedScore = 1,    # full height
  Region = "Divider"
)

divider_df$Category <- factor(divider_df$Category, levels = levels(data$Category))

plot_data <- bind_rows(data_norm, divider_df)

plot_data$Region <- factor(plot_data$Region, levels = c("Alaska", "NW EU", "NW Atlantic", "Divider"))

pdf("../Output/Figure4_sub_effic.pdf",width = 7,height = 7)

tr <- ggplot() +
  # Draw divider bars very narrow and full height, dodge = 0 so bars sit in the center of categories
  geom_bar(data = subset(plot_data, Region != "Divider"),
           aes(x = Category, y = NormalizedScore, fill = Region, color=Region),
           stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.5) +
  coord_polar(start = 0) +
  scale_fill_manual(values = c(
    "Alaska" = "#FF000026",
    "NW EU" = "#00000026",
    "NW Atlantic" = "#0000FF26"
  )) +
  scale_color_manual(values = c(
    "Alaska" = "red",
    "NW EU" = "black",
    "NW Atlantic" = "blue"
  )) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  labs(title = "Fleet efficiency")

tr +   geom_bar(data = subset(plot_data, Region == "Divider"),
                aes(x = Category, y = NormalizedScore),
                stat = "identity",
                fill = "gray90",
                width = 0.01,
                position = position_nudge(x = 0.5),  # shift dividers halfway between categories
                show.legend = FALSE)

dev.off()