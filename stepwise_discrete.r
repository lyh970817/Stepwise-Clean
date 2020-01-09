require(tidyverse)

sex_plot <- function(data, sex = "Sex") {
  data <- rename(data, Sex = sex)
  sex_plot <- ggplot(
    data = subset(data, !is.na(data[["Sex"]])),
    aes(x = Sex, fill = Sex)
  ) +
    geom_bar(stat = "count", position = position_dodge()) +
    scale_fill_manual(values = palette) +
    scale_color_manual(values = palette) +
    theme(
      panel.grid.major.x = element_line(
        size = 0.5,
        linetype = "dashed",
        colour = "gray"
      ),
      axis.title.y = element_blank(),
      axis.text.x = element_text(colour = "black", size = 12),
      axis.text.y = element_text(colour = "black", size = 12),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_blank(),
      legend.position = "bottom"
    ) +
    labs(
      y = "Frequency",
      title = "Biological sex\n(self-report)",
      subtitle = paste("n = ", sum(!is.na(data[["Sex"]])),
        "; NA = ", sum(is.na(data[["Sex"]])),
        "; n(total) = ", length(data[["Sex"]]),
        sep = ""
      ),
      fill = "Sex"
    ) +
    geom_text(stat = "count", aes(label = ..count..), hjust = -0.1, color = "black", size = 4) +
    coord_flip(clip = "off")
  return(sex_plot)
}

factor_plot <- function(data, var, sex = "Sex", googlesheet) {
  data <- rename(data, Sex = sex)
  title <- googlesheet[["title"]][googlesheet[["newvar"]] == var] %>%
    unique() %>%
    na.omit()
  if (length(title) > 1) {
    warning("More than one question matching the variable name, only the first will be used.")
    title <- title[1]
  }

  factor_plot <- ggplot(
    data = subset(data, !is.na(data[[var]])),
    aes_string(x = var, fill = "Sex")
  ) +
    geom_bar(stat = "count", position = position_dodge()) +
    scale_fill_manual(values = palette) +
    scale_color_manual(values = palette) +
    theme(
      panel.grid.major.x = element_line(
        size = 0.5,
        linetype = "dashed",
        colour = "gray"
      ),
      axis.title.y = element_blank(),
      axis.text.x = element_text(colour = "black", size = 12),
      axis.text.y = element_text(colour = "black", size = 12),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_blank(),
      legend.position = "bottom"
    ) +
    labs(
      y = "Frequency",
      title = title,
      subtitle = paste("n = ", sum(!is.na(data[[var]])),
        "; NA = ", sum(is.na(data[[var]])),
        "; n(total) = ", length(data[[var]]),
        "; n(female) = ", sum(!is.na(data[data[["Sex"]] == "Female", ][[var]])),
        "; n(male) = ", sum(!is.na(data[data[["Sex"]] == "Male", ][[var]])),
        sep = ""
      ),
      fill = "Sex"
    ) +
    geom_text(
      stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9),
      hjust = -0.25, color = "black", size = 4
    ) +
    coord_flip(clip = "off")

  return(factor_plot)
}
