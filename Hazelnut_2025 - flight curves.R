# 0. Caricamento del file Excel
library(readxl)
library(lme4)
library(multcompView)
library(emmeans)
library(multcomp)
library(dplyr)
library(ggplot2)

df <- read_excel("C:/Users/luigi/Desktop/Georgia/Hazelnut monitoring_22_23_24.xlsx", sheet = 1)

# Preparazione dataset
df$Anno <- format(df$Data, "%Y")
df$Mese <- format(df$Data, "%m")
df_filtrato <- subset(df, Conteggio != -9999 & Anno != 2022 & !(Mese %in% c("11")))
df_filtrato <- df_filtrato[!is.na(df_filtrato$Conteggio), ]
df_filtrato$Sito <- as.factor(df_filtrato$Sito)
livelli_specie <- unique(df_filtrato$Specie)
df_filtrato$Anno <- as.factor(df_filtrato$Anno)


df_time <- df_filtrato %>%
  group_by(Management, Specie, Data) %>%
  summarise(
    mean_count = mean(Conteggio, na.rm = TRUE),
    se = sd(Conteggio, na.rm = TRUE) / sqrt(n()),  # errore standard
    .groups = "drop"
  )

# Grafico con ggplot
ggplot(df_time, aes(x = Data, y = mean_count, color = Management, group = Management)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_count - se, ymax = mean_count + se), width = 0.2) +
  facet_wrap(~ Specie, scales = "free_y") +
  labs(
    x = "Sampling date",
    y = "Mean captures per trap",
    color = "Management strategy",
    title = "Temporal trends of mean captures per trap by management strategy"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 12)
  )


##per specie

df_filtrato$Management <- recode(df_filtrato$Management,
                                 "IMP" = "IPM",
                                 "integrated" = "IPM",
                                 "Integrated" = "IPM")
#
specie_scelta <- "X. germanus"  

df_single <- df_filtrato %>%
  filter(Specie == specie_scelta) %>%
  group_by(Management, Data) %>%
  summarise(
    mean_count = mean(Conteggio, na.rm = TRUE),
    se = sd(Conteggio, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
# Ordina i livelli di Management
df_single$Management <- factor(df_single$Management, levels = c("ORG", "IPM", "REN"))

# Grafico
ggplot(df_single, aes(x = as.Date(Data), y = mean_count, color = Management, group = Management)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_count - se, ymax = mean_count + se), width = 0.2) +
  scale_color_manual(values = c("ORG" = "#C3C4D6",
                                "IPM" = "#D08045",
                                "REN" = "#CFC45A")) +
  scale_x_date(
    date_breaks = "2 week",
    date_labels = "%d-%b",
    expand = c(0.01, 0.01)
  ) +
  labs(
    x = "Sampling date",
    y = "Mean captures per trap (± SE)",
    color = "Management",
    title = bquote("Temporal trend of " * italic(.(specie_scelta)))
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "top",
    plot.title = element_text(size = 12, hjust = 0.5),
    panel.grid.minor = element_blank()
  )

#
specie_scelta <- "X. saxesenii"  

df_single <- df_filtrato %>%
  filter(Specie == specie_scelta) %>%
  group_by(Management, Data) %>%
  summarise(
    mean_count = mean(Conteggio, na.rm = TRUE),
    se = sd(Conteggio, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
# Ordina i livelli di Management (se serve)
df_single$Management <- factor(df_single$Management, levels = c("ORG", "IPM", "REN"))

# Grafico
ggplot(df_single, aes(x = as.Date(Data), y = mean_count, color = Management, group = Management)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_count - se, ymax = mean_count + se), width = 0.2) +
  scale_color_manual(values = c("ORG" = "#C3C4D6",
                                "IPM" = "#D08045",
                                "REN" = "#CFC45A")) +
  scale_x_date(
    date_breaks = "2 week",
    date_labels = "%d-%b",
    expand = c(0.01, 0.01)
  ) +
  labs(
    x = "Sampling date",
    y = "Mean captures per trap (± SE)",
    color = "Management",
    title = bquote("Temporal trend of " * italic(.(specie_scelta)))
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "top",
    plot.title = element_text(size = 12, hjust = 0.5),
    panel.grid.minor = element_blank()
  )

#
specie_scelta <- "A. dispar"  

df_single <- df_filtrato %>%
  filter(Specie == specie_scelta) %>%
  group_by(Management, Data) %>%
  summarise(
    mean_count = mean(Conteggio, na.rm = TRUE),
    se = sd(Conteggio, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
# Ordina i livelli di Management (se serve)
df_single$Management <- factor(df_single$Management, levels = c("ORG", "IPM", "REN"))

# Grafico
ggplot(df_single, aes(x = as.Date(Data), y = mean_count, color = Management, group = Management)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_count - se, ymax = mean_count + se), width = 0.2) +
  scale_color_manual(values = c("ORG" = "#C3C4D6",
                                "IPM" = "#D08045",
                                "REN" = "#CFC45A")) +
  scale_x_date(
    date_breaks = "2 week",
    date_labels = "%d-%b",
    expand = c(0.01, 0.01)
  ) +
  labs(
    x = "Sampling date",
    y = "Mean captures per trap (± SE)",
    color = "Management",
    title = bquote("Temporal trend of " * italic(.(specie_scelta)))
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "top",
    plot.title = element_text(size = 12, hjust = 0.5),
    panel.grid.minor = element_blank()
  )

#
specie_scelta <- "H. eruditus"  

df_single <- df_filtrato %>%
  filter(Specie == specie_scelta) %>%
  group_by(Management, Data) %>%
  summarise(
    mean_count = mean(Conteggio, na.rm = TRUE),
    se = sd(Conteggio, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
# Ordina i livelli di Management (se serve)
df_single$Management <- factor(df_single$Management, levels = c("ORG", "IPM", "REN"))

# Grafico
ggplot(df_single, aes(x = as.Date(Data), y = mean_count, color = Management, group = Management)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_count - se, ymax = mean_count + se), width = 0.2) +
  scale_color_manual(values = c("ORG" = "#C3C4D6",
                                "IPM" = "#D08045",
                                "REN" = "#CFC45A")) +
  scale_x_date(
    date_breaks = "2 week",
    date_labels = "%d-%b",
    expand = c(0.01, 0.01)
  ) +
  labs(
    x = "Sampling date",
    y = "Mean captures per trap (± SE)",
    color = "Management",
    title = bquote("Temporal trend of " * italic(.(specie_scelta)))
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "top",
    plot.title = element_text(size = 12, hjust = 0.5),
    panel.grid.minor = element_blank()
  )

#
specie_scelta <- "X. monographus"  

df_single <- df_filtrato %>%
  filter(Specie == specie_scelta) %>%
  group_by(Management, Data) %>%
  summarise(
    mean_count = mean(Conteggio, na.rm = TRUE),
    se = sd(Conteggio, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
# Ordina i livelli di Management (se serve)
df_single$Management <- factor(df_single$Management, levels = c("ORG", "IPM", "REN"))

# Grafico
ggplot(df_single, aes(x = as.Date(Data), y = mean_count, color = Management, group = Management)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_count - se, ymax = mean_count + se), width = 0.2) +
  scale_color_manual(values = c("ORG" = "#C3C4D6",
                                "IPM" = "#D08045",
                                "REN" = "#CFC45A")) +
  scale_x_date(
    date_breaks = "2 week",
    date_labels = "%d-%b",
    expand = c(0.01, 0.01)
  ) +
  labs(
    x = "Sampling date",
    y = "Mean captures per trap (± SE)",
    color = "Management",
    title = bquote("Temporal trend of " * italic(.(specie_scelta)))
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "top",
    plot.title = element_text(size = 12, hjust = 0.5),
    panel.grid.minor = element_blank()
  )