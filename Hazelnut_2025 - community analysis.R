library(vegan)
library(tidyr)
library(dplyr)
library(indicspecies)

df <- read_excel("C:/Users/luigi/Desktop/Georgia/Hazelnut monitoring_22_23_24.xlsx", sheet = 1)


# Preparazione dataset
df$Anno <- format(df$Data, "%Y")
df$Mese <- format(df$Data, "%m")
df_filtrato <- subset(df, Conteggio != -9999 & Anno != 2022 & !(Mese %in% c("11")))
df_filtrato <- df_filtrato[!is.na(df_filtrato$Conteggio), ]
df_filtrato$Sito <- as.factor(df_filtrato$Sito)
livelli_specie <- unique(df_filtrato$Specie)
df_filtrato$Anno <- as.factor(df_filtrato$Anno)

df_filtrato$Man <- recode(df_filtrato$Management,
                                 "IMP" = "IPM",
                                 "integrated" = "IPM",
                                 "Integrated" = "IPM")


# Crea la matrice delle specie (righe = siti, colonne = specie)
df_species <- df_filtrato %>%
  group_by(Code, Specie) %>%
  summarise(Abundance = sum(Conteggio), .groups = "drop") %>%
  pivot_wider(names_from = Specie, values_from = Abundance, values_fill = 0)

# Crea un dataframe di metadati
df_metadata <- df_filtrato %>%
  distinct(Code, Management) %>%
  arrange(match(Code, df_species$Code))

# Allinea le righe tra df_species e df_metadata
df_species <- df_species %>%
  arrange(match(Code, df_metadata$Code))

# Rimuovi la colonna Code dalla matrice delle specie
rownames(df_species) <- df_species$Code
df_species <- df_species %>% select(-Code)

#Alfa-diversity
alpha_shannon <- diversity(df_species, index = "shannon")
alpha_simpson <- diversity(df_species, index = "simpson")

alpha_df <- data.frame(
  Site = rownames(df_species),
  Management = df_metadata$Management,
  Shannon = alpha_shannon,
  Simpson = alpha_simpson
)

# Test tra gestioni
kruskal.test(Shannon ~ Management, data = alpha_df)
kruskal.test(Simpson ~ Management, data = alpha_df)

#post-hoc
library(PMCMRplus)
library(FSA)

# Dunn post hoc per Shannon
dunn_shannon <- dunnTest(Shannon ~ Management, data = alpha_df, method = "bh")
dunn_shannon


# Dunn post hoc per Simpson
dunn_simpson <- dunnTest(Simpson ~ Management, data = alpha_df, method = "bh")
dunn_simpson


# Boxplot Shannon
ggplot(alpha_df, aes(x = Management, y = Shannon, fill = Management)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Shannon diversity by Management",
       x = "Management type",
       y = "Shannon index") +
  scale_fill_manual(
    values = c("ORG" = "#C3C4D6",
               "IPM" = "#D08046",
               "REN" = "#CFC45A")
  ) +
  theme(
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1.2, "lines"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11)
  )
# Boxplot Simpson
ggplot(alpha_df, aes(x = Management, y = Simpson, fill = Management)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Simpson diversity by Management",
       x = "Management type",
       y = "Simpson index") +
  scale_fill_manual(
    values = c("ORG" = "#C3C4D6",
               "IPM" = "#D08046",
               "REN" = "#CFC45A")
  ) +
  theme(
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1.2, "lines"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11)
  )

#β-diversità (Bray–Curtis)
bray_dist <- vegdist(df_species, method = "bray")

# PERMANOVA
adonis_res <- adonis2(bray_dist ~ Management, data = df_metadata, permutations = 999)
print(adonis_res)

# NMDS
nmds <- metaMDS(df_species, distance = "bray", k = 2)
plot(nmds, type = "t")
ordiellipse(nmds, df_metadata$Management, kind = "se", conf = 0.95, label = TRUE)

## NMDS con colori
colors <- c("ORG" = "#C3C4D6", "IPM" = "#D08046", "REN" = "#CFC45A")

# Assicurati che Management sia fattore
df_metadata$Management <- factor(df_metadata$Management, levels = c("ORG","IPM","REN"))

# Genera il plot NMDS vuoto
plot(nmds, type = "n")  # type="n" evita che punti predefiniti vengano tracciati

# Aggiungi i punti colorati
points(nmds, display = "sites", col = colors[df_metadata$Management], pch = 16, cex = 1.2)

# Aggiungi le ellissi 95% per ciascun gruppo
ordiellipse(nmds, df_metadata$Management, kind = "se", conf = 0.95, label = TRUE, 
            col = colors)

# Aggiungi legenda
legend("topright", legend = levels(df_metadata$Management), 
       col = colors, pch = 16, bty = "n", pt.cex = 1.5)



#Analisi delle specie indicatrici
indicator_res <- multipatt(df_species, df_metadata$Management, func = "IndVal.g", control = how(nperm = 999))
summary(indicator_res)