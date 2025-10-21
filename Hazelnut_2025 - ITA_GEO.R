# 0. Caricamento del file Excel
library(readxl)
library(lme4)
library(multcompView)
library(emmeans)
library(multcomp)
library(dplyr)
library(ggplot2)
library(Matrix)
library(AICcmodavg)
library(glmmTMB)
library(pscl)
library(lsmeans)
library(DHARMa)
library("bbmle") ## for AICtab
library(performance)
library(broom.mixed)

df <- read_excel("C:/Users/luigi/Desktop/Georgia/ITA(INT)_GEO_TT.xlsx")


# Preparazione dataset
df$Anno <- format(df$Date, "%Y")
df$Mese <- format(df$Date, "%m")
df_filtrato <- subset(df, Count != -9999 & !(Mese %in% c("11")))
df_filtrato <- df[!is.na(df$Count), ]
livelli_species <- unique(df_filtrato$Species)
df_filtrato$Anno <- as.factor(df_filtrato$Anno)

# GLM negative binomial random effect

# Modello 1: Only species GEORGIA
mod1G <- glmer.nb(Count ~ Species + (1 | Anno) + (1 | Code),
                 data = subset(df_filtrato, 
                               Country == "Georgia" & 
                                 Species != "X. monographus"))

##Controllo overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type="pearson")
  chi_sq <- sum(rp^2)
  ratio <- chi_sq / rdf
  return(ratio)
}
overdisp_fun(mod1G)

###Controllo zero-inflation
check_zeroinflation(mod1G)


####Residual diagnostics
sim_res <- simulateResiduals(fittedModel = mod1G, n = 1000)
plot(sim_res)         # residui vs predetti, distribuzione, qqplot
testDispersion(sim_res)  # controlla dispersion
testUniformity(sim_res)  # verifica uniformità dei residui
testZeroInflation(sim_res)

##Intervalli confidenza
tidy(mod1G, effects = "fixed", conf.int = TRUE, conf.method = "Wald")


#####

##Preparazione per confronto Nazioni
# Definisci le date di inizio e fine per ciascun paese e anno
periodi_trappole <- data.frame(
  Country = c("Italy", "Italy", "Georgia", "Georgia"),
  Anno = c(2023, 2024, 2023, 2024),
  start_date = as.Date(c("2023-03-07", "2024-03-20", "2023-05-16", "2024-03-16")),
  end_date   = as.Date(c("2023-10-19", "2024-10-30", "2023-10-16", "2024-10-16"))
)

# Calcola il numero totale di giorni attivi
periodi_trappole$TotTrapDays <- as.numeric(periodi_trappole$end_date - periodi_trappole$start_date)
periodi_trappole

#unire questa informazione al dataset dei conteggi
df_filtrato <- merge(df_filtrato, periodi_trappole[, c("Country", "Anno", "TotTrapDays")],
                     by = c("Country", "Anno"),
                     all.x = TRUE)


#Modello per standardizzare sforzo di campionamento_OFFSET(LOG)#Country comparison
mod_offset_3G <- glmer.nb(
  Count ~ Country + offset(log(TotTrapDays)) + (1 | Anno) + (1 | Code),
  data = df_filtrato
)

##Controllo overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type="pearson")
  chi_sq <- sum(rp^2)
  ratio <- chi_sq / rdf
  return(ratio)
}
overdisp_fun(mod_offset_3G)

###Controllo zero-inflation
check_zeroinflation(mod_offset_3G)

####Residual diagnostics
sim_res <- simulateResiduals(fittedModel = mod_offset_3G, n = 1000)
plot(sim_res)         # residui vs predetti, distribuzione, qqplot
testDispersion(sim_res)  # controlla dispersion
testUniformity(sim_res)  # verifica uniformità dei residui
testZeroInflation(sim_res)

##Intervalli confidenza
tidy(mod_offset_3G, effects = "fixed", conf.int = TRUE, conf.method = "Wald")


#######

# ANALISI 1 ================ n° catture x species

print(summary(mod1G))

# Pairwise comparison:
marginal = emmeans(mod1G, ~ Species)
pairs(marginal, adjust="bh")

# Letters of significance:
lettere <- cld(marginal, alpha=0.05, Letters=letters, adjust="bh")
print(lettere)

#Grafico:
library(ggplot2)
df_spp_filtered <- subset(df_filtrato, !(Species %in% c("X. monographus")))
barplot_species_effect_GEO <- ggplot(data = df_spp_filtered, aes(x = Species, y = Count, )) +
  stat_summary(fun = "mean", geom = "bar", width = 0.5,  
               fill = c("orchid3", "darkseagreen3", "lightsalmon", "lightskyblue2")) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", 
               width = 0.2, colour = "black") +
  scale_x_discrete(labels = c(
    "A. dispar" = expression(italic("A. dispar")),
    "H. eruditus" = expression(italic("H. eruditus")),
    "X. germanus" = expression(italic("X. germanus")),
    "X. saxesenii" = expression(italic("X. saxesenii"))
  )) +
  labs(x = "Species", y = "Mean ± SE of catches per trap") +
  theme(
    panel.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5, family = "sans"),
    axis.text.y = element_text(size = 14, family = "sans"),
    axis.title.x = element_text(size = 14, family = "sans", 
                                margin = margin(t = 15)),  # spazio sopra il titolo x
    axis.title.y = element_text(size = 14, family = "sans", 
                                margin = margin(r = 10)),  # spazio a destra del titolo y
    legend.title = element_text(size = 14, family = "sans"),
    legend.text = element_text(size = 12, family = "sans")
  )
plot(barplot_species_effect_GEO)

ggsave("barplot_species_effect_GEO.png",       # nome file
       plot = barplot_species_effect_GEO,          # oggetto grafico
       width = 8, height = 6, units = "in",    # dimensioni (in pollici)
       dpi = 300,                              # risoluzione (300 = stampa, 150 = schermo)
       bg = "white")                           # sfondo bianco


####

# ANALISI 3 ================ n° catture x Nazione (considerando species) e OFFSET(LOG)
print(summary(mod_offset_3G))

# Pairwise comparison:
marginal = emmeans(mod_offset_3G, ~ Country)
pairs(marginal, adjust="bh")

# Letters of significance:
lettere <- cld(marginal, alpha=0.05, Letters=letters, adjust="bh")
print(lettere)

#Grafico:
barplot_country_effect <- ggplot(data = df_filtrato, aes(x = Country, y = Count, )) +
  stat_summary(fun = "mean", geom = "bar", width = 0.5,  
               fill = c("brown2", "seagreen3")) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.25, colour = "black") +
  labs(x = "Countries", y = "Mean ± SE of catches per trap") +
  theme(
    panel.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5, family = "sans"),
    axis.text.y = element_text(size = 14, family = "sans"),
    axis.title.x = element_text(size = 14, family = "sans", 
                                margin = margin(t = 15)),  # spazio sopra il titolo x
    axis.title.y = element_text(size = 14, family = "sans", 
                                margin = margin(r = 10)),  # spazio a destra del titolo y
    legend.title = element_text(size = 14, family = "sans"),
    legend.text = element_text(size = 12, family = "sans")
  )
plot(barplot_country_effect)

ggsave("barplot_country_effect.png",       # nome file
       plot = barplot_country_effect,          # oggetto grafico
       width = 8, height = 6, units = "in",    # dimensioni (in pollici)
       dpi = 300,                              # risoluzione (300 = stampa, 150 = schermo)
       bg = "white")                           # sfondo bianco



#####

# SPECIE ANALIZZATA: "X. saxesenii"

# Modello 3.1G: una species + NAZIONE

# Filtra il dataset per la singola species
df_subset <- subset(df_filtrato, Species == "X. saxesenii")
# Costruisci il modello
mod3.1G_sax <- glmer.nb(Count ~ Country + offset(log(TotTrapDays))
                        + (1 | Anno) + (1 | Code), data = df_subset)

##Controllo overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type="pearson")
  chi_sq <- sum(rp^2)
  ratio <- chi_sq / rdf
  return(ratio)
}
overdisp_fun(mod3.1G_sax)

###Controllo zero-inflation
check_zeroinflation(mod3.1G_sax)

####Residual diagnostics
sim_res <- simulateResiduals(fittedModel = mod3.1G_sax, n = 1000)
plot(sim_res)         # residui vs predetti, distribuzione, qqplot
testDispersion(sim_res)  # controlla dispersion
testUniformity(sim_res)  # verifica uniformità dei residui
testZeroInflation(sim_res)

##Intervalli confidenza
tidy(mod3.1G_sax, effects = "fixed", conf.int = TRUE, conf.method = "Wald")

# Stampa il modello
print(summary(mod3.1G_sax))
# Pairwise comparison:
marginal = emmeans(mod3.1G_sax, ~ Country)
pairs(marginal, adjust="bh")
# Letters of significance:
lettere <- cld(marginal, alpha=0.05, Letters=letters, adjust="bh")
print(lettere)

#Grafico:
barplot_SingleCountry_effect <- ggplot(data = df_subset, aes(x = Country, y = Count, )) +
  stat_summary(fun = "mean", geom = "bar", width = 0.5, 
               fill = c("brown2", "seagreen3")) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.25, colour = "black") +
  labs(title="X. saxesenii", x = "Countries", y = "Mean ± SE of catches per trap") +
  theme(
    panel.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5, family = "sans"),
    axis.text.y = element_text(size = 14, family = "sans"),
    axis.title.x = element_text(size = 14, family = "sans", 
                                margin = margin(t = 15)),  # spazio sopra il titolo x
    axis.title.y = element_text(size = 14, family = "sans", 
                                margin = margin(r = 10)),  # spazio a destra del titolo y
    legend.title = element_text(size = 14, family = "sans"),
    legend.text = element_text(size = 12, family = "sans")
  )
plot(barplot_SingleCountry_effect)
ggsave("barplot_SingleCountry_effect_Xsaxesenii.png",       # nome file
       plot = barplot_SingleCountry_effect,          # oggetto grafico
       width = 8, height = 6, units = "in",    # dimensioni (in pollici)
       dpi = 300,                              # risoluzione (300 = stampa, 150 = schermo)
       bg = "white")                           # sfondo bianco


# SPECIE ANALIZZATA: "X. germanus"


# Modello 3.1G: una species + NAZIONE

# Filtra il dataset per la singola species
df_subset <- subset(df_filtrato, Species == "X. germanus")
# Costruisci il modello
mod3.1G_ger <- glmer.nb(Count ~ Country + offset(log(TotTrapDays)) +
                        (1 | Anno) + (1 | Code), data = df_subset)

##Controllo overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type="pearson")
  chi_sq <- sum(rp^2)
  ratio <- chi_sq / rdf
  return(ratio)
}
overdisp_fun(mod3.1G_ger)

###Controllo zero-inflation
check_zeroinflation(mod3.1G_ger)

####Residual diagnostics
sim_res <- simulateResiduals(fittedModel = mod3.1G_ger, n = 1000)
plot(sim_res)         # residui vs predetti, distribuzione, qqplot
testDispersion(sim_res)  # controlla dispersion
testUniformity(sim_res)  # verifica uniformità dei residui
testZeroInflation(sim_res)

##Intervalli confidenza
tidy(mod3.1G_ger, effects = "fixed", conf.int = TRUE, conf.method = "Wald")


# Stampa il modello
print(summary(mod3.1G_ger))
# Pairwise comparison:
marginal = emmeans(mod3.1G_ger, ~ Country)
pairs(marginal, adjust="bh")
# Letters of significance:
lettere <- cld(marginal, alpha=0.05, Letters=letters, adjust="bh")
print(lettere)

#Grafico:
barplot_SingleCountry_effect <- ggplot(data = df_subset, aes(x = Country, y = Count, )) +
  stat_summary(fun = "mean", geom = "bar", width = 0.5, 
               fill = c("brown2", "seagreen3")) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.25, colour = "black") +
  labs(title="X. germanus", x = "Countries", y = "Mean ± SE of catches per trap") +
  theme(
    panel.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5, family = "sans"),
    axis.text.y = element_text(size = 14, family = "sans"),
    axis.title.x = element_text(size = 14, family = "sans", 
                                margin = margin(t = 15)),  # spazio sopra il titolo x
    axis.title.y = element_text(size = 14, family = "sans", 
                                margin = margin(r = 10)),  # spazio a destra del titolo y
    legend.title = element_text(size = 14, family = "sans"),
    legend.text = element_text(size = 12, family = "sans")
  )
plot(barplot_SingleCountry_effect)
ggsave("barplot_SingleCountry_effect_Xgermanus.png",       # nome file
       plot = barplot_SingleCountry_effect,          # oggetto grafico
       width = 8, height = 6, units = "in",    # dimensioni (in pollici)
       dpi = 300,                              # risoluzione (300 = stampa, 150 = schermo)
       bg = "white")                           # sfondo bianco


# SPECIE ANALIZZATA: "A. dispar"


# Modello 3.1G: una species + NAZIONE

# Filtra il dataset per la singola species
df_subset <- subset(df_filtrato, Species == "A. dispar")
# Costruisci il modello
mod3.1G_dis <- glmer.nb(Count ~ Country + offset(log(TotTrapDays)) +
                          (1 | Anno) + (1 | Code), data = df_subset)

##Controllo overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type="pearson")
  chi_sq <- sum(rp^2)
  ratio <- chi_sq / rdf
  return(ratio)
}
overdisp_fun(mod3.1G_dis)

###Controllo zero-inflation
check_zeroinflation(mod3.1G_dis)

####Residual diagnostics
sim_res <- simulateResiduals(fittedModel = mod3.1G_dis, n = 1000)
plot(sim_res)         # residui vs predetti, distribuzione, qqplot
testDispersion(sim_res)  # controlla dispersion
testUniformity(sim_res)  # verifica uniformità dei residui
testZeroInflation(sim_res)

##Intervalli confidenza
tidy(mod3.1G_dis, effects = "fixed", conf.int = TRUE, conf.method = "Wald")


# Stampa il modello
print(summary(mod3.1G_dis))
# Pairwise comparison:
marginal = emmeans(mod3.1G_dis, ~ Country)
pairs(marginal, adjust="bh")
# Letters of significance:
lettere <- cld(marginal, alpha=0.05, Letters=letters, adjust="bh")
print(lettere)

#Grafico:
barplot_SingleCountry_effect <- ggplot(data = df_subset, aes(x = Country, y = Count, )) +
  stat_summary(fun = "mean", geom = "bar", width = 0.5, 
               fill = c("brown2", "seagreen3")) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.25, colour = "black") +
  labs(title="A. dispar", x = "Countries", y = "Mean ± SE of catches per trap") +
  theme(
    panel.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5, family = "sans"),
    axis.text.y = element_text(size = 14, family = "sans"),
    axis.title.x = element_text(size = 14, family = "sans", 
                                margin = margin(t = 15)),  # spazio sopra il titolo x
    axis.title.y = element_text(size = 14, family = "sans", 
                                margin = margin(r = 10)),  # spazio a destra del titolo y
    legend.title = element_text(size = 14, family = "sans"),
    legend.text = element_text(size = 12, family = "sans")
  )
plot(barplot_SingleCountry_effect)
ggsave("barplot_SingleCountry_effect_Adispar.png",       # nome file
       plot = barplot_SingleCountry_effect,          # oggetto grafico
       width = 8, height = 6, units = "in",    # dimensioni (in pollici)
       dpi = 300,                              # risoluzione (300 = stampa, 150 = schermo)
       bg = "white")                           # sfondo bianco


# SPECIE ANALIZZATA: "H. eruditus"


# Modello 3.1G: una species + NAZIONE

# Filtra il dataset per la singola species
df_subset <- subset(df_filtrato, Species == "H. eruditus")
# Costruisci il modello
mod3.1G_eru <- glmer.nb(Count ~ Country + offset(log(TotTrapDays)) +
                          (1 | Anno) + (1 | Code), data = df_subset)

##Controllo overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type="pearson")
  chi_sq <- sum(rp^2)
  ratio <- chi_sq / rdf
  return(ratio)
}
overdisp_fun(mod3.1G_eru)

###Controllo zero-inflation
check_zeroinflation(mod3.1G_eru)

####Residual diagnostics
sim_res <- simulateResiduals(fittedModel = mod3.1G_eru, n = 1000)
plot(sim_res)         # residui vs predetti, distribuzione, qqplot
testDispersion(sim_res)  # controlla dispersion
testUniformity(sim_res)  # verifica uniformità dei residui
testZeroInflation(sim_res)

##Intervalli confidenza
tidy(mod3.1G_eru, effects = "fixed", conf.int = TRUE, conf.method = "Wald")

# Stampa il modello
print(summary(mod3.1G_eru))
# Pairwise comparison:
marginal = emmeans(mod3.1G_eru, ~ Country)
pairs(marginal, adjust="bh")
# Letters of significance:
lettere <- cld(marginal, alpha=0.05, Letters=letters, adjust="bh")
print(lettere)

#Grafico:
barplot_SingleCountry_effect <- ggplot(data = df_subset, aes(x = Country, y = Count, )) +
  stat_summary(fun = "mean", geom = "bar", width = 0.5, 
               fill = c("brown2", "seagreen3")) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.25, colour = "black") +
  labs(title="H. eruditus", x = "Countries", y = "Mean ± SE of catches per trap") +
  theme(
    panel.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5, family = "sans"),
    axis.text.y = element_text(size = 14, family = "sans"),
    axis.title.x = element_text(size = 14, family = "sans", 
                                margin = margin(t = 15)),  # spazio sopra il titolo x
    axis.title.y = element_text(size = 14, family = "sans", 
                                margin = margin(r = 10)),  # spazio a destra del titolo y
    legend.title = element_text(size = 14, family = "sans"),
    legend.text = element_text(size = 12, family = "sans")
  )
plot(barplot_SingleCountry_effect)
ggsave("barplot_SingleCountry_effect_Heruditus.png",       # nome file
       plot = barplot_SingleCountry_effect,          # oggetto grafico
       width = 8, height = 6, units = "in",    # dimensioni (in pollici)
       dpi = 300,                              # risoluzione (300 = stampa, 150 = schermo)
       bg = "white")                           # sfondo bianco
