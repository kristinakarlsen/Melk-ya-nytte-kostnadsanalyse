library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)

#Data for Hammerfest LNG
Utslipp_data <- read_excel("Anleggseksport.xlsx", 
                             sheet = "Utslipp", skip = 1)

Produksjonsvolum_data <- read_excel("Anleggseksport.xlsx", 
                           sheet = "Produksjonsvolum", skip = 1)

Energiforbruk_data <- read_excel("Anleggseksport.xlsx", 
                           sheet = "Energiforbruk", skip = 1)


#Filtrerer data. Inkluderer kun relevante variabler
co2_data <- Utslipp_data %>% 
  filter(Komponent %in% c("CO2-ekvivalent", "Karbondioksid")) %>% 
  select(c(År, Komponent, 
           "Utslipp" = `Årlig utslipp til luft`,
           Enhet)) %>% 
  mutate(Utslipp = ifelse(Enhet == "1000 tonn", Utslipp * 1000, Utslipp),
         Enhet = ifelse(Enhet == "1000 tonn", "tonn", Enhet))


#Utvikling utslipp fra Hammerfest LNG  
co2_data %>% 
  ggplot(aes(x = År, y = Utslipp, color = Komponent)) +
  geom_line() +
  theme_classic()


#wide-format
co2_data <- co2_data %>% 
  select(c(År, Komponent, Utslipp)) %>% 
  pivot_wider(names_from = "Komponent", values_from = "Utslipp")

Produksjon_df1 <- Produksjonsvolum_data
Produksjon_df1$Rammekrav <- ifelse(Produksjon_df1$Rammekrav == "Produksjon av LNG", 
                                   "LNG", 
                                   Produksjon_df1$Rammekrav)

Produksjon_df1 <- Produksjon_df1 %>% 
  select(c(År, Rammekrav, Mengde)) %>% 
  pivot_wider(names_from = Rammekrav, values_from = Mengde)


Energiforbruk_df2 <- Energiforbruk_data %>% 
  select(c(År, Energibærer, Mengde)) %>% 
  pivot_wider(names_from = "Energibærer", values_from = "Mengde")


UF_df <- left_join(co2_data, Energiforbruk_df2, by = "År")

Melkøya_df1 <- left_join(UF_df, Produksjon_df1, by = "År", suffix = c(" Forbruk", " Produsert")) %>% 
  filter(År > 2008, 
         År < 2020)

Melkøya_df1$`LNG Produsert` <- as.numeric(Melkøya_df1$`LNG Produsert`)

#oversikt
Melkøya_df1 %>% 
  ggplot(aes(x = År, y = `LNG Produsert`)) +
  geom_line() +
  theme_classic()

Melkøya_df1 %>% 
  ggplot(aes(x = År, y = `CO2-ekvivalent`)) +
  geom_line() +
  theme_classic()


Melkøya_df1 %>% 
  ggplot(aes(x = `elektrisk kraft`, y = Karbondioksid))+
  geom_point()+
  theme_bw()

Melkøya_df1 %>% 
  ggplot(aes(x = `LNG Forbruk`, y = Karbondioksid))+
  geom_point()+
  theme_bw()

Melkøya_df1 %>% 
  ggplot(aes(x = `naturgass`, y = Karbondioksid))+
  geom_point()+
  theme_bw()

Melkøya_df1 %>% 
  ggplot(aes(x = `LNG Produsert`, y = `CO2-ekvivalent`))+
  geom_point()+
  theme_bw()

#ingen klar linær sammenheng


#Beregner gjennomsnittlig utslipp for alle år tilgjenglig 2007-2022
Tabell_1.1 <- co2_data %>% 
  summarise(CO2_ekvivalent_gj1 = mean(`CO2-ekvivalent`), 
            CO2_gj1 = mean(Karbondioksid))
 
#Foretar flere beregninger for å kontrollere for unormale verdier: 
#2007: Utslippene var høyere dette året pga innkjøringsproblemer
#2020-2022: Brann i 2020, anlegg stengt frem til 2021.  
Tabell_1.2 <- co2_data %>% 
  filter(År > 2007, 
         År < 2020) %>% 
  summarise(CO2_ekvivalent_gj2 = mean(`CO2-ekvivalent`), 
            CO2_gj2 = mean(Karbondioksid))

#2007-2008: Ekstremverdier. 
#Nylig etablert anlegg. kan medføre variasjon i data som følge av etableringsforhold. 
#for å se tendenser og estimere videre utslipp, er det hensiktsmessig å se på statistikk 
#fra en periode når produksjonen er mer stabil. 
Tabell_1.3 <- co2_data %>% 
  filter(År > 2008) %>% 
  summarise(CO2_ekvivalent_gj3 = mean(`CO2-ekvivalent`), 
            CO2_gj3 = mean(Karbondioksid))

#Oversikt klimagassutslipp
Tabell_1 <- tibble(
  Gjennomsnitt = c("År: 2007 - 2022", "År: 2008 - 2019", "År: 2009 - 2022"), 
  CO2_ekvivalent = c(Tabell_1.1$CO2_ekvivalent_gj1, Tabell_1.2$CO2_ekvivalent_gj2, Tabell_1.3$CO2_ekvivalent_gj3), 
  CO2 = c(Tabell_1.1$CO2_gj1, Tabell_1.2$CO2_gj2, Tabell_1.3$CO2_gj3)
)

Tabell_1

Tabell_2 <- Produksjon_df1 %>% 
  select(År, LNG)
  

#Enkel regresjonsanalyse
utslipp <- co2_data %>%
  filter(År > 2008, 
         År < 2020)
  
reg_utslipp <- lm(`CO2-ekvivalent` ~ År, utslipp)
summary(reg_utslipp)

reg_2 <- lm(`CO2-ekvivalent` ~ `LNG Produsert`, Melkøya_df1)
summary(reg_2)

ggplot(Melkøya_df1, aes(x = `LNG Produsert`, y = Karbondioksid))+
  geom_point()+
  geom_line(data = reg_2, color = "pink")+
  geom_text(aes(label = År)) +
  theme_classic()


length(unlist(Melkøya_df1$`CO2 til lagring`))
nrow(Melkøya_df1)

Melkøya_df1$`CO2 til lagring` <- sapply(Melkøya_df1$`CO2 til lagring`, function(x) ifelse(is.null(x), 0, x))

Tabell_3 <- Melkøya_df1 %>% 
  select(År, `CO2-ekvivalent`, Karbondioksid, `LNG Produsert`, 
         `CO2 til lagring`, fakkelgass)

reg_3 <- lm(`CO2-ekvivalent` ~ År + `LNG Produsert` + fakkelgass + `CO2 til lagring`, 
            data = Melkøya_df1)

summary(reg_3)

names(Produksjon_df1)

Melkøya_df1$`elektrisk kraft` <- sapply(Melkøya_df1$`elektrisk kraft`, function(x) ifelse(is.null(x), 0, x))

reg_4 <- lm(`CO2-ekvivalent` ~ `LNG Forbruk` + `elektrisk kraft` + fakkelgass + diesel, 
            data = Melkøya_df1)
summary(reg_4)

#######################

reg_LNG <- lm(`CO2-ekvivalent` ~ År + `LNG Produsert`, data = Melkøya_df1)
summary(reg_LNG)

#tabell_4 <- Produksjonsvolum_data %>% 
  filter(Enhet %in% "tonn") %>% 
  group_by(År) %>%
  summarise(Mengde, Grense, sum_mengde = sum(Mengde), 
            sum_grense = sum(Grense))


  