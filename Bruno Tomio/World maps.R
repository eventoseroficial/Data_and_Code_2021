# PACKAGES NEEDED ####

list.of.packages <- c('WDI', 'ggthemes', 'knitr', 'kableExtra', 'rnaturalearth', 
                      'tidyverse', 'ggrepel', 'gganimate', 'transformr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = T, quietly = T)

# World maps (1/2) ####

## Code Map 1 ####

# Self-employed in 2019
indicator <- c("Self-employed" = 'SL.EMP.SELF.ZS')
datWM1 <- WDI(indicator, country="all", start = 2019, end = 2019)
Data_info <- WDI_data
name_self_employed <- as.data.frame(Data_info$series) %>%
  filter(indicator == "SL.EMP.SELF.ZS") %>%
  select(name)
source_self_employed <- as.data.frame(Data_info$series) %>%
  filter(indicator == "SL.EMP.SELF.ZS") %>%
  select(sourceOrganization)
ne_countries(returnclass = "sf") %>%
  left_join(datWM1, c("iso_a2" = "iso2c")) %>%
  filter(iso_a2 != "ATA") %>% # remove Antarctica
  ggplot(aes(fill = `Self-employed`)) +
  geom_sf() +
  scale_fill_viridis_c(labels = scales::percent_format(scale = 1)) +
  theme(legend.position="bottom") +
  labs(
    title = paste0(name_self_employed, " in 2019"),
    fill = NULL,
    caption = paste0("Source:", source_self_employed) 
  )

## Code Map 2 ####

# Self-employed in 2008
indicator <- c("Self-employed" = 'SL.EMP.SELF.ZS')
datWM2 <- WDI(indicator, country="all", start = 2008, end = 2008)
name_self_employed <- as.data.frame(Data_info$series) %>%
  filter(indicator == "SL.EMP.SELF.ZS") %>%
  select(name)
source_self_employed <- as.data.frame(Data_info$series) %>%
  filter(indicator == "SL.EMP.SELF.ZS") %>%
  select(sourceOrganization)
ne_countries(returnclass = "sf") %>%
  left_join(datWM2, c("iso_a2" = "iso2c")) %>%
  filter(iso_a2 != "ATA") %>% # remove Antarctica
  ggplot(aes(fill = `Self-employed`)) +
  geom_sf() +
  scale_fill_viridis_c(labels = scales::percent_format(scale = 1)) +
  theme(legend.position="bottom") +
  labs(
    title = paste0(name_self_employed, " in 2008"),
    fill = NULL,
    caption = paste0("Source:", source_self_employed) 
  )

## Code Map 3 ####

# Self-employed in 1991
indicator <- c("Self-employed" = 'SL.EMP.SELF.ZS')
datWM3 <- WDI(indicator, country="all", start = 1991, end = 1991)
name_self_employed <- as.data.frame(Data_info$series) %>%
  filter(indicator == "SL.EMP.SELF.ZS") %>%
  select(name)
source_self_employed <- as.data.frame(Data_info$series) %>%
  filter(indicator == "SL.EMP.SELF.ZS") %>%
  select(sourceOrganization)
ne_countries(returnclass = "sf") %>%
  left_join(datWM3, c("iso_a2" = "iso2c")) %>%
  filter(iso_a2 != "ATA") %>% # remove Antarctica
  ggplot(aes(fill = `Self-employed`)) +
  geom_sf() +
  scale_fill_viridis_c(labels = scales::percent_format(scale = 1)) +
  theme(legend.position="bottom") +
  labs(
    title = paste0(name_self_employed, " in 1991"),
    fill = NULL,
    caption = paste0("Source:", source_self_employed) 
  )

# World maps (2/2) ####

## Code Map 4 ####

# GDP per capita (constant 2010 US$) in 2019
indicator <- c("GDP per capita" = 'NY.GDP.PCAP.KD')
datWM4 <- WDI(indicator, country="all", start = 2019, end = 2019)
name_GDP_PC <- as.data.frame(Data_info$series) %>%
  filter(indicator == "NY.GDP.PCAP.KD") %>%
  select(name)
source_GDP_PC <- as.data.frame(Data_info$series) %>%
  filter(indicator == "NY.GDP.PCAP.KD") %>%
  select(sourceOrganization) 
ne_countries(returnclass = "sf") %>%
  left_join(datWM4, c("iso_a2" = "iso2c")) %>%
  filter(iso_a2 != "ATA") %>% # remove Antarctica
  ggplot(aes(fill = `GDP per capita`)) +
  geom_sf() +
  scale_fill_viridis_c(labels = scales::dollar_format(scale = 1)) +
  theme(legend.position="bottom", legend.key.width = unit(2.5, "cm")) +
  labs(
    title = paste0(name_GDP_PC, " in 2019"),
    fill = NULL,
    caption = paste0("Source:", source_GDP_PC)
  )

## Code Map 5 ####

# Search for "unemployment"
WDIsearch('unemployment')

# Save results for "unemployment"
UNEMP_search <- WDIsearch('unemployment')

# Unemployment rate,Percent,,, in February 2020 - Global Economic Monitor
indicator <- c("Unemployment" = 'UNEMPSA_')
datWM5 <- WDI(indicator, country="all",start = '2020M02', end = '2020M02')
name_UNEMP <- as.data.frame(Data_info$series) %>%
  filter(indicator == "UNEMPSA_") %>%
  select(name)
ne_countries(returnclass = "sf") %>%
  left_join(datWM5, c("iso_a3" = "iso2c")) %>%
  filter(iso_a2 != "ATA") %>% # remove Antarctica
  ggplot(aes(fill = `Unemployment`)) +
  geom_sf() +
  scale_fill_viridis_c(labels = scales::percent_format(scale = 1)) +
  theme(legend.position="bottom") +
  labs(
    title = paste0(name_UNEMP, " in February 2020"),
    fill = NULL,
    caption = "Source: Global Economic Monitor, World Bank"
  )

## Code Map 6 ####

# Search for "life"
WDIsearch('life')

# Save results for "life"
life_search <- WDIsearch('life')

# Life expectancy at birth, female (years) in 2018
indicator <- c("Life expectancy at birth, female (years)" = 'SP.DYN.LE00.FE.IN')
datWM6 <- WDI(indicator, country="all",start = '2018', end = '2018')
name_life <- as.data.frame(Data_info$series) %>%
  filter(indicator == "SP.DYN.LE00.FE.IN") %>%
  select(name)
source_life <- as.data.frame(Data_info$series) %>%
  filter(indicator == "SP.DYN.LE00.FE.IN") %>%
  select(sourceOrganization)
ne_countries(returnclass = "sf") %>%
  left_join(datWM6, c("iso_a2" = "iso2c")) %>%
  filter(iso_a2 != "ATA") %>% # remove Antarctica
  ggplot(aes(fill = `Life expectancy at birth, female (years)`)) +
  geom_sf() +
  scale_fill_viridis_c(labels = scales::number_format(scale = 1)) +
  theme(legend.position="bottom") +
  labs(
    title = paste0(name_life, " in {closest_state}"),
    fill = NULL,
    caption = paste0("Source:", source_life)
  )

## Code Map 7 (Animation) ####

# Self-employed, 1970-2019
indicator <- c("Self-employed" = 'SL.EMP.SELF.ZS')
datWM7 <- WDI(indicator, country="all", start = 1970, end = 2019) #<<

name_self_employed <- as.data.frame(Data_info$series) %>%
  filter(indicator == "SL.EMP.SELF.ZS") %>%
  select(name)

source_self_employed <- as.data.frame(Data_info$series) %>%
  filter(indicator == "SL.EMP.SELF.ZS") %>%
  select(sourceOrganization)

ne_countries(returnclass = "sf") %>%
  left_join(datWM7, c("iso_a2" = "iso2c")) %>% #<<
  filter(iso_a2 != "ATA") %>% # remove Antarctica
  ggplot(aes(fill = `Self-employed`)) +
  geom_sf() +
  scale_fill_viridis_c(labels = scales::percent_format(scale = 1)) +
  theme(legend.position="bottom") +
  labs(
    title = paste0(name_self_employed, " in {closest_state}"), #<<
    fill = NULL,
    caption = paste0("Source:", source_self_employed)) + 
  transition_states(year, transition_length = 3, state_length = 1)

## Code Map 8 (Animation) ####

# GDP per capita (constant 2010 US$), 1990-2019
indicator <- c("GDP per capita" = 'NY.GDP.PCAP.KD') #<<
datWM8 <- WDI(indicator, country="all", start = 1990, end = 2019) #<<

name_GDP_PC <- as.data.frame(Data_info$series) %>% 
  filter(indicator == "NY.GDP.PCAP.KD") %>% 
  select(name)

source_GDP_PC <- as.data.frame(Data_info$series) %>% 
  filter(indicator == "NY.GDP.PCAP.KD") %>% 
  select(sourceOrganization) 

ne_countries(returnclass = "sf") %>%
  left_join(datWM8, c("iso_a2" = "iso2c")) %>% 
  filter(iso_a2 != "ATA") %>% # remove Antarctica
  ggplot(aes(fill = `GDP per capita`)) + 
  geom_sf() +
  scale_fill_viridis_c(labels = scales::dollar_format(scale = 1)) + 
  theme(legend.position="bottom", legend.key.width = unit(2.5, "cm")) + 
  labs(
    title = paste0(name_GDP_PC, " in {closest_state}"), #<<
    fill = NULL,
    caption = paste0("Source:", source_GDP_PC)) +
  transition_states(year, transition_length = 3, state_length = 1)

#WM8 <-
#ne_countries(returnclass = "sf") %>%
#  left_join(datWM7, c("iso_a2" = "iso2c")) %>% 
#  filter(iso_a2 != "ATA") %>% # remove Antarctica
#  ggplot(aes(fill = `GDP per capita`)) + 
#  geom_sf() +
#  scale_fill_viridis_c(labels = scales::dollar_format(scale = 1)) + 
#  theme(legend.position="bottom", legend.key.width = unit(2.5, "cm")) + 
#  labs(
#    title = paste0(name_GDP_PC, " in {closest_state}"), #<<
#    fill = NULL,
#    caption = paste0("Source:", source_GDP_PC)) +
#  transition_states(year, transition_length = 3, state_length = 1)

#animate(WM8, nframes = 150, fps = 5, res = 100, width = 900, height = 600,
#          end_pause = 20, renderer=gifski_renderer("WM8.gif"))

## Code Map 9 (Animation) ####

# Life expectancy at birth, female (years), 1970-2018
indicator <- c("Life expectancy at birth, female (years)" = 'SP.DYN.LE00.FE.IN')
datWM9 <- WDI(indicator, country="all",start = '1970', end = '2018')

name_life <- as.data.frame(Data_info$series) %>%
  filter(indicator == "SP.DYN.LE00.FE.IN") %>%
  select(name)

source_life <- as.data.frame(Data_info$series) %>%
  filter(indicator == "SP.DYN.LE00.FE.IN") %>%
  select(sourceOrganization)

ne_countries(returnclass = "sf") %>%
  left_join(datWM9, c("iso_a2" = "iso2c")) %>%
  filter(iso_a2 != "ATA") %>% # remove Antarctica
  ggplot(aes(fill = `Life expectancy at birth, female (years)`)) +
  geom_sf() +
  scale_fill_viridis_c(labels = scales::number_format(scale = 1)) +
  theme(legend.position="bottom") +
  labs(
    title = paste0(name_life, " in {closest_state}"),
    fill = NULL,
    caption = paste0("Source:", source_life)
  ) +
  transition_states(year, transition_length = 3, state_length = 1)