# PACKAGES NEEDED ####

list.of.packages <- c('WDI', 'dplyr', 'ggplot2', 'ggthemes', 'knitr', 'kableExtra', 'rnaturalearth', 'tidyverse', 'ggrepel')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = T, quietly = T)

# CODE HRG 1 ####

indicators <- c(life_exp = "SP.DYN.LE00.IN", 
                gdp_capita ="NY.GDP.PCAP.CD",
                pop = "SP.POP.TOTL")
hrg <- WDI(indicators, country="all", start = "2018", end = "2018")
Data_info <- WDI_data
Data_countries <- as.data.frame(Data_info$country) 
hrg %>%
  left_join(Data_countries, "iso2c") %>%
  filter(region != "Aggregates") %>% # remove aggregates (groups of countries)
  ggplot() +
  geom_point(aes(x = gdp_capita, y = life_exp, size = pop, color = region)) +
  scale_x_continuous(
    labels = scales::dollar_format(),
    breaks = scales::log_breaks(n = 10)) +
  coord_trans(x = 'log10') +
  scale_size_continuous(
    labels = scales::number_format(scale = 1/1e6, suffix = "m"),
    breaks = seq(1e8,1e9, 2e8),
    range = c(1,20)) +
  theme_minimal() +
  labs(title = "An Example of Hans Rosling's Gapminder using WDI (Data for 2018)",
    x = "GDP per capita (log scale)",
    y = "Life expectancy at birth",
    size = "Population",
    color = NULL,
    caption = "Source: World Bank")

# CODE HRG 2 ####

hrg2 <- hrg %>%
  left_join(Data_countries, "iso2c") %>%
  filter(region != "Aggregates") # remove aggregates (groups of countries)
ggplot(hrg2) +
  geom_point(
    aes(x = gdp_capita, y = life_exp, size = pop, color = region)) +
  scale_x_continuous(
    labels = scales::dollar_format(),
    breaks = scales::log_breaks(n = 10)) +
  coord_trans(x = 'log10') +
  scale_size_continuous(
    labels = scales::number_format(scale = 1/1e6, suffix = "m"),
    breaks = seq(1e8,1e9, 2e8),
    range = c(1,20)) +
  theme_minimal() +
  labs(x = "GDP per capita (log scale)",
       y = "Life expectancy at birth",
       size = "Population",
       color = NULL,
       caption = "Source: World Bank") +
  geom_label_repel(data = subset(hrg2, life_exp > 84 | life_exp < 55),
                   aes(x = gdp_capita, y = life_exp, label = country.x),
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50')

# CODE HRG 3 ####

ggplot(hrg2) +
  geom_point(
    aes(x = gdp_capita, y = life_exp, size = pop, color = region)) +
  scale_x_continuous(
    labels = scales::dollar_format(),
    breaks = scales::log_breaks(n = 10)) +
  coord_trans(x = 'log10') +
  scale_size_continuous(
    labels = scales::number_format(scale = 1/1e6, suffix = "m"),
    breaks = seq(1e8,1e9, 2e8),
    range = c(1,20)) +
  theme_minimal() +
  labs(x = "GDP per capita (log scale)",
       y = "Life expectancy at birth",
       size = "Population",
       color = NULL,
       caption = "Source: World Bank") +
  geom_label_repel(data = subset(hrg2, pop > 90000000), # 90 millions
                   aes(x = gdp_capita, y = life_exp, label = country.x),
                   box.padding   = 0.9,
                   point.padding = 0.9,
                   segment.color = 'grey50')
