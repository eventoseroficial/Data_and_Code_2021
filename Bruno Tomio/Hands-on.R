# PACKAGES NEEDED ####

list.of.packages <- c('WDI', 'ggthemes', 'knitr', 'kableExtra', 'rnaturalearth', 
                      'tidyverse', 'ggrepel', 'gganimate', 'transformr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = T, quietly = T)

# SLIDE 2 ####

# Search for "GDP"
WDIsearch('GDP')

# Save results for "GDP"
GDP_search <- WDIsearch('GDP')

# SLIDE 3 ####

# indicator = NY.GDP.PCAP.KD / name = GDP per capita (constant 2010 US$)
indicator <- c("GDP per capita" = 'NY.GDP.PCAP.KD')
dat1 <- WDI(indicator, country=c('FR', 'BR'), end = 2019)
head(dat1)

# indicators = NY.GDP.PCAP.KD and NY.GDP.PCAP.KN / names = GDP per capita (constant 2010 US$) and GDP per capita (constant LCU)
indicators <- c("GDP per capita (US$)" = 'NY.GDP.PCAP.KD', "GDP per capita (LCU)" = "NY.GDP.PCAP.KN")
dat2 <- WDI(indicators, country=c('FR', 'BR'), end = 2019)
head(dat2)

# SLIDE 4 ####

# GDP per capita for France and Brazil
ggplot(dat1, aes(year, `GDP per capita`, color=country)) + geom_line() +
  xlab('Year') + ylab('GDP per capita')

# SLIDE 5 ####

# GDP per capita (US$ and local currency unity) for France and Brazil
ggplot(dat2, aes(year, color=country)) + 
  geom_line(aes(year, `GDP per capita (US$)`)) +
  geom_line(aes(year, `GDP per capita (LCU)`), linetype = "dashed") +
  xlab('Year') + ylab('GDP per capita') +
  labs(caption = "GDP per capita (US$), solid; GDP per capita (LCU), dashed") +
  theme_economist() +
  scale_colour_economist()

# SLIDE 6 ####

Data_info <- WDI_data
Data_series <- as.data.frame(Data_info$series) %>%
  filter(indicator == "NY.GDP.PCAP.KD")
colnames(Data_series)
Data_series$description

# SLIDE 7 ####

Data_countries <- as.data.frame(Data_info$country) 
Data_countries %>%
  kable("html") %>%
  kable_styling(font_size = 11) %>%
  scroll_box(width = "100%", height = "60%")

# SLIDE 8 ####

# indicator = NY.GDP.PCAP.KD / name = GDP per capita (constant 2010 US$)
indicator <- c("GDP per capita" = 'IT.NET.USER.ZS')
datall <- WDI(indicator, country="all", end = 2019)
head(datall)

LATAM <- Data_info$country %>%
  data.frame() %>%
  filter(region == "Latin America & Caribbean") %>%
  select(country) %>%
  unlist()

datall %>%
  na.omit() %>%
  filter(country %in% LATAM) %>%
  ggplot(aes(year, `GDP per capita`)) + geom_line() +
  facet_wrap(vars(country), scales = "free_y")

