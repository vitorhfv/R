usethis::create_project ('C:\\Users\\PC\\Desktop\\Trabalhos\\Programacao_R_2_Inicial')
here::dr_here()

## Test 1

500/30
############################################
imdb <- read_delim(
  "https://raw.githubusercontent.com/curso-r/202010-r4ds-1/master/dados/imdb2.csv",
  ";",
  escape_double = FALSE,
  trim_ws = TRUE
)


imdb %>%
  drop_na(orcamento, receita) %>%
  mutate(lucro = receita - orcamento) %>%
  select(titulo, ano, diretor, lucro) %>%
  arrange(desc(lucro))

#########################################################

imdb_dir <- imdb %>%
  mutate(lucro = receita - orcamento) %>%
  group_by(diretor) %>%
  select(titulo, ano, lucro, diretor) %>%
  glimpse()

imdb_dir_lucrat <- imdb_dir %>%
  head(10)


  imdb_dir_lucrat %>%
  mutate(diretor = forcats::fct_reorder(diretor, media_lucro)) %>%
  ggplot() +
  geom_col(aes(x = diretor, y = media_lucro)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "cineastas", y = "lucro médio por filme") +
  coord_flip()


library(forcats)
###########################
windowsFonts(Coolveltica=windowsFont("Times New Roman"))
install.packages('extrafont')
library(extrafont)
font_import()
y


library(extrafont)

library(remotes)
remotes::install_version("Rttf2pt1", version = "1.3.8")
extrafont::font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()

##############################################
library(priceR)

adjust_for_inflation(100, 2005, country, to_date = 2017,
                     inflation_dataframe = inflation_dataframe,
                     countries_dataframe = countries_dataframe)
# Assign these variables once

#
exchange_rate_latest("USD") %>%
  head(10)

nominal_prices

show_countries()
set.seed(284)

nominal_prices <- 1+1
years <- round(rnorm(10, mean=2006, sd=5))

lucro <- imdb_lucro %>%
  select(lucro) %>%
  head(10)

ano <- imdb_lucro %>%
  select(ano) %>%
  head(10)

###
ano1<- ano %>%
  as.Date(as.yearmon(yrs))

lucro10 <- data.frame(ano, lucro)


install.packages("zoo")
library(zoo)
#####################
imdb_lucro2
#############

imdb_lucro2 <- as.data.frame(imdb_lucro)

df <- data.frame(years, nominal_prices)
df$in_2008_dollars <- adjust_for_inflation(nominal_prices, years, "US", to_date = 2008)
df

imdb_lucro$def_lucro <- adjust_for_inflation(lucro, ano, "US", to_date = 2020)

##########################
imdb_lucro

ano <- imdb_lucro %>%
  select(ano)


lucro <- imdb_lucro %>%
  select(lucro)

###############################################################################
imdb_3 <- imdb_lucro2 %>%
  select(titulo, ano, lucro) %>%
  head(10)
imdb_lucro

as.numeric(as.character(imdb_3$lucro))

imdb

imdb_3

imdb_lucro

imdb_lucro$lucro <- as.numeric(as.character(imdb_lucro$lucro))
imdb_lucro$ano <- as.numeric(as.character(imdb_lucro$ano))

imdb_lucro$ano <- as.Date(as.character(imdb_lucro$ano))

lucro$lucro <- as.numeric(as.character(lucro$lucro))
ano$ano <- as.Date.numeric (as.character(ano$ano))

years

#iris2 %>% pull(Species)

ano <- ano %>% pull (ano)
lucro <- lucro %>% pull (lucro)

imdb_lucro %>%
  arrange(desc(def_lucro))

#
imdb

imdb4 <- imdb %>%
  select(titulo, ano, receita)

imdb_receita
