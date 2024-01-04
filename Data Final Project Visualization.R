install.packages("ggplot2")
install.packages("gt")
install.packages("tidyverse")
install.packages("stargazer")
install.packages("ggtext")
library(ggtext)
library(tidyverse)
library(readxl)
library(gt)
library(ggplot2)
library(stargazer)

#may need to change the file path if you downloaded this code
file_path <- file.path("C:/Users/aidan/Documents/Data Final Project")
setwd(file_path)

#reading in data and adjusting source to match Nationality
data <- read_xlsx("international_billboard.xlsx")

data2 <- data |> 
  mutate(source = case_when(
      source == "ARG"~"AR",
      source == "AUS"~"AU",
      source == "CAN"~"CA",
      source == "ESP"~"ES",
      source == "EUR"~"EUROPE",
      source == "FRA"~"FR",
      source == "GBP"~"GB",
      source == "GER"~"DE", 
      source == "ITA"~"IT", 
      source == "JPN"~"JP", 
      source == "KOR"~"KR", 
      source == "MEX"~"MX", 
      source == "SUI"~"CH"
  ))

#data used for the map

data2 <- data2 |> group_by(TrackName, Date, source) |>
  mutate(number_of_artists = n()) |>
  ungroup()

data3 <- data2 |>
  mutate(foreign = (source != Nationality)) |>
  filter(source != "EUROPE")
  
#creating linear regressions

# "Are songs produced in a country more popular in that country?"
model1 <- lm(Rank ~ foreign, data = data3, weights = 1 / number_of_artists)

summary(model1)

stargazer(model1, type = "html", digits = 2, title = "Linear Regression of Billboard Rank on International Success of Foreign Artists",
  dep.var.labels=c("Billboard Rank"),
  covariate.labels=c("Foreign Artist (Binary)", "Intercept")
  , out = 'linRegOnForeign.htm')

# "Do countries with a shared language exert greater influence on one another?"

#Spanish Speaking NO LONGER BEING USED IN FINAL PROJECT
data4 <- data2 |>
  filter(source %in% c("AR", "ES", "MX")) |>
  mutate(speaks_spanish = (Nationality %in% c("AR", "ES", "MX", "PR", "PA", "CO", "DO", "VE")))
  
data4 |> select(Nationality, speaks_spanish, source)

model2 <- lm(Rank ~ speaks_spanish, data = data4, weights = 1 / number_of_artists)

model2

stargazer(model2, type = "html", digits = 2, title = "Linear Regression of Billboard Rank on Shared Language between Artists and Country (Spanish)",
          dep.var.labels=c("Billboard Rank"),
          covariate.labels=c("Spanish-Speaking Artist (Binary)", "Intercept")
          , out = 'linRegOnSpanish.htm')

#English Speaking But Not American
data5 <- data3 |>
  mutate(
    speaks_english_not_am = (Nationality %in% c("CA", "GB", "PR", "JM", "GY", "NZ", "AU", "ZA", "NG", "UG")),
    )

model3 <- lm(Rank ~ speaks_english_not_am, data = data5, weights = 1 / number_of_artists)

summary(model3)

stargazer(model3, type = "html", digits = 2, title = "Linear Regression of Billboard Rank on Shared Language between Artists and Country (English, Excluding US)",
          dep.var.labels=c("Billboard Rank"),
          covariate.labels=c("English-Speaking Artist (Binary)", "Intercept")
          , out = 'linRegOnEnglish.htm')

#How much influence do Asian countries have on European music? NOT BEING USED IN FINAL

data6 <- data2 |>
  filter(source %in% c("ES", "GB", "FR", "BE", "DE", "RO", "CH", "IT", "NL")) |>
  mutate(asian = Nationality %in% c("PH", "BD", "JP", "KR"))
  
model4 <- lm(Rank ~ asian, data = data6, weights = 1 / number_of_artists)
  
stargazer(model4, type = "html", digits = 2, title = "Linear Regression on Asianness to European Billboard Rank",
          dep.var.labels=c("Billboard Rank"),
          covariate.labels=c("Asian Artist (Binary)", "Intercept")
          , out = 'linRegOnAsian.htm')

# "Are songs produced in America more popular?"
data7 <- data2 |>
  filter(source != "EUROPE") |>
  mutate(american = Nationality == "US")

model5 <- lm(Rank ~ american, data = data7, weights = 1 / number_of_artists) 

model5

stargazer(model5, type = "html", digits = 2, title = "Linear Regression of Billboard Rank on International Success of American Artists",
          dep.var.labels=c("Billboard Rank"),
          covariate.labels=c("American Artist (Binary)", "Intercept")
          , out = 'linRegOnAmerican.htm')

#stacked bar plots for rank proportions

country_codes <- c("AR", "PR", "US", "CA", "PA", "ES", "CO", "GB", "AU", "JM", 
                   "GY", "NZ", "BR", "DO", "VE", "MX", "DE", "PH", "BD", "ZA", 
                   "NG", "FR", "BE", "DZ", "UG", "TR", "JP", "RO", "ZW", "CH", 
                   "IT", "NL", "IN", "KR", "NO", "SE")

country_names <- c("Argentina", "Puerto Rico", "United States", "Canada", "Panama", 
                   "Spain", "Colombia", "United Kingdom", "Australia", "Jamaica", 
                   "Guyana", "New Zealand", "Brazil", "Dominican Republic", 
                   "Venezuela", "Mexico", "Germany", "Philippines", "Bangladesh", 
                   "South Africa", "Nigeria", "France", "Belgium", "Algeria", 
                   "Uganda", "Turkey", "Japan", "Romania", "Zimbabwe", "Switzerland", 
                   "Italy", "Netherlands", "India", "South Korea", "Norway", "Sweden")

country_dict <- setNames(country_names, country_codes)


data8 <- data3 |> mutate(
  Rank = 100 - Rank,
  source = country_dict[source],
  Nationality = country_dict[source]
)
  


ggplot(data8, aes(fill = foreign, y = Rank, x = reorder(source, foreign*Rank))) + 
  geom_bar(position="fill", stat="identity") +
  labs(
    title = "Proportion of Foreign Artists in Country's Top 100 Billboards",
    subtitle = "Rank-Adjusted",
    x = "", y = "Percent of Billboard"
  ) + scale_fill_discrete(name = "Foreign Artist") +
  scale_fill_manual(values = c('pink', '#748E54')) + theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  

ggsave("rank-adjusted-histogram.png")

data9 <- data3 |> mutate(
  Rank = 1,
  source = country_dict[source],
  Nationality = country_dict[source]
)

ggplot(data9, aes(fill = foreign, y = Rank, x = reorder(source, foreign))) + 
  geom_bar(position="fill", stat="identity") +
  labs(
    title = "Proportion of Foreign Artists in Country's Top 100 Billboards",
    subtitle = "Not Rank-Adjusted",
    x = "", y = "Percent of Billboard"
  ) + scale_fill_discrete(name = "Foreign Artist") +
  scale_fill_manual(values = c('pink', '#748E54')) + theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("not-rank-adjusted-histogram.png")

#creating summary statistic table

data10 <- data3 |> mutate(
  source = country_dict[source],
  Nationality = country_dict[source]
)

table <- data10 |>
  group_by(source) |>
  summarize(
    NumberOfArtists = n(),
    NumberOfForeignArtists = sum(foreign),
    foreignProp = round(NumberOfForeignArtists / NumberOfArtists, 3)
  ) |> arrange(desc(foreignProp))

table |> gt() |>
tab_header(
  title = md("**Summary Statistics for Top 100 Billboards**"),
  subtitle = md("*Countries ordered in descending foreign representation*")
) |>   cols_label(
    source = md("**Country**"),
    NumberOfArtists = md("**# of Artists**"),
    NumberOfForeignArtists = md("**# of Foreign Artists**"),
    foreignProp = md("**Ratio**")
  ) |> gtsave("sum_stats.png")

#### code for creating lollipop chart

data3 |>
  filter(foreign) |>
  group_by(source) |>
  mutate(foreign_avg = mean(Rank)) |> distinct(foreign_avg)

data3 |>
  filter(!foreign) |>
  group_by(source) |>
  mutate(foreign_avg = mean(Rank)) |> distinct(foreign_avg)

data11 <- data3 |>
  mutate(
    source = country_dict[source],
    Nationality = country_dict[source]
  ) |>
  group_by(source) |>
  mutate(
    rank_avg = mean(Rank),
    total_artists = n()) |>
  ungroup() |>
  filter(foreign) |>
  group_by(source) |>
  mutate(
    foreign_avg = mean(Rank),
    foreign_artists = n(),
    national_avg = ((rank_avg * total_artists) - (foreign_avg * foreign_artists)) / (total_artists - foreign_artists)
  ) |>
  ungroup() |>
  arrange(foreign_avg)

data11 |> distinct(foreign_avg)

ggplot(data11 |> mutate(source = fct_reorder(source, desc(national_avg)))) +
  geom_segment( aes(x=source, xend=source, y=national_avg, yend=foreign_avg), color="grey") +
  geom_point( aes(x=source, y=national_avg), color="pink", size=3) +
  geom_point( aes(x=source, y=foreign_avg), color="lightgreen", size=3) +
  coord_flip() +
  labs(
    title = "Average <span style='color: green;'>Foreign</span> and <span style='color: pink;'>National</span> Billboard Rank",
    x = "", y = "Average Billboard Rank"
  ) + theme(plot.title = element_markdown(hjust = 0.5,face="bold", size = 17))
  
ggsave("avgs_lollipop.png")

data10 |> filter(source == "Japan") |> distinct(Date)

