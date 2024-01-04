#required packages
install.packages("tidyverse")
install.packages("jsonlite")
install.packages("writexl")
library(tidyverse)
library(jsonlite)
library(httr2)
library(stringr)
library(writexl)

#may need to change the file path if you downloaded this code
file_path <- file.path("C:/Users/aidan/Documents/Data Final Project")
setwd(file_path)
data_list = list.files("data", pattern = "\\.csv$", full.names = T)

#turns file names (country) into column along side data
read_country_file <- function(file_path){
  df <- read_csv(file_path) |>
    mutate(
      source = str_extract(
        string = file_path,
        pattern = "(?<=data/).*(?=\\.csv$)")
    )
  return(df)
}

countries <- purrr::map(.x = data_list, .f = read_country_file)
var_names <- purrr::map(.x = countries, .f = names)

shortest_country <- var_names[1]

for (x in var_names){
  if (length(x) < length(shortest_country)){
    shortest_country = x
  }
}

#only keeps columns shared by all tables

shared_columns <- purrr::map2(var_names, shortest_country, intersect)

shared_columns <- Reduce(intersect, shared_columns)

clean_country_file <- function(df){
  df_clean <- df |>
    select(all_of(shared_columns))
  
  return(df_clean)
}

cleaned_dfs <- purrr::map(.x = countries, .f = clean_country_file)

clean_df <- bind_rows(cleaned_dfs)

#list of artists who do not follow delimiter patterns, or have typos

exception_list <- c('Jawsh 685 Jason Derulo','IU (Prod.& Feat. SUGA of BTS)',
                    'MC MONG (Feat. Song Ga-in Chancellor)',"AKMU.How Can I Love The Heartbreak You're The One I Love",
                    'King & Prince', 'Benji & Fede', 'Tones And I', 'Lil Nas X Featuring Billy Ray Cyrus',
                    'Jesse & Joy And J Balvin', 'Lim Jae Hyun (prod. 2soo)', "Jang Hye Jin Yun Min Soo (VIBE)")

#common good delimiters

subfilter1 <- clean_df |>
  filter(!(clean_df$Artist %in% exception_list)) |>
    separate_longer_delim(Artist, " Featuring ") |>
    separate_longer_delim(Artist, " & ") |>
    separate_longer_delim(Artist, " X ") |>
    separate_longer_delim(Artist, " x ") |>
    separate_longer_delim(Artist, ", ") |>
    separate_longer_delim(Artist, "  ") |>
    separate_longer_delim(Artist, " And ") |>
    separate_longer_delim(Artist, " Feat. ") |>
    separate_longer_delim(Artist, " / ") |>
    separate_longer_delim(Artist, " + ")

subfilter1 |> distinct(Artist)

#all the annoying edge cases

subfilter2 <- clean_df |>
  filter(Artist %in% exception_list) |>
  filter(!(Artist %in% c('Jawsh 685 Jason Derulo','Jesse & Joy And J Balvin','MC MONG (Feat. Song Ga-in Chancellor)', "Jang Hye Jin Yun Min Soo (VIBE)"))) |>
  separate_longer_delim(Artist, " Featuring ") |>
  separate_longer_delim(Artist, "Feat. ") |>
  mutate(Artist = str_remove(Artist, " (\\(prod. 2soo\\))|(\\.How Can I Love The Heartbreak You're The One I Love)|( \\(Prod\\.&)|(\\))|( \\()"))

subfilter2 |> distinct(Artist)

subfilter3 <- clean_df |>
  filter(Artist == "Jawsh 685 Jason Derulo") |>
  mutate(Artist = str_remove(Artist, " Jason Derulo"))

subfilter3 |> distinct(Artist)

subfilter4 <- clean_df |>
  filter(Artist == "Jawsh 685 Jason Derulo") |>
  mutate(Artist = str_remove(Artist, "Jawsh 685 "))

subfilter5 <- clean_df |>
  filter(Artist == "Jesse & Joy And J Balvin") |>
  separate_longer_delim(Artist, " And ")

subfilter6 <- clean_df |>
  filter(Artist == 'MC MONG (Feat. Song Ga-in Chancellor)') |>
  mutate(Artist = str_remove(Artist, " \\(Feat\\. Song Ga-in Chancellor\\)"))

subfilter6 |> distinct(Artist)

subfilter7 <- clean_df |>
  filter(Artist == 'MC MONG (Feat. Song Ga-in Chancellor)') |>
  mutate(Artist = str_extract(Artist, "Song Ga-in"))

subfilter7 |> distinct(Artist)

subfilter8 <- clean_df |>
  filter(Artist == 'MC MONG (Feat. Song Ga-in Chancellor)') |>
  mutate(Artist = str_extract(Artist, "Chancellor"))

subfilter8 |> distinct(Artist)

subfilter9 <- clean_df |>
  filter(Artist == "Jang Hye Jin Yun Min Soo (VIBE)") |>
  mutate(Artist = str_extract(Artist, "Jang Hye Jin"))

subfilter10 <- clean_df |>
  filter(Artist == "Jang Hye Jin Yun Min Soo (VIBE)") |>
  mutate(Artist = str_extract(Artist, "Yun Min Soo"))

#joining all the sub filters together

cleaner_df <- bind_rows(subfilter1, subfilter2, subfilter3, subfilter4, subfilter5, subfilter6, subfilter7, subfilter8, subfilter9, subfilter10)

#makes api call with artist name, sys.sleep 1 because that is what the API requires
#returns two letter country code associated with artist

get_origin <- function(artistName){
  
  req <- request("https://musicbrainz.org/ws/2/artist/?")
  Sys.sleep(1)
  
  req <- req |>
    req_url_query(
      "query"  = artistName,
      "fmt" = "json"
    )
  
  resp <- req_perform(req)
  
  df <- resp |> resp_body_json(simplifyVector = TRUE)
  
  retVal <- tibble(Artist = artistName, Nationality = df$artists$country[1])
  
  return(retVal)
}

#list of all artists

unique_artist_list <- (cleaner_df |> distinct(Artist))$Artist

#repeated call of get_origin on all artists

nationality_list <- purrr::map(.x = unique_artist_list, .f = get_origin, .progress = T)
nationality_df <- nationality_list |> bind_rows()

#manual input of two-letter country code for 35 artists that had errors

manual1 <- tribble(~Artist, ~Nationality,
        "ROSALIA", "ES",
        "Tatool", "AR",
        "The Bugler", "JM",                          
        "The Beatles", "GB",                         
        "Jack Johnson", "US",                       
        "Orquesta Filarmonica de Gran Canaria", "ES",
        "RESISTIRE MEXICO", "MX",                 
        "Depedro", "ES",                           
        "Ismel Serriano", "ES",                      
        "Nina de Juan", "ES",                        
        "Santi Blames", "ES",
        "Agoney", "ES",
        "The Harmony Group","US",
        "Calvin Harris", "GB",
        "Rag'n'Bone Man","GB",
        "Live Lounge Allstars","GB",
        "Et demain ? Le Collectif","FR",
        "Joel Corry MNEK","UG",
        "Eno", "TR",
        "Kalazh44","DE",
        "Joker Bra","DE",
        "Rymez","ZW",
        "Loredana Zuna","CH",
        "Takagi","JP"
        )

manual2 <- tibble(
  Artist = c("Johnny's WEST", "Kazuya Kamenashi", "Twenty Twenty", "Jang Hye Jin", "Yun Min Soo", "Paul Kim", "Cho Jung Seok", "BLOO", "Horacio Palencia", "Conkarah", "Song Ga-in", "Jesse & Joy"),
  Nationality = c("JP", "JP", "GB", "KR", "KR","KR", "KR", "KR", "MX", "JM", "KR", "MX")
)

#binds the initial list (except NAs) with manual inputs
ultimate_nationality <- bind_rows(nationality_df |> filter(!is.na(Nationality)), manual1, manual2)

#joins the tables to get final output, renames rank column
cleanest_df <- cleaner_df |> left_join(ultimate_nationality, join_by(Artist == Artist))
cleanest_df <- cleanest_df |> rename(Rank = 1)
glimpse(cleanest_df)

write_xlsx(cleanest_df, path = file.path(file_path, "data", "international_billboard.xlsx"))





