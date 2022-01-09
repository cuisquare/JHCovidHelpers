## code to prepare `countries_population` dataset goes here
path <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
h <- xml2::read_html(path)
tab <- h %>% rvest::html_nodes("table")
countries_population <- tab[[1]] %>%
  rvest::html_table(fill=TRUE)
names(countries_population)[2] <- "Country_Region"
countries_population <- countries_population %>%
  #rename(Country_Region = "Country or dependent territory") %>%
  rename(Population= "Population") %>%
  select(Country_Region,Population)

countries_population <- countries_population %>%
  mutate(Population = as.numeric(str_replace_all(Population,",",""))) %>%
  mutate(Country_Region = str_replace_all(Country_Region,"\\[.+\\]","")) %>%
  mutate(Country_Region = str_replace_all(Country_Region,"\\u2020","")) %>% #"†" #"\\u2020"
  mutate(Country_Region = str_replace_all(Country_Region,"\\(more\\)","")) %>%
  mutate(Country_Region = str_trim(Country_Region))

countries_population <- countries_population %>%
  mutate(Country_Region = str_replace_all(Country_Region,"Ivory Coast", "Cote d'Ivoire"))
countries_population <- countries_population %>%
  mutate(Country_Region = str_replace_all(Country_Region,"Cape Verde", "Cabo Verde"))

#replacement of São Tomé and Príncipe with check before and after change:
#Sao_Name_Before <- countries_population %>% filter(Population > 205000 & Population < 220000) %>% pull(Country_Region)
#print(paste("***CHECK*** Before replacement, Sao name in coutries_population df (found based on population) is: ",Sao_Name_Before,sep=""))
countries_population <- countries_population %>%
  mutate(Country_Region = str_replace_all(Country_Region,"S\\u00e3o Tom\\u00e9 and Pr\\u00edncipe", "Sao Tome and Principe")) #São Tomé and Príncipe stringi::stri_escape_unicode("São Tomé and Príncipe")
#Sao_Name_After <- countries_population %>% filter(Population > 205000 & Population < 220000) %>% pull(Country_Region)
#print(paste("***CHECK*** After replacement, Sao name in coutries_population df (found based on population) is: ",Sao_Name_After,sep=""))

countries_population <- countries_population %>%
  mutate(Country_Region = str_replace_all(Country_Region,"DR Congo", "Congo (Kinshasa)"))
countries_population <- countries_population %>%
  mutate(Country_Region = str_replace_all(Country_Region,"Congo$", "Congo (Brazzaville)"))
countries_population <- countries_population %>%
  mutate(Country_Region = str_replace_all(Country_Region,"Myanmar", "Burma"))
countries_population <- countries_population %>%
  mutate(Country_Region = str_replace_all(Country_Region,"East Timor", "Timor-Leste"))
countries_population <- countries_population %>%
  add_row(Country_Region = "West Bank and Gaza", Population = 4543126)

#cote d'ivoire not found was ivory coast on wiki DONE
#cabo verde not found was cape verde on wiki DONE
#Taiwan* not found was Taiwan on wiki DONE
#Holy See not found was Vatican City on wiki DONE
#US not found was United States on wiki DONE
#Czechia not found was Czech Republic DONE
#Korea, South not found was South Korea DONE
#Sao Tome and Principe not found was São Tomé and Príncipe DONE
#Congo (Brazzaville) not found was Republic of the Congo # https://en.wikipedia.org/wiki/Republic_of_the_Congo DONE
#Congo (Kinshasa) not found was #https://en.wikipedia.org/wiki/Democratic_Republic_of_the_Congo DONE
#Burma not found  was Myanmar on wiki DONE
#Timor-Leste not found was East Timor on wiki DONE
#West Bank and Gaza not found DONE STATIC
# https://en.wikipedia.org/wiki/Palestinian_territories
#https://en.wikipedia.org/wiki/Demographics_of_the_Palestinian_territories
#MS Zaandam not found is a cruise ship - will be dropped by inner_join
#Diamond Princess not found is a cruise ship - will be dropped by inner_join

usethis::use_data(countries_population, overwrite = TRUE)
