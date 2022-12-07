#data(sysdata, envir=environment())


##Data extraction ####


#' @title getRawDataFromWeb
#'
#' @param destfolder destination folder for downloaded files.
#'
#' @return master_data data frame containing deaths, confirmed, recovered and
#' vaccine data from John Hopkins, without change to regional data
#'
#' @export
#'
getRawDataFromWeb <- function(destfolder = tempdir()) {
  data_file_deaths <- "time_series_covid19_deaths_global.csv"
  data_file_recovered <- "time_series_covid19_recovered_global.csv"
  data_file_confirmed <- "time_series_covid19_confirmed_global.csv"
  data_file_vaccines <- "time_series_covid19_vaccine_global.csv"
  #datasource_choice <- "DirectDownload" #"GithubPull" #""LocalDrectory"
  #getting death and cases data
  remote_url_root <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
  file_list_cases_death <- c(data_file_deaths,data_file_recovered,data_file_confirmed)
  for (filename in file_list_cases_death) {
    destfile <- paste0(destfolder,"\\",filename)
    source_url <- paste0(remote_url_root,"/",filename)
    print(paste("source_url =",source_url))
    print(paste("destfile =",destfile))
    try(
      download.file(
        url = source_url,
        destfile = destfile
      )
    )
  }
  #getting vacine data
  data_folder_vaccines <- destfolder
  remote_url_root_vaccines <- "https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/"
  destfile_vaccine <- paste0(destfolder,"\\",data_file_vaccines)
  source_url_vaccine <- paste0(remote_url_root_vaccines,"/",data_file_vaccines)
  print(paste("source_url_vaccine =",source_url_vaccine))
  print(paste("destfile_vaccine =",destfile_vaccine))
  try(
    download.file(
      url = source_url_vaccine,
      destfile = destfile_vaccine
    )
  )
  #TODO write
  #data_folder_vaccine <- "./Data/JH_Data_GitHubClone/csse_covid_19_data/csse_covid_19_time_series/"


  ##Deaths
  master_data_raw_deaths <- readr::read_csv(paste0(c(destfolder,data_file_deaths),collapse ="\\"))

  #TODO: ? get data with old metric for death in UK using this link:
  #https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/909660/COVID-19_Death_Series_20200816.xlsx

  #change into tidy data
  last_day_col <- ncol(master_data_raw_deaths)
  master_data_deaths <- master_data_raw_deaths %>%
    tidyr::gather(key="Date",value="Deaths",5:last_day_col)

  #reformat dates and long char column names
  master_data_deaths <- master_data_deaths  %>%
    mutate(Date = lubridate::mdy(Date))  %>%
    dplyr::rename(Country_Region = `Country/Region`, Province_State = `Province/State`)

  ##Confirmed Cases
  master_data_raw_confirmed <- readr::read_csv(paste0(c(destfolder,data_file_confirmed),collapse ="\\"))

  #change into tidy data
  master_data_confirmed <- master_data_raw_confirmed %>%
    select(-Lat,-Long)
  last_day_col <- ncol(master_data_confirmed)
  master_data_confirmed <- master_data_confirmed %>%
    tidyr::gather(key="Date",value="Confirmed",3:last_day_col)

  #reformat dates and long char column names
  master_data_confirmed <- master_data_confirmed %>%
    mutate(Date = lubridate::mdy(Date))  %>%
    dplyr::rename(Country_Region = `Country/Region`, Province_State = `Province/State`)

  ##Recovered Cases
  master_data_raw_recovered <- readr::read_csv(paste0(c(destfolder,data_file_recovered),collapse ="\\"))

  #change into tidy data
  master_data_recovered <- master_data_raw_recovered %>%
    select(-Lat,-Long)
  last_day_col <- ncol(master_data_recovered)
  master_data_recovered <- master_data_recovered %>%
    tidyr::gather(key="Date",value="Recovered",3:last_day_col)

  #reformat dates and long char column names
  master_data_recovered <- master_data_recovered %>%
    mutate(Date = lubridate::mdy(Date))  %>%
    dplyr::rename(Country_Region = `Country/Region`, Province_State = `Province/State`)

  ##vaccine data
  try(
    master_data_raw_vaccine <-  readr::read_csv(paste0(c(destfolder,data_file_vaccines),collapse ="\\"))
  )
  if (exists("master_data_raw_vaccine")) {
    master_data_vaccine  <- master_data_raw_vaccine
  }

  #View(master_data_raw_vaccine) #i think that's the one i want

  #Merging data
  master_data <- master_data_deaths
  master_data <- left_join(master_data,master_data_confirmed)
  master_data <- left_join(master_data,master_data_recovered)
  if (exists("master_data_vaccine")) {
    master_data <- left_join(master_data,master_data_vaccine)
  }
  return(master_data)
}

#' Gets data from web
#'
#' @param destfolder destination folder for downloaded files.
#'
#' @return master_data data frame containing deaths, confirmed, recovered and
#' vaccine data from John Hopkins with missing country wide data added
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @import ggplot2
#' @importFrom utils download.file
#' @importFrom utils tail
#' @importFrom zoo rollapply
#' @importFrom rlang :=
#'
#' @export
#'
#' @examples #TODO
getDataFromWeb <- function(destfolder = tempdir()) {
  master_data <- getRawDataFromWeb (destfolder)

  master_data <- master_data %>%
    addMissingCountryWideData()

  return(master_data)
}

#' Add Population Data
#'
#' @param master_data data frame containing JH compatible countries
#'
#' @return data frame with population information for each contry
#'
#' @importFrom stringr str_replace_all
#' @importFrom  stringr str_trim
#'
#' @export
#'
#' @examples #TODO
addPopulationData <- function(master_data) {
  #TODO: add country density
  #https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population_density

  #TODO: add urbanisation level

  #countries_population is part of the data of the package

  #checking data not found
  JHCountries_NotFound_Before <- master_data %>%
    select(Country_Region) %>%
    unique() %>%
    filter(!(Country_Region %in% countries_population$Country_Region))

  #correcting the names for not found data
  master_data <- master_data %>%
    mutate(Country_Region = str_replace_all(Country_Region,"Taiwan\\*", "Taiwan"))
  master_data <- master_data %>%
    mutate(Country_Region = str_replace_all(Country_Region,"Holy See", "Vatican City"))
  master_data <- master_data %>%
    mutate(Country_Region = str_replace_all(Country_Region,"^US$", "United States"))
  master_data <- master_data %>%
    mutate(Country_Region = str_replace_all(Country_Region,"Czechia", "Czech Republic"))
  master_data <- master_data %>%
    mutate(Country_Region = str_replace_all(Country_Region,"Korea, South", "South Korea"))

  #checking data not found after correction
  JHCountries_NotFound_After <- master_data %>%
    select(Country_Region) %>%
    unique() %>%
    filter(!(Country_Region %in% countries_population$Country_Region))

  Nb_JHCountries_NotFound_After <- nrow(JHCountries_NotFound_After) #expected: 2

  print(paste("After population data extraction, ",
              Nb_JHCountries_NotFound_After,
              " countries in master_data were not found ",
              "in countries_population (and will be dropped):",sep=""))
  print(JHCountries_NotFound_After$Country_Region)

  #save data for future run


  #final join, will leave out anything not found
  master_data_try2 <- master_data %>%
    inner_join(countries_population)

  master_data <- master_data_try2

  Nb_JHCountriesTotal_After <- master_data %>%  select(Country_Region) %>% unique() %>% nrow() #186
  return(master_data)
}


#' Add missing country wide data
#'
#' @param JH_Data dataframe containing expected John Hopkins data
#'
#' @return dataframe containing expected John Hopkins data with country wide
#' data added for countries that have only regional data - currently Canada,
#' Australia and China
#'
#' @examples #TODO
addMissingCountryWideData <- function(JH_Data) {
  regional_only <- JH_Data %>%
    select(Country_Region,Province_State) %>%
    unique() %>%
    mutate(isRegional = !is.na(Province_State)) %>%
    group_by(Country_Region,isRegional) %>%
    summarise(count = n()) %>%
    group_by(Country_Region) %>%
    mutate(hasRegional = (sum(isRegional)>0)) %>%
    mutate(isRegionalOnly =(hasRegional & (n()==1))) %>%
    filter(isRegionalOnly) %>%
    select(Country_Region) %>%
    unique()

  curr_names <- c("Province_State", "Country_Region", "Lat", "Long", "Date",
                  "Deaths", "Confirmed", "Recovered", "UID", "Doses_admin", "People_at_least_one_dose")

  non_grouping_names <- curr_names[!curr_names %in% c("Country_Region", "Date")]

  summarise_names <- c("Deaths", "Confirmed", "Recovered", "Doses_admin", "People_at_least_one_dose")
  #as checked on 5/12/2022

  # curr_names <- c("Province_State", "Country_Region", "Lat", "Long",
  #                 "Deaths", "Confirmed", "Recovered", "Doses_admin", "People_partially_vaccinated",
  #                 "People_fully_vaccinated", "Report_Date_String",
  #                 "UID") #old version

  JH_Data_countrylevel_for_regionalonly <- JH_Data %>%
    filter(Country_Region %in% unique(regional_only$Country_Region)) %>%
    group_by(across(-non_grouping_names
      # -c("Province_State","Lat","Long", "Report_Date_String",
      #                  "UID", "Deaths", "Confirmed", "Recovered", "Doses_admin", "People_partially_vaccinated",
      #                  "People_fully_vaccinated")
                    )) %>%
    #group_by(c("Province_State", "Date")) %>%
    summarise_at(.vars = summarise_names, #c("Deaths", "Confirmed", "Recovered", "Doses_admin", "People_partially_vaccinated","People_fully_vaccinated"),
                 .funs = function(x) {sum(x,na.rm = TRUE)}
    )

  JH_Data <- JH_Data %>%
    bind_rows(JH_Data_countrylevel_for_regionalonly)

  return(JH_Data)
}

#' Add calculated variables
#'
#' @param master_data JH compatiable dataframe
#'
#' @return dataframe with calculated variable added
#'
#' @export
#' @examples #TODO
addCalculatedVariables <- function(master_data) {
  #added data
  lagvaluedays <- 7

  scaling_death <- 10^6
  scaling_confirmed <- 10^4

  master_data <- master_data %>%
    group_by(Country_Region,Province_State) %>%
    mutate(Increase_Deaths = Deaths - lag(Deaths,1)) %>%
    mutate(Increase_Deaths_Avg = rollapply(data=Increase_Deaths,FUN=mean,width=lagvaluedays,fill=NA,align="right")) %>%
    mutate(Increase_Deaths_Avg_Avg = rollapply(data=Increase_Deaths_Avg,FUN=mean,width=lagvaluedays,fill=NA,align="right")) %>%
    mutate(Increase_Confirmed = Confirmed - lag(Confirmed,1)) %>%
    mutate(Increase_Confirmed_Avg = rollapply(data=Increase_Confirmed,FUN=mean,width=lagvaluedays,fill=NA,align="right")) %>%
    mutate(Increase_Confirmed_Avg_Avg = rollapply(data=Increase_Confirmed_Avg,FUN=mean,width=lagvaluedays,fill=NA,align="right")) %>%
    mutate(Weighted_Deaths = scaling_death*Deaths/Population) %>%
    mutate(Weighted_Confirmed = scaling_confirmed*Confirmed/Population) %>%

    mutate(Increase_Weighted_Deaths = (Weighted_Deaths - lag(Weighted_Deaths,lagvaluedays))/lagvaluedays) %>%
    mutate(Increase_Weighted_Deaths = Weighted_Deaths - lag(Weighted_Deaths,1)) %>%
    mutate(Increase_Weighted_Deaths_Avg = rollapply(data=Increase_Weighted_Deaths,FUN=mean,width=lagvaluedays,fill=NA,align="right")) %>%
    mutate(Increase_Weighted_Deaths_Avg_Avg = rollapply(data=Increase_Weighted_Deaths_Avg,FUN=mean,width=lagvaluedays,fill=NA,align="right")) %>%

    mutate(Increase_Weighted_Confirmed = (Weighted_Confirmed - lag(Weighted_Confirmed,lagvaluedays))/lagvaluedays) %>%
    mutate(Increase_Weighted_Confirmed = Weighted_Confirmed - lag(Weighted_Confirmed,1)) %>%
    mutate(Increase_Weighted_Confirmed_Avg = rollapply(data=Increase_Weighted_Confirmed,FUN=mean,width=lagvaluedays,fill=NA,align="right")) %>%
    mutate(Increase_Weighted_Confirmed_Avg_Avg = rollapply(data=Increase_Weighted_Confirmed_Avg,FUN=mean,width=lagvaluedays,fill=NA,align="right")) %>%
    mutate(People_at_least_one_dose_Perc = 100*People_at_least_one_dose/Population) %>%
    ungroup()

  #new data not yet confirmed if useful
  #TODO make those averaged out over a week like rest of data
  master_data <- master_data %>%
    group_by(Country_Region, Province_State) %>%
    mutate(Increase_Increase_Confirmed = (Increase_Weighted_Confirmed - lag(Increase_Weighted_Confirmed,lagvaluedays))/lagvaluedays) %>%
    mutate(Increase_Increase_Deaths = (Increase_Weighted_Deaths - lag(Increase_Weighted_Deaths,lagvaluedays))/lagvaluedays) %>%
    mutate(Rate_Increase_Deaths = Increase_Increase_Deaths/Increase_Deaths) %>%
    mutate(Rate_Increase_Confirmed = Increase_Increase_Confirmed/Increase_Confirmed) %>%
    ungroup()

  return(master_data)
}

##Data Manipulation ####

#' Get Data at Country Level
#'
#' @param JH_Data data frame containing JH Data
#' @param CountryList character list of countries
#' @param VarName variable in JH_Data to be extracted
#'
#' @return ggplot plot
#' @export
#'
#' @examples #TODO
JHGetdata_CountryLevel <- function(JH_Data,CountryList,VarName) {
  # TODO Bug ? this does not give right answer for countries including France
  # possibly because of the regional aspect for that country
  # 810 milions for actual value of 67 gives ratio of 12 which is the
  # number of france regions. this might be due to how the population is
  # joined to the original data, it might get added even for rows for which
  # the province is not NA.
  # initial attempt is to remove regional information unfortunately this
  # does not work for countries ike australia for which there is only regional
  # data available. therefore this needs to be refined

  thedata <- JH_Data %>%
    #filter(Date>lubridate::ymd("20200315")) %>%
    filter(Country_Region %in% CountryList)

  thedata <- thedata %>%
    filter(is.na(Province_State)) #added 20211228 to deal with pop count bug

  #remove duplication if it exists
  thedata_dupeinfo <- thedata %>%
    select(Country_Region,Date,!!as.name(VarName)) %>%
    mutate(dupe = duplicated(.))

  thedata <- thedata  %>%
    dplyr::bind_cols(thedata_dupeinfo %>% select(dupe)) %>%
    filter(!dupe) %>%
    select(-dupe)

  thedata <- thedata  %>%
    filter(!is.na(!!as.name(VarName))) %>%
    group_by(Country_Region,Date) %>%
    summarise(!!VarName := sum(!!as.name(VarName))) %>%
    ungroup()

  thedata <- thedata %>%
    mutate(Country_Region= forcats::fct_reorder(Country_Region,!!as.name(VarName),function(x) {-tail(x,1)}))

}

#' get latest available date in JH data
#'
#' @param JH_Data JHData format dataframe
#' @param CountryName character valid country name
#'
#' @return latest available date
#' @export
#'
#' @examples #TODO
getLatestAvailableDate <- function(JH_Data,CountryName) {
  JH_Data %>%
    filter(Country_Region == CountryName) %>%
    pull(Date) %>%
    max(na.rm = TRUE)
}

#' get latest available JH Data over a days span
#'
#' @param JH_Data JH formatted data source (raw or augmented)
#' @param CountryList character list of countries to be extracted
#' @param spanDays number of days to look back start from latest available date in JH_Data.
#' If unspecified, defaults to a span of one day (latest available date)
#'
#' @return dataframe with the latest available data as specified by parameters
#' @export
#'
#' @examples #TODO
getLatestData <- function(JH_Data,CountryList,spanDays=FALSE) {

  LatestAvailableDate <- lubridate::as_date(min(sapply(CountryList,
                                            FUN=function(CN) {
                                              lubridate::as_date(getLatestAvailableDate(JH_Data,CN))}),na.rm = TRUE))

  print(paste("LatestAvailableDate =",LatestAvailableDate))
  print(paste("spanDays =",spanDays))

  if (spanDays==FALSE | !(is.numeric(spanDays))) {
    StartDate <- LatestAvailableDate
    print("spanDays not assigned or not numeric, limiting to latest available date")
  } else {
    StartDate <- LatestAvailableDate - lubridate::days(spanDays-1)
  }
  print(paste("StartDate = ",StartDate))

  JH_Data %>%
    filter(Country_Region %in% CountryList) %>%
    filter(Date <= LatestAvailableDate & Date >= StartDate)
}

top_N_CountryLevel <- function(JH_Data,VarName,ntop,datetop) {
  thedata <- JH_Data %>%
    filter(Date==datetop) %>%
    filter(!is.na(!!as.name(VarName))) %>%
    group_by(Country_Region)

  thedata <-thedata %>%
    summarise(!!VarName := sum(!!as.name(VarName)))

  thedata <-thedata%>%
    ungroup() %>%
    slice_max(order_by = !!as.name(VarName),n=ntop)

  return(thedata)
}

bottom_N_CountryLevel <- function(JH_Data,VarName,nbottom,datebottom) {
  thedata <- JH_Data %>%
    filter(Date==datebottom) %>%
    filter(!is.na(!!as.name(VarName))) %>%
    group_by(Country_Region) %>%
    summarise(!!VarName := sum(!!as.name(VarName))) %>%
    ungroup() %>%
    slice_min(order_by = !!as.name(VarName),n=nbottom)
  return(thedata)
}


#' Get latest variation
#'
#' @param JH_Aug_Data JH formatted data source (raw or augmented)
#' @param CountryList character list of countries to be extracted
#' @param VarName character JHData compatible variable name
#'
#' @return dataframe
#' @export
#'
#' @examples #TODO
JHGetProgress <- function(JH_Aug_Data,CountryList,VarName) {
  pos_min <- function(x) {
    min(x[x>0])
  }
  Progress <- JH_Aug_Data %>%
    filter(Country_Region %in% CountryList & is.na(Province_State)) %>%
    filter(Date > lubridate::ymd("20200401")) %>%
    group_by(Country_Region,Population) %>%
    summarise_at(VarName,list(pos_min, max,mean))

  Progress <- Progress %>%
    dplyr::rename(
      !!as.name(paste("min_",VarName,sep="")) := !!as.name("fn1"),
      !!as.name(paste("max_",VarName,sep="")) := !!as.name("fn2"),
      !!as.name(paste("avg_",VarName,sep="")) := !!as.name("fn3")
      )

  LatestData <- JH_Aug_Data %>%
    filter(is.na(Province_State)) %>%
    getLatestData(CountryList) %>%
    select(Country_Region,!!as.name(VarName))

  LatestData <- LatestData %>%
    dplyr::rename(!!as.name(paste("current_",VarName,sep="")) := !!as.name(VarName))

  Progress <- Progress %>%
    left_join(LatestData,by="Country_Region")

  return (Progress)
}

JHDateYWasXWhen <- function(JH_Data,VarName,CountryX,CountryY,DateX,DateLookUpFrom) {
  XVarNameVal <- JH_Data %>%
    filter(Country_Region == CountryX) %>%
    filter(Date == DateX) %>%
    filter(is.na(Province_State)) %>%
    pull(VarName)

  DateYWasXWhen <- JH_Data %>%
    filter(Country_Region == CountryY & is.na(Province_State) & Date > DateLookUpFrom) %>%
    filter(!!as.name(VarName) < XVarNameVal) %>%
    slice_min(Date,1) %>%
    pull(Date)

  #at which date did France get as good as UK is now ?
  #TODO: find peak and search from there, or define up and downs search, or get all candidates
  #at moment only looking for the downward match from peak
}

JHDateXWillBeYWhen <- function(JH_Data,VarName,CountryX,CountryY,DateX,DateLookUpFrom) {
  #take a few values of countryX varname vals with corresponding dateX
  #get corresponding dateYs that countryY had same vals
  #get average time span dateX-DateY
  #TODO write

}

#Data Plotting ####

JHGetplot_ProvinceLevel <- function(JH_Data,CountryList,VarName) {
  theplot <- JH_Data %>%
    filter(Date>lubridate::ymd("20200315")) %>%
    filter(Country_Region %in% CountryList) %>%
    ggplot(aes_string("Date",VarName,color="Province_State")) +
    geom_point() + geom_line()

  return(theplot)
}

JHGetMultipledata_CountryLevel <- function(JH_Data,CountryList,VarNames) {
  output <- data.frame()
  for (VarName in VarNames) {
    NewData <- JHGetdata_CountryLevel(JH_Data,CountryList,VarName)
    if(nrow(output) == 0) {
      output <- NewData
    } else {
      output <- output %>% left_join(NewData)
    }

  }
  return(output)
}


#' Get facet_wrap ggplot plot of Multiple Variables for a list of Country
#'
#' @param JH_Data data frame containing JH Data
#' @param CountryList character list of countries
#' @param VarNames character list of variables in JH_Data to be extracted
#'
#' @return ggplot plot
#' @export
#'
#' @examples #TODO
JHGetplot_CountryLevel_MultipleVar <- function(JH_Data,CountryList,VarNames) {

  thedata <- JH_Data %>%
    JHGetMultipledata_CountryLevel(CountryList,VarNames) %>%
    tidyr::pivot_longer(
      cols = VarNames,
      names_to = "VarName",
      values_to = "Value"
    )

  theplot <- thedata  %>%
    ggplot(aes(Date,Value,color = Country_Region)) +
    geom_line() +
    facet_wrap(~VarName, ncol=1,strip.position = "bottom",scales = "free_y") +
    theme(axis.title.y=element_blank())

  return(theplot)
}

#' Get ggplot plot of Single Variables for a list of Country
#'
#' @param JH_Data data frame containing JH Data
#' @param CountryList character list of countries
#' @param VarName character variable name in JH_Data to be extracted
#' @param add_label boolean on whether to add a label to the curve
#' @param adjust_label boolean on whether to adjust the label
#' @param mindiffval minimum value difference assured in case of label adjustmment
#'
#' @return ggplot of the variable with colour by country
#' @export
#'
#' @examples #TODO
JHGetplot_CountryLevel <- function(JH_Data,CountryList,VarName,add_label = FALSE,adjust_label = FALSE, mindiffval = 5) {
  VarNameString <- VarName
  VarName <- sym(VarName)

  thedata <- JH_Data %>%
    JHGetdata_CountryLevel(CountryList,VarName)

  adjust_vals <- function(vals,mindiffval) {
    #assumes vals sorted in ascending order
    #but beware this might have been changed by a previous adjustment
    for (valindex in 1:(length(vals)-1)) {
      if (vals[valindex+1]- vals[valindex] < mindiffval) {
        vals[valindex+1] <- vals[valindex] + mindiffval
      }
    }
    return(vals)
  }

  thelabeldata <- thedata %>%
    filter(Date == max(Date)) %>%
    arrange(!!as.name(VarName))

  if (adjust_label) {
    thelabeldata$adjustedvarname <- adjust_vals(thelabeldata[[VarNameString]],mindiffval)
  } else {
    thelabeldata <- thelabeldata %>%
      mutate(adjustedvarname = !!VarName)
  }



  #TODO order the countries by order of value at the last available date

  #TODO do the nudging at the labeldata df level but that would require being able to access VarName data
  #on the fly and not being able to so far is the reason we are currently using aes_string


  theplot <- ggplot(data =thedata,
                    mapping = aes(Date,!!VarName,color=Country_Region)
  ) +
    geom_line()

  if (add_label) {
    theplot <- theplot +
      geom_text( data=thelabeldata,
                 mapping = aes(x = Date,
                               y = adjustedvarname, #!!VarName
                               label = paste0(Country_Region," (",round(!!as.name(VarName),2),")"),
                               colour = Country_Region
                 ),
                 check_overlap = TRUE,
                 nudge_x = 20#,
                 # nudge_y = rnorm(n=rep(1,length(thelabeldata[[VarNameString]])),
                 #                 mean = 0,
                 #                 sd = 0.2*thelabeldata[[VarNameString]]
                 #                 )

      )
  }
  return(theplot)
}






