library(plotly)
library(ggrepel)
library(directlabels)
library(zoo)

##Data extraction ####

getDataFromWeb <- function(datasource_choice = "DirectDownload") {
  data_file_deaths <- "time_series_covid19_deaths_global.csv"
  data_file_recovered <- "time_series_covid19_recovered_global.csv"
  data_file_confirmed <- "time_series_covid19_confirmed_global.csv"
  data_file_vaccines <- "time_series_covid19_vaccine_global.csv"
  #datasource_choice <- "DirectDownload" #"GithubPull" #""LocalDrectory"
  if (datasource_choice == "GithubPull") {
    #getting death and cases data
    system("UpdateData") #runs data update from github repo
    data_folder_casesdeaths <- "./Data/JH_Data_GitHubClone/csse_covid_19_data/csse_covid_19_time_series/"
    #getting vaccine data
    data_folder_vaccines <- "./Data"
    remote_url_root_vaccines <- "https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/"
    try(
      download.file(
        url = paste0(remote_url_root_vaccines,"/",data_file_vaccines),
        destfile = paste0(data_folder_vaccines,"/",data_file_vaccines)
      )
    )}
  if (datasource_choice == "DirectDownload") {
    #getting death and cases data
    data_folder_casesdeaths <- "./Data"
    remote_url_root <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
    file_list_cases_death <- c(data_file_deaths,data_file_recovered,data_file_confirmed)
    for (filename in file_list_cases_death) {
      try(
        download.file(
          url = paste0(remote_url_root,"/",filename),
          destfile = paste0(data_folder_casesdeaths,"/",filename)
        )
      )
    }
    #getting vacine data
    data_folder_vaccines <- "./Data"
    remote_url_root_vaccines <- "https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/"
    try(
      download.file(
        url = paste0(remote_url_root_vaccines,"/",data_file_vaccines),
        destfile = paste0(data_folder_vaccines,"/",data_file_vaccines)
      )
    )
  }
  #TODO write
  #data_folder_vaccine <- "./Data/JH_Data_GitHubClone/csse_covid_19_data/csse_covid_19_time_series/"


  ##Deaths
  master_data_raw_deaths <- read_csv(paste(data_folder_casesdeaths,data_file_deaths,sep="/"))

  #TODO: ? get data with old metric for death in UK using this link:
  #https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/909660/COVID-19_Death_Series_20200816.xlsx

  #change into tidy data
  last_day_col <- ncol(master_data_raw_deaths)
  master_data_deaths <- master_data_raw_deaths %>%
    gather(key="Date",value="Deaths",5:last_day_col)

  #reformat dates and long char column names
  master_data_deaths <- master_data_deaths  %>%
    mutate(Date = mdy(Date))  %>%
    rename(Country_Region = `Country/Region`, Province_State = `Province/State`)

  ##Confirmed Cases
  master_data_raw_confirmed <- read_csv(paste(data_folder_casesdeaths,data_file_confirmed,sep="/"))

  #change into tidy data
  master_data_confirmed <- master_data_raw_confirmed %>%
    select(-Lat,-Long)
  last_day_col <- ncol(master_data_confirmed)
  master_data_confirmed <- master_data_confirmed %>%
    gather(key="Date",value="Confirmed",3:last_day_col)

  #reformat dates and long char column names
  master_data_confirmed <- master_data_confirmed %>%
    mutate(Date = mdy(Date))  %>%
    rename(Country_Region = `Country/Region`, Province_State = `Province/State`)

  ##Recovered Cases
  master_data_raw_recovered <- read_csv(paste(data_folder_casesdeaths,data_file_recovered,sep="/"))

  #change into tidy data
  master_data_recovered <- master_data_raw_recovered %>%
    select(-Lat,-Long)
  last_day_col <- ncol(master_data_recovered)
  master_data_recovered <- master_data_recovered %>%
    gather(key="Date",value="Recovered",3:last_day_col)

  #reformat dates and long char column names
  master_data_recovered <- master_data_recovered %>%
    mutate(Date = mdy(Date))  %>%
    rename(Country_Region = `Country/Region`, Province_State = `Province/State`)

  ##vaccine data
  try(master_data_raw_vaccine <- read_csv(file = paste(data_folder_vaccines,data_file_vaccines,sep="/")))
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

  master_data <- master_data %>%
    addMissingCountryWideData()

  return(master_data)
}

addCalculatedVariables <- function(master_data) {
  #added data
  library(zoo)
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
    mutate(People_fully_vaccinated_Perc = 100*People_fully_vaccinated/Population) %>%
    mutate(People_partially_vaccinated_Perc = 100*People_partially_vaccinated/Population) %>%
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

  curr_names <- c("Province_State", "Country_Region", "Lat", "Long",
                  "Deaths", "Confirmed", "Recovered", "Doses_admin", "People_partially_vaccinated",
                  "People_fully_vaccinated", "Report_Date_String", "UID")

  JH_Data_countrylevel_for_regionalonly <- JH_Data %>%
    filter(Country_Region %in% unique(regional_only$Country_Region)) %>%
    group_by(across(-c("Province_State","Lat","Long", "Report_Date_String","UID", "Deaths", "Confirmed", "Recovered", "Doses_admin", "People_partially_vaccinated",
                       "People_fully_vaccinated"))) %>%
    #group_by(c("Province_State", "Date")) %>%
    summarise_at(.vars = c("Deaths", "Confirmed", "Recovered", "Doses_admin", "People_partially_vaccinated","People_fully_vaccinated"),
                 .funs = function(x) {sum(x,na.rm = TRUE)}
    )

  JH_Data <- JH_Data %>%
    bind_rows(JH_Data_countrylevel_for_regionalonly)

  return(JH_Data)
}

##Data Manipulation ####

getLatestAvailableDate <- function(JH_Data,CountryName) {
  JH_Data %>%
    filter(Country_Region == CountryName) %>%
    pull(Date) %>%
    max()
}

getLatestDataSimple <- function(JH_Data,CountryName) {
  LatestAvailableDate <- getLatestAvailableDate(JH_Data,CountryName)

  JH_Data %>%
    filter(Country_Region == CountryName) %>%
    filter(Date == LatestAvailableDate)
}

getLatestData <- function(JH_Data,CountryList,spanDays=FALSE) {

  LatestAvailableDate <- as_date(min(sapply(CountryList,
                                            FUN=function(CN) {
                                              as_date(getLatestAvailableDate(master_data,CN))})))

  print(paste("spanDays =",spanDays))

  if (spanDays==FALSE | !(is.numeric(spanDays))) {
    StartDate <- LatestAvailableDate
    print("spanDays not assigned or not numeric, limiting to latest available date")
  } else {
    StartDate <- LatestAvailableDate - days(spanDays-1)
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

JHGetProgress <- function(JH_Data,CountryList,VarName) {
  CountryList <- c("United Kingdom","France","Italy","Germany","Belgium","Greece")

  pos_min <- function(x) {
    min(x[x>0])
  }
  Progress <- master_data %>%
    filter(Country_Region %in% CountryList & is.na(Province_State)) %>%
    filter(Date > ymd("20200401")) %>%
    group_by(Country_Region,Population) %>%
    summarise_at(VarName,list(pos_min, max))

  Progress <- Progress %>%
    rename(!!as.name(paste("min_",VarName,sep="")) := !!as.name("fn1"),
           !!as.name(paste("max_",VarName,sep="")) := !!as.name("fn2"))

  LatestData <- master_data %>%
    filter(is.na(Province_State)) %>%
    getLatestData(CountryList) %>%
    select(Country_Region,!!as.name(VarName))

  LatestData <- LatestData %>%
    rename(!!as.name(paste("current_",VarName,sep="")) := !!as.name(VarName))

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

JHplot_ProvinceLevel <- function(JH_Data,CountryList,VarName,simple=TRUE) {
  theplot <- JH_Data %>%
    filter(Date>ymd("20200315")) %>%
    filter(Country_Region %in% CountryList) %>%
    ggplot(aes_string("Date",VarName,color="Province_State")) +
    geom_point() + geom_line()
  if (simple) {
    print(theplot)
  } else {
    print(ggplotly(theplot))
  }
  #return(p)
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

JHGetdata_CountryLevel <- function(JH_Data,CountryList,VarName) {
  #'TODO Bug ? this does not give right answer for countries including France
  #'possibly because of the regional aspect for that country
  #'810 milions for actual value of 67 gives ratio of 12 which is the
  #'number of france regions. this might be due to how the population is
  #'joined to the original data, it might get added even for rows for which
  #'the province is not NA.
  #'initial attempt is to remove regional information unfortunately this
  #'does not work for countries ike australia for which there is only regional
  #'data available. therefore this needs to be refined

  thedata <- JH_Data %>%
    filter(Date>ymd("20200315")) %>%
    filter(Country_Region %in% CountryList)

  thedata <- thedata %>%
    filter(is.na(Province_State)) #added 20211228 to deal with pop count bug

  thedata <- thedata  %>%
    filter(!is.na(!!as.name(VarName))) %>%
    group_by(Country_Region,Date) %>%
    summarise(!!VarName := sum(!!as.name(VarName))) %>%
    ungroup()

  thedata <- thedata %>%
    mutate(Country_Region= fct_reorder(Country_Region,!!as.name(VarName),function(x) {-tail(x,1)}))

}

JHGetplot_CountryLevel_MultipleVar <- function(JH_Data,CountryList,VarNames) {

  thedata <- JH_Data %>%
    JHGetMultipledata_CountryLevel(CountryList,VarNames) %>%
    pivot_longer(
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

JHplot_CountryLevel <- function(JH_Data,CountryList,VarName,simple=FALSE,add_label = FALSE,adjust_label = FALSE, mindiffval = 5) {

  theplot <- JHGetplot_CountryLevel(JH_Data,CountryList,VarName,add_label,adjust_label,mindiffval)

  if (simple) {
    print(theplot)
  } else {
    ggplotly(theplot,dynamicTicks = TRUE) #%>% layout(xaxis = list(rangeslider = list(type = "date")))
  }
  #return(theplot)
}





