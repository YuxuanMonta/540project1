# If running the app.R, then the current folder is "HeartDisease/"
# The default R console is in "project-hargow/", if opened by the .Rproj
# To run the app successfully, the large csv should be in "HeartDisease/Data/"

# GET DATA

# define a function returning the index of matched string
get_pattern_index <- function(allcharacters, pattern = "heart") {
  index <- str_match(allcharacters, pattern) %>% is.na() %>% `!` 
  which(index == T)
}

get_data.preprocessed <- function() {
  # load raw data
  if ("US_Chronic_Disease_Indicators.csv" %in% list.files("Data/")) {
    RawData <- read.csv("Data/US_Chronic_Disease_Indicators.csv")
  } else {
    # If it is too slow to download the csv file in your R console, 
    # directly copy the URL in the browser may help.
    # Then, make sure to rename the csv file as "US_Chronic_Disease_Indicators.csv"
    # and move it into "HeartDisease/Data/", 
    download.file(
      "https://chronicdata.cdc.gov/api/views/g4ie-h725/rows.csv?accessType=DOWNLOAD",
      "Data/US_Chronic_Disease_Indicators.csv"
    )
    RawData <- read.csv("Data/US_Chronic_Disease_Indicators.csv")
  }
  
  # pre-processing raw data
  RawData %>% 
    # delete unnecessary variables
    dplyr::select(-Response, -ResponseID, 
                  -DataValueFootnoteSymbol, -DatavalueFootnote,
                  -StratificationCategory2, -Stratification2, 
                  -StratificationCategory3, -Stratification3, 
                  -StratificationCategoryID2, -StratificationCategoryID3,
                  -StratificationID2, -StratificationID3) %>% 
    # get the data related to heart disease
    .[get_pattern_index(.$Question, "heart"),] %>% 
    # delete missingness
    filter(!is.na(DataValueAlt)) %>% 
    janitor::clean_names()
}


get_data.heart <- function(data.preprocessed) {
  # Data: pure heart disease
  data.preprocessed[-c(get_pattern_index(data.preprocessed$question, "history"),
                       get_pattern_index(data.preprocessed$question, "Arthritis")),]
}
get_data.heartrelated <- function(data.preprocessed) {
  # Data: disease related to heart disease
  data.preprocessed[c(get_pattern_index(data.preprocessed$question, "history"),
                      get_pattern_index(data.preprocessed$question, "Arthritis")),]
}

data.preprocessed <- get_data.preprocessed()
HeartData <- get_data.heart(data.preprocessed)
HeartRelatedData <- get_data.heartrelated(data.preprocessed)


# PLOTS IN THE TOP PANEL

plot.CauseOfDeath <- function() {
  CauseOfDeath <- 
    readxl::read_xlsx("Data/CDC_NCHS_Cause_of_death.xlsx", sheet = 1) %>% 
    janitor::clean_names() %>% 
    dplyr::select(-percent) %>% 
    mutate(sign = ifelse(cause_of_death == "Diseases of heart", 1, 0),
           label = str_c(number, ", ", rate, "%")) %>% 
    filter(cause_of_death != "All causes")
  
  CauseOfDeath %>% 
    ggplot() +
    geom_bar(aes(reorder(cause_of_death, number), number, fill = as.factor(sign)), 
             stat = "identity", show.legend = F) +
    geom_label(aes(reorder(cause_of_death, number), number, 
                   label = label),
               nudge_y = 53000, size = 4, label.padding = unit(0.25, "lines")) +
    ylim(c(NA, 760000)) +
    scale_fill_manual(values = c("#8aabc7", "#2e4152")) +
    labs(caption = str_c("*Percentage is the death rate ", 
                         "(deaths per 100,000 U.S. standard population).\n",
                         "**Data source: CDC, National Center for Health Statistics ",
                         "(see https://www.cdc.gov/nchs/fastats/deaths.htm)."),
         title = "Top 10 leading causes of death in the U.S., 2018",
         subtitle = str_c("Heart disease is the top leading cause of death, ",
                          "followed by cancerous tumor and unintentional injuries.")) +
    theme_light(base_size = 17) + coord_flip() +
    theme(axis.title = element_blank(),
          plot.caption = element_text(face = "italic"))
}


WorldDeathRate <- read.csv("Data/cardiovascular-disease-death-rates.csv") %>% 
  set_colnames(c("entity", "code", "year", "death_rate"))
world <- ne_countries(scale = "medium", returnclass = "sf") %>% .[,"iso_a3"]
plot.WorldDeathRate.map <- function(year_input) {
  inner_join(world, WorldDeathRate, by = c("iso_a3" = "code")) %>% 
    filter(year == year_input) %>% 
    ggplot() + theme_light(base_size = 17) +
    geom_sf(aes(fill = death_rate), color = "#363636", size = 0.1) +
    scale_fill_gradient(low = "#FFFFFF", high = "#133a5e") + 
    labs(title = str_c("Worldwide death rate from cardiovascular disease, ", 
                       year_input),
         subtitle = str_c("Globally, 17.9 million deaths each year, ",
                          "taking up 31% of all deaths.")) +
    theme(legend.position = "right", 
          axis.text = element_blank(),
          legend.title = element_blank(),
          legend.key.width = unit(0.3, "cm"),
          legend.key.height = unit(2, "lines"),
          plot.subtitle = element_text(size = 15))
}


countries <- c("United States", "China", "Japan", "Russia", "Greenland", 
               "Indonesia") %>% sort()
plot.WorldDeathRate.line <- function(year_input) {
  WorldDeathRate_select <- WorldDeathRate %>% 
    filter(entity %in% countries) %>% 
    filter(year <= year_input)
  
  label_df <- data.frame(label_x = year_input, 
                         label_y = WorldDeathRate_select %>% 
                           filter(year == year_input) %>% pull(death_rate),
                         entity = WorldDeathRate_select %>% 
                           filter(year == year_input) %>% pull(entity)) 
  emptyspace <- (year_input - 1990) * 0.08
  axis.x.breaks <- seq(1990, year_input+emptyspace*5,
                       ifelse(year_input > 2000, 2, 1))
  axis.x.labels <- axis.x.breaks
  axis.x.labels[axis.x.labels > year_input] <- ""
  
  countries[countries == "United States"] <- "US" # shorten the label
  
  WorldDeathRate_select %>% 
    ggplot() + theme_light(base_size = 17) +
    geom_line(aes(x = year, y = death_rate, color = entity)) +
    geom_point(aes(x = year, y = death_rate, color = entity), shape = 1) +
    geom_label(aes(x = label_x, y = label_y, label = countries), data = label_df,
               nudge_x = emptyspace, size = 3) +
    scale_color_manual(
      values = colorRampPalette(c("#d01b1b", "#ffc5b8", 
                                  "#85b7d6", "#0b2841"))(length(countries))
    ) +
    scale_x_continuous(breaks = axis.x.breaks, labels = axis.x.labels) +
    theme(axis.title = element_blank(), legend.position = "none",
          axis.text.x = element_text(size = 13))
}



