library(shiny)
library(tidyverse)
library(magrittr)
library(patchwork)
library(shinythemes)
library(shinyalert)
library(choroplethr)
library(choroplethrMaps)
library(rnaturalearth) # world map
library(rnaturalearthdata)
library(rgeos)
library(gridExtra)
options(scipen = 200)

source("Part1.R")
source("Part2.R")

ui <- fluidPage(
    theme = shinytheme("cosmo"),
    useShinyalert(),
    navbarPage(
        title = "Heart Disease",
        tabPanel(
            title = "About Heart Disease",
            
            fluidRow(
                tags$div(style = "margin: 1%",
                htmlOutput(outputId = "InfoTitle1"),
                tabsetPanel(
                    tabPanel(title = "Worldwide death rate",
                             tags$div(
                                 style = "margin-right: 4%; margin-left: 4%",
                                 plotOutput(outputId = "plot.WorldDeathRate"),
                                 tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, 
                                             .js-irs-0 .irs-bar {background: #696969; border-color: white}")),
                                 sliderInput(inputId = "plot.WorldDeathRate.select",
                                             label = str_c("Select year ",
                                                           "(map will be plotted on the selected year, ",
                                                           "curves will be plotted on the year range):"),
                                             min = 1990,
                                             max = 2017,
                                             value = 2017,
                                             step = 1,
                                             width = "100%")
                             )),
                    tabPanel(title = "US leading cause of death",
                             br(),
                             tags$div(
                                 style = "margin-right: 9%; margin-left: 9%",
                                 plotOutput(outputId = "plot.CauseOfDeath")
                             ))
                )
                ),
                htmlOutput(outputId = "text.Overview"),
                
                div(align = "right", style = "margin-right: 1%", 
                    checkboxInput(inputId = "show_us_state",
                                  label = "More on the statistics of heart disease in different states across the US"))

            ), # fluidRow
            
            br(),
            
            fluidRow(
                conditionalPanel(
                    condition = "input.show_us_state",
                    tags$div(style = "margin: 1%",
                             htmlOutput(outputId = "InfoTitle2"),
                             
                             column(4,
                                    wellPanel(
                                        titlePanel(title = "Selection Criteria"),
                                        
                                        
                                        selectInput(inputId = "type",
                                                    label   = h4("Type of Disease"),
                                                    choices = c("Heart Disease",
                                                                "Heart Disease-Related Disease" = 
                                                                    "Heart Disease Related Disease"),
                                                    selected = "Heart Disease"
                                        ), # select disease type
                                        
                                        conditionalPanel(condition = "input.type == 'Heart Disease'",
                                                         radioButtons(inputId  = "datatype1",
                                                                      label    = h4("Data Value Type"),
                                                                      choices  = c("Crude Rate (cases per 100,000)" = 
                                                                                       "Crude Rate",
                                                                                   "Age-adjusted Rate (cases per 100,000)" =
                                                                                       "Age-adjusted Rate",
                                                                                   "Number"),
                                                                      selected = "Crude Rate"
                                                         )
                                        ), # conditional panel for Heart Disease for value type
                                        
                                        conditionalPanel(condition = "input.type == 'Heart Disease Related Disease'",
                                                         radioButtons(inputId  = "datatype2",
                                                                      label    = h4("Data Value Type"),
                                                                      choices  = c("Crude Prevalence (%)" =
                                                                                       "Crude Prevalence",
                                                                                   "Age-adjusted Prevalence (%)" = 
                                                                                       "Age-adjusted Prevalence"),
                                                                      selected = "Crude Prevalence"
                                                         )
                                        ), # conditional panel for Heart Disease for year
                                        
                                        conditionalPanel(condition = "input.type == 'Heart Disease'",
                                                         checkboxGroupInput(inputId = "Year1",
                                                                            label   = h4("Year"),
                                                                            selected = 2010,
                                                                            choices = 2010:2017,
                                                                            inline = T,
                                                                            width = "100%"
                                                         )
                                        ), # conditional panel for Heart Disease for year
                                        
                                        conditionalPanel(condition = "input.type == 'Heart Disease Related Disease'",
                                                         checkboxGroupInput(inputId = "Year2",
                                                                            label   = h4("Year"),
                                                                            selected = 2011,
                                                                            choices = 2011:2018,
                                                                            inline = T,
                                                                            width = "100%"
                                                         )
                                        ), # conditional panel for Heart Disease Related Disease for year
                                        
                                        
                                        hr(),
                                        
                                        conditionalPanel(condition = "input.type == 'Heart Disease Related Disease'",
                                                         div(strong("Optional Search") %>% h3()),
                                                         checkboxGroupInput(inputId = "topic",
                                                                            label   = h4("Topic"),
                                                                            choices = c("Pneumonia and Influenza with history of heart disease" = 
                                                                                            "Cardiovascular Disease",
                                                                                        "Arthritis with heart disease" = 
                                                                                            "Arthritis")
                                                         )
                                        ), # conditional panel for Heart Disease Related Disease for topic
                                        
                                        conditionalPanel(condition = "input.type == 'Heart Disease'",
                                                         div(strong("Optional Search") %>% h3())),
                                        
                                        radioButtons(inputId = "category",
                                                     label   = h4("Stratification Category"),
                                                     choices = c("All",
                                                                 "Gender",
                                                                 "Race/Ethnicity",
                                                                 "Overall"),
                                                     inline = T
                                        ), # radioButton for category
                                        
                                        conditionalPanel(condition = ("input.category == 'Gender'"),
                                                         checkboxGroupInput(inputId = "gender",
                                                                            label   = h4("Gender"),
                                                                            choices = c("Male",
                                                                                        "Female")
                                                         ) 
                                        ), # conditional panel for gender
                                        
                                        conditionalPanel(condition = ("input.category == 'Race/Ethnicity'"),
                                                         checkboxGroupInput(inputId = "race",
                                                                            label   = h4("Race/Ethnicity"),
                                                                            choices = c("White, non-Hispanic",
                                                                                        "Multiracial, non-Hispanic",
                                                                                        "Hispanic",
                                                                                        "Black, non-Hispanic",
                                                                                        "Other, non-Hispanic",
                                                                                        "Asian or Pacific Islander",
                                                                                        "American Indian or Alaska Native")
                                                         )
                                        ), # conditional panel for race,
                                        
                                        hr(),
                                        
                                        # action button
                                        div(align = "right",
                                            actionButton(inputId = "getplot",
                                                         label   = strong("Pull Plot"))
                                        ),
                                        
                                        
                                        
                                    ) # wellPanel
                             ), # column left
                             column(8,
                                    conditionalPanel(condition = "input.getplot",
                                                     htmlOutput(outputId = "title3"),
                                                     htmlOutput(outputId = "title3.subtitle"),
                                                     plotOutput(outputId = "map"),
                                                     div(align = "right",
                                                         tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, 
                                                         .js-irs-1 .irs-bar {background: #2a6bb2; border-color: white}")),
                                                         sliderInput(inputId = "num_colors",
                                                                     label   = "Number of bins:",
                                                                     min     = 1,
                                                                     max     = 9,
                                                                     value   = 7))),
                                    DT::dataTableOutput(outputId = "tb")
                             ) # column right
                             
                    )
                )
                
                
            ), # fluidRow
        ), # tabPanel (About Heart Disease)
        
        
        tabPanel(
            title = "Risk Factors for Heart Disease",
            
            div(style = "margin-top: 1%",
                htmlOutput(outputId = "InfoTitle4"),
                div(
                    style = "padding: 1%; margin: 1%; background-color: #f7f7f7",
                    htmlOutput(outputId = "text.Risk"),
                    div(align = "center",
                        actionButton(inputId = "predict",
                                     label = "Click here to start prediction!"))
                )
            ),
            
            hr(),
            
            conditionalPanel(condition = "input.predict",
                             sidebarLayout(
                                 sidebarPanel(
                                     width = 4,
                                     conditionalPanel(
                                         condition = "input.getProb",
                                         plotOutput(outputId = "gaugePlot"),
                                     ),
                                     titlePanel(title = "Input Health Statistics"),
                                     tags$style(HTML("#big-heading{color: black;}")),
                                     radioButtons(inputId  = "sex",
                                                  label    = strong("Sex") %>% h4(),
                                                  choices  = c("female", "male"),
                                                  inline   = TRUE,
                                                  selected = "female"
                                     ), # radioButtons
                                     numericInput(inputId = "age",
                                                  label   = strong("Age") %>% h4(),
                                                  min     = 32,
                                                  max     = 70,
                                                  value   = 32
                                     ), # numericInput
                                     numericInput(input = "sysBP",
                                                  label = strong("Systolic Blood Pressure") %>% h4(),
                                                  min   = 83.5,
                                                  max   = 295,
                                                  value = 90
                                     ), # numericInput
                                     numericInput(input = "glucose",
                                                  label = strong("Glucose") %>% h4(),
                                                  min   = 40,
                                                  max   = 394,
                                                  value = 40
                                     ), # numericInput
                                     radioButtons(input = "smoker",
                                                  label = strong("Do you smoke?") %>% h4(),
                                                  choices = c("Yes", "No"),
                                                  inline  = TRUE,
                                                  selected = "No"
                                     ), # radioButtons
                                     conditionalPanel(condition = "input.smoker == 'Yes'",
                                                      numericInput(inputId = "cigsPerDay",
                                                                   label   = strong("Average cigarettes per day") %>% h4(),
                                                                   min     = 1,
                                                                   max     = 70,
                                                                   value   = 1
                                                      ) # numericInput 
                                     ), # conditionalPanel
                                     div(align = "middle",
                                         actionButton(inputId = "getProb",
                                                      label   = strong("Get Your Risk Evaluation"),
                                                      style   = "font-size:80%"
                                         ) # actionButton
                                     ) # div
                                 ), #sidebarPanel
                                 mainPanel(
                                     div(class = "row",
                                         tags$p(
                                             column(width = 8,
                                                    plotOutput(outputId = "resultsPlot",
                                                               width = "150%",
                                                               height = "850px"
                                                    ) # plotOutput 
                                             ) # column
                                         )
                                     ),
                                     div(class = "row",
                                         align = "right",style = "margin-right: 1%",
                                         conditionalPanel(
                                             condition = "input.getProb",
                                             em(
                                                 br(),
                                                 tags$p("*The model is a logistic regression model."),
                                                 tags$p(str_c('**“Risk” is defined as the ratio of the output predicted by ',
                                                              "logistic regression model to"),
                                                        br(),
                                                        str_c(" the inverse quantile of population ",
                                                              "prevalence of all the logistic fitted values."))
                                             )
                                         )
                                     )
                                 ) # mainPanel
                             ) # sidebarLayout
            ) # conditional panel to pop up prediction
        ), # tabPanel (Risk Factors for Heart Disease)
        
        navbarMenu(
            title = "Data Source",
            tabPanel(
                title = "Worldwide Heart Disease Data",
                DT::dataTableOutput(outputId = "tb.world")
            ),
            tabPanel(
                title = "Risk Model Data",
                DT::dataTableOutput(outputId = "tb.model")
            )
        )
    ) # navbarPage
)

server <- function(input, output) {
    
    ### PAGE 1
    
    # Title1: "What is heart disease?"
    output$InfoTitle1 <- renderText({
        tags$div(tags$h1("What is heart disease?" %>% tags$b()),
                 style = "border-top: 4px solid #1a2937") %>% 
            as.character() %>% HTML()
    })
    # Title2: "Heart disease in the United States"
    output$InfoTitle2 <- renderText({
        tags$div(tags$h1("Heart disease in the United States" %>% tags$b()),
                 style = "border-top: 4px solid #1a2937") %>% 
            as.character() %>% HTML()
    })

    
    # Plot: cause of death barplot and world death rate map
    output$plot.CauseOfDeath <- renderPlot({ plot.CauseOfDeath() })
    output$plot.WorldDeathRate <- renderPlot({ 
        source <- "https://ourworldindata.org/grapher/cardiovascular-disease-death-rates"
        p1 <- plot.WorldDeathRate.map(input$plot.WorldDeathRate.select) 
        p2 <- plot.WorldDeathRate.line(input$plot.WorldDeathRate.select) 
        p1 + p2 + plot_annotation(
            caption = str_c("*Death rate: The annual number of deaths from ",
                            "cardiovascular diseases per 100,000 people.\n",
                            "**Data source: ", source),
            theme = theme(plot.caption = element_text(face = "italic", size = 11))
        )
    })
    
    # Text: overview of heart disease
    output$text.Overview <- renderText({
        tags$div(
            style = "padding: 2%; margin: 1%; background-color: #f7f7f7",
            tags$h2("Overview:"),
            tags$p( # introduction
                str_c("Heart disease describes a range of conditions that affect your heart. ", 
                      "Diseases under the heart disease umbrella include blood vessel diseases, ",
                      "such as coronary artery disease; heart rhythm problems (arrhythmias); ",
                      "and heart defects you're born with (congenital heart defects), among others.")
            ),
            tags$p( # introduction
                str_c("The term 'heart disease' is often used interchangeably ",
                      "with the term 'cardiovascular disease.' Cardiovascular disease ",
                      "generally refers to conditions that involve narrowed or blocked ",
                      "blood vessels that can lead to a heart attack, chest pain (angina) ",
                      "or stroke. Other heart conditions, such as those that affect ",
                      "your heart's muscle, valves or rhythm, also are considered forms of heart disease.")
            ),
            tags$p( # worldwide
                str_c("Globally, CVDs are the number 1 cause of death, ",
                      "taking an estimated 17.9 million lives each year and ",
                      "an estimated 31% of all deaths worldwide.")
            ),
            tags$p( # US
                str_c("In the United States, according to the Centers for Disease Control and Prevention (CDC), ",
                      "heart disease is the leading cause of death. ",
                      "Around 1 in 4 deaths in the U.S. occur due to heart disease, ",
                      "and it affects all genders as well as all racial and ethnic groups.")
            ),
            tags$p( # prevention
                str_c("People with cardiovascular disease or who are at high cardiovascular ",
                      "risk (due to the presence of one or more risk factors such as ",
                      "hypertension, diabetes, hyperlipidaemia or already established disease) ",
                      "need early detection and management using counselling and medicines, as appropriate.")
            )
            
        ) %>% 
            as.character() %>% HTML()
    })
    
    
    # Data: data selected by the widgets
    data_pulled <- eventReactive(input$getplot, {
        if (input$type == "Heart Disease") {
            data1 <- HeartData %>% 
                filter(location_id != 59) %>%
                filter(year_start %in% input$Year1) %>% 
                mutate(region = tolower(location_desc)) %>%
                dplyr::select(-location_desc) %>%
                { if (!is.null(input$category) & (input$category != "All")) 
                    filter(., stratification_category1 %in% input$category)
                    else select(., everything()) } %>%
                { if (!is.null(input$race) & "Race/Ethnicity" %in% input$category) 
                    filter(., stratification1 %in% input$race)
                    else select(., everything()) } %>%
                { if (!is.null(input$gender) & "Gender" %in% input$category) 
                    filter(., stratification1 %in% input$gender)
                    else select(., everything()) } %>%
                { if (!is.null(input$datatype1)) filter(., data_value_type %in% input$datatype1)
                    else select(., everything()) } %>%
                group_by(region) %>% 
                summarise(value = mean(data_value_alt))
        } else if (input$type == "Heart Disease Related Disease") {
            data1 <- HeartRelatedData %>% 
                filter(location_id != 59) %>%
                filter(year_start %in% input$Year2) %>% 
                mutate(region = tolower(location_desc)) %>%
                select(-location_desc) %>%
                { if (!is.null(input$topic)) filter(., topic %in% input$topic)
                    else select(., everything()) } %>%
                { if ((!is.null(input$category)) & (input$category != "All"))
                    filter(., stratification_category1 %in% input$category)
                    else select(., everything()) } %>%
                { if (!is.null(input$race) & "Race/Ethnicity" %in% input$category) 
                    filter(., stratification1 %in% input$race)
                    else select(., everything()) } %>%
                { if (!is.null(input$gender) & "Gender" %in% input$category) 
                    filter(., stratification1 %in% input$gender)
                    else select(., everything()) } %>%
                { if (!is.null(input$datatype2)) filter(., data_value_type %in% input$datatype2)
                    else select(., everything()) } %>%
                group_by(region) %>% 
                summarise(value = mean(data_value_alt))
        }
        if (nrow(data1) == 0) {
            showModal(
                modalDialog("No data available, please change your search parameters!",
                            easyClose = T, fade = F)
            )
        }
        data1
    })
    
    # Data: top 3 regions with the highest mean value
    data_pulled.top <- eventReactive(input$getplot, {
        data_pulled() %>% 
            arrange(desc(value)) %>% 
            slice(1:3) %>% 
            pull(region)
    })
    # Year: to print subtitle
    year <- eventReactive(input$getplot, {
        if (input$type == "Heart Disease") { str_c(input$Year1, collapse = ", ") }
        else { str_c(input$Year2, collapse = ", ") }
    })
    
    # Title3: Disease map for the US
    title3 <- eventReactive(input$getplot, {
        if (input$type == "Heart Disease") {
            str_c("US States Mean Value Map of ", input$datatype1, " for ", input$type) %>%
                tags$h3() %>% tags$div() %>% as.character() %>% HTML()
        } else {
            str_c("US States Mean Value Map of ", input$datatype2, " for Heart Disease-Related Disease") %>%
                tags$h3() %>% tags$div() %>% as.character() %>% HTML()
        }
        
    })
    output$title3 <- renderText({ title3() })
    
    output$title3.subtitle <- renderText({
        tags$div(
            str_c("Year: ", year()) %>% tags$p(),
            str_c("Top 3 states with the highest mean value:  ",
                  str_c(data_pulled.top() %>% str_to_title(), collapse = ", ")) %>% tags$p()
        ) %>% 
            em() %>% tags$h4() %>% tags$div() %>% as.character() %>% HTML()
    })
    
    # Table: Disease map for the US
    output$tb <- DT::renderDataTable({
        data_pulled() %>% 
            mutate_at(vars(value), ~round(., 3)) %>% 
            mutate_at(vars(region), str_to_title) %>% 
            set_colnames(c("State", "State Mean Level")) %>% 
            DT::datatable()
    })
    
    # Plot: Disease map for the US
    output$map <- renderPlot({
        state_choropleth(data_pulled(),
                         title      = "",
                         num_colors = input$num_colors)
    })
    
    
    
    ### PAGE 2
    
    # warnings
    observeEvent(input$getProb, {
        if (input$age < 32 | input$age > 70) {
            shinyalert(title = "Reminder!",
                       text = str_c("The age permitted is from 32 to 70. \n",
                                    "The output may not be significative."),
                       type = "warning")
        }
        
        if (input$sysBP < 83.5 | input$sysBP > 295) {
            shinyalert(title = "Reminder!",
                       text = str_c("The Systolic Blood Pressure permitted is ",
                                    "from 83.5 to 295. \n",
                                    "The output may not be significative."),
                       type = "warning")
        }
        
        if (input$glucose < 40 | input$glucose > 394) {
            shinyalert(title = "Reminder!",
                       text = str_c("The glucose permitted is from 40 to 394. ",
                                    "\n The output may not be significative."),
                       type = "warning")
        }
        
        if (input$smoker == "Yes" &
            (input$cigsPerDay < 0 | input$cigsPerDay > 70)) {
            shinyalert(title = "Reminder!",
                       text = str_c("The average cigarettes per day permitted ",
                                    "is from 0 to 70. ",
                                    "\n The output may not be significative."),
                       type = "warning")
        }
    })
    
    # data
    data_orig <- eventReactive(input$getProb, { get_data() })
    
    # making the data reactive to events
    user_data_sex <- eventReactive(input$getProb, { input$sex })
    
    user_data_age <- eventReactive(input$getProb, { input$age })
    
    user_data_sysBP <- eventReactive(input$getProb, { input$sysBP })
    
    user_data_smoker <- eventReactive(input$getProb, { input$smoker })
    
    user_data_cigsPerDay <- eventReactive(input$getProb, {
        if (input$smoker == "Yes") { input$cigsPerDay } 
        else { 0 }
    })
    
    user_data_glucose <- eventReactive(input$getProb, { input$glucose })
    
    # generate the plots (using event reactive data)
    output$resultsPlot <- renderPlot({
        ageplot <- ggplot(data = data.frame(age = data_orig()$age)) +
            geom_density(aes(x = age), alpha = .5, bw = 8, size = 1, fill = "#ee6f57") +
            xlim(32, 70) +
            geom_vline(xintercept = user_data_age(), size = 1, color = "red") +
            annotate("label",
                     x = user_data_age(),
                     y = 0,
                     label = formatC(mean(user_data_age() > data_orig()$age),
                                     digits = 3)
            ) +
            ggtitle("Your Age Quantile") +
            xlab("Age") + ylab("Density") +
            theme_classic(base_size = 13)
        
        bpplot <- ggplot(data = data.frame(sysBP = data_orig()$sysBP)) +
            geom_density(aes(x = sysBP), alpha = .5, bw = 22, size = 1, fill = "grey") +
            xlim(83.5, 296) +
            geom_vline(xintercept = user_data_sysBP(), size = 1, color = "red") +
            annotate("label",
                     x = user_data_sysBP(),
                     y = 0,
                     label = formatC(mean(user_data_sysBP() > data_orig()$sysBP),
                                     digits = 3)) +
            ggtitle("Your Systolic Blood Pressure Quantile") +
            xlab("Systolic Blood Pressure") + ylab("Density") +
            theme_classic(base_size = 13)
        
        cpdplot <- ggplot(data = data.frame(cpd = data_orig()$cigsPerDay)) +
            geom_density(aes(x = cpd), alpha = .5, bw = 11, size = 1, fill = "#fddb3a") +
            xlim(0, 70) +
            geom_vline(xintercept = user_data_cigsPerDay(), size = 1, color = "red") +
            annotate("label",
                     x = user_data_cigsPerDay(),
                     y = 0,
                     label = formatC(mean(user_data_cigsPerDay() > data_orig()$cigsPerDay),
                                     digits = 3)) +
            ggtitle("Your Average Cigarettes Smoked Per Day Quantile") +
            xlab("Cigarettes Smoked Per Day") + ylab("Density") +
            theme_classic(base_size = 13)
        
        gluplot <- ggplot(data = data.frame(glucose = data_orig()$glucose)) +
            geom_density(aes(x = glucose), alpha = .5, bw = 24, size = 1, fill = "#51adcf") +
            xlim(40, 394) +
            geom_vline(xintercept = user_data_glucose(), size = 1, color = "red") +
            annotate("label",
                     x = user_data_glucose(),
                     y = 0,
                     label = formatC(mean(user_data_glucose() > data_orig()$glucose),
                                     digits = 3)) +
            ggtitle("Your Glucose Quantile") +
            xlab("Glucose") + ylab("Density") +
            theme_classic(base_size = 13)

        grid.arrange(
            ageplot,
            bpplot,
            cpdplot,
            gluplot,
            ncol = 2,
            nrow = 2,
            widths = c(2, 2),
            clip = FALSE
        )
    })
    
    output$gaugePlot <- renderPlot({
        # risk calculation
        risk <- get_risk(Sex = user_data_sex(), 
                         Age = user_data_age(),
                         sysBP = user_data_sysBP(), 
                         glucose = user_data_glucose(),
                         cigsPerDay = user_data_cigsPerDay(),
                         smoker = user_data_smoker())
        gg.gauge(risk)
    })
    
    # Title4: "Start your own heart disease risk prediction!"
    output$InfoTitle4 <- renderText({
        tags$div(tags$h1("Start your own heart disease risk prediction!" %>% tags$b()),
                 style = "border-top: 4px solid #1a2937") %>% 
            as.character() %>% HTML()
    })
    output$text.Risk <- renderText({
        tags$div(
            tags$p(
                str_c("People with cardiovascular disease or who are at high cardiovascular ",
                      "risk (due to the presence of one or more risk factors such as ",
                      "hypertension, diabetes, hyperlipidaemia or already established disease) ",
                      "need early detection and management using counselling and medicines, as appropriate.")
            ),
            tags$p(
                str_c("Based on a database which collected over 3,500 heart disease patients information, ",
                      "we are able to identify the important factors that affect the risk of getting the heart disease. ",
                      "It turns out that overweight, unhealthy diet and smoking significantly ",
                      "increase the risk of getting heart disease! ",
                      "By living a healthy lifestyle, you can help keep your blood pressure, ",
                      "cholesterol, and blood sugar levels normal and lower your risk for heart ",
                      "disease and heart attack.")
            ),
            tags$p(
                str_c("Now, based on the model, let's get your own prediction for getting heart disease!")
            )
        ) %>% as.character() %>% HTML()
    })
    
    
    ### PAGE 3
    
    output$tb.world <- DT::renderDataTable({
        WorldDeathRate %>% 
            mutate_at(vars(death_rate), ~round(., 3)) %>% 
            set_colnames(c("Region", "Region Code", "Year", "Death Rate")) %>% 
            DT::datatable()
    })
    
    output$tb.model <- DT::renderDataTable({
        get_data() %>% 
            relocate(TenYearCHD) %>% 
            set_rownames(c(seq_len(nrow(.)))) %>% 
            DT::datatable(option = list(scrollX = T))
    })

}

shinyApp(ui = ui, server = server)



