library(bigrquery)
library(dbplyr)
library(DBI)
library(tidyverse)
library(shiny)
library(dqshiny)

mimic <- readRDS("mimic_icu_cohort.rds") |>
  mutate(race = as_factor(race), insurance = as_factor(insurance),
         marital_status = as_factor(marital_status), 
         gender = as_factor(gender)) |>
  rename("systolic_blood_pressure" = "non_invasive_blood_pressure_systolic", 
         "diastolic_blood_pressure" = "non_invasive_blood_pressure_diastolic")

satoken <- "../biostat-203b-2024-winter-313290ce47a6.json"
bq_auth(path = satoken)
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2024-winter",
  dataset = "mimic4_v2_2",
  billing = "biostat-203b-2024-winter"
)

# create a new shiny ui
mimic_ui <- fluidPage(
  
  # application title
  titlePanel("MIMIC-IV ICU Cohort Explorer"), 
  
  # create tabs
  tabsetPanel(
    
    # first tab
    tabPanel("Overall Data Summary", 
             
             # Sidebar layout with input and output definitions
             sidebarLayout(
               
               # Sidebar panel for inputs
               sidebarPanel(
                 
                 # Input: Text for providing a caption 
                 textInput(inputId = "caption", 
                           label = "Caption:", 
                           value = "Data Summary"),
                 
                 # Input: choose a dataset (first level category)
                 selectInput(inputId = "dataset", 
                             label = "Choose a Dataset:",
                             choices = c("demographics", "lab measurements", 
                                         "vitals")),
                 
                 # Input: choose a variable (second level category)
                 conditionalPanel(
                   condition = "input.dataset == 'demographics'",
                   selectInput(inputId = "demo_summary",
                               label = "Demographics Variable:",
                               choices = c("race", "insurance", 
                                           "marital_status", 
                                           "gender", "age_intime"))),
                 
                 # Input: choose a variable (second level category)
                 conditionalPanel(
                   condition = "input.dataset == 'lab measurements'",
                   selectInput(inputId = "lab_summary",
                               label = "Lab Variable:",
                               choices = c("bicarbonate", "chloride", 
                                           "creatinine", "glucose", 
                                           "potassium", "sodium", 
                                           "hematocrit", "wbc"))),
                 
                 # Input: choose a variable (second level category)
                 conditionalPanel(
                   condition = "input.dataset == 'vitals'", 
                   selectInput(inputId = "vital_summary",
                               label = "Vital Variable:",
                               choices = c("heart_rate", 
                                           "systolic_blood_pressure", 
                                           "diastolic_blood_pressure", 
                                           "respiratory_rate", 
                                           "temperature_fahrenheit"))),
                 
                 # Input: Slider for the limit of x-axis
                 uiOutput("xlimit")
               ),
               
               # Main panel for displaying outputs
               mainPanel(
                 
                 # Output: Formatted text for caption
                 h3(textOutput("caption", container = span)),
                 
                 # Output: Verbatim text for data summary
                 verbatimTextOutput("summary"),
                 
                 # Output: plot of the data
                 plotOutput(outputId = "sumPlot")
               )
             )
    ), 
    
    # second tab
    tabPanel("Patient Information", 
             
             # Sidebar layout with input and output definitions
             sidebarLayout(
               
               # Sidebar panel for inputs
               sidebarPanel(
                 
                 # Input: Text for providing a patient ID
                 autocomplete_input(id = "patient_id", 
                                   label = "Patient ID:", 
                                   options = 
                                     as.character(unique(mimic$subject_id)),
                                   max_options = 10, create = TRUE),
                 
                 # Input: Selector for choosing a type of information
                 selectInput(inputId = "info_type",
                             label = "Type of Information:",
                             choices = c("ADT", "ICU Stay"))
               ),
               
               # Main panel for displaying outputs
               mainPanel(
                 
                 # Output: Formatted text for caption
                 h3(textOutput("patient_id", container = span)),
                 
                 # Output: plot the patient information
                 uiOutput("plot_or_error")
               )
             )
    )
  )
)




mimic_server <- function(input, output, session) {
  
  # create ractive data
  filtered_data <- reactive({
    switch(input$dataset, 
           "demographics" = mimic[, input$demo_summary],
           "lab measurements" = mimic[, input$lab_summary],
           "vitals" = mimic[, input$vital_summary])
  })
  
  # render the caption
  output$caption <- renderText({
    input$caption
  })
  
  # render the numerical summary
  output$summary <- renderPrint({
    summary(filtered_data())
  })
  
  # render the graphical summary
  output$sumPlot <- renderPlot({
    data <- filtered_data()
    
    if (is.factor(data[[colnames(data)]])) {
      
      # draw a bar plot if it is a categorical variable
      ggplot(data, aes_string(x = colnames(data), fill = colnames(data))) + 
        geom_bar() +
        labs(title = colnames(data)) +
        theme(axis.text.x = element_blank()) +
        theme_bw()
      } else {
        
        # draw a histogram if it is a continuous variable
        ggplot(data, aes_string(x = colnames(data))) + 
          geom_histogram(binwidth = 1, fill = "salmon", color = "black") +
          labs(title = colnames(data)) +
          xlim(input$xlimit) +
          theme_bw()
        }
  })
  
  # render the slider
  output$xlimit <- renderUI({
    data <- filtered_data()
    if (is.numeric(data[[colnames(data)]])) {
      sliderInput(inputId = "xlimit",
                  label = "Limit of x-axis:",
                  min = min(data, na.rm = TRUE),
                  max = max(data, na.rm = TRUE),
                  value = c(min(data, na.rm = TRUE),
                            max(data, na.rm = TRUE)))
      }
  })
  
  # render the patient id
  output$patient_id <- renderText({
    paste0("Patient ", input$patient_id, " Information")
  })
  
  
  output$plot_or_error <- renderUI({
    if (input$patient_id %in% mimic$subject_id) {
      plotOutput("idPlot")
      } else {
        textOutput("idText")
        }
  })
  
  # render the patient plot
  output$idPlot <- renderPlot({
    if (input$info_type == "ADT") {
      patient_id <- as.numeric(input$patient_id) # set patient ID
      
      # find race for that patient
      race_info <- mimic |>
        filter(subject_id == patient_id) |>
        select(subject_id, race)
      
      # find other personal info for that patient
      personal_info <- mimic |>
        filter(subject_id == patient_id) |>
        select(subject_id, gender, anchor_age) |>
        rename(age = anchor_age) |>
        left_join(race_info, by = "subject_id") |>
        distinct()
      
      # find top 3 diagnoses for that patient in each stay ID
      diagnose_info <- tbl(con_bq, "diagnoses_icd") |> 
        filter(subject_id == patient_id) |>
        select(-icd_version) |>
        filter(seq_num <= 3) |>
        left_join(tbl(con_bq, "d_icd_diagnoses"), by = "icd_code") |>
        select(-icd_version) |>
        collect()
      
      # find ADT info for that patient
      ADT_info <- tbl(con_bq, "transfers") |>
        filter(subject_id == patient_id) |>
        select(subject_id, hadm_id, intime, outtime, eventtype, careunit) |>
        filter(eventtype != "discharge") |>
        arrange(intime) |>
        collect() |>
        distinct() 
      
      # find lab info for that patient
      lab_info <- tbl(con_bq, "labevents") |>
        filter(subject_id == patient_id) |>
        select(subject_id, hadm_id, charttime) |>
        collect() |>
        distinct()
      
      # find procedure info for that patient
      procedure_info <- tbl(con_bq, "procedures_icd") |>
        filter(subject_id == patient_id) |>
        select(subject_id, hadm_id, chartdate, icd_code) |>
        mutate(chartdate = as.POSIXct(chartdate)) |>
        left_join(tbl(con_bq, "d_icd_procedures"), by = "icd_code") |>
        select(-icd_version) |>
        arrange(chartdate) |>
        collect() |>
        
        # split long title and only keep the first part
        mutate(short_title = str_split(long_title, ",") %>% sapply(function(x)
          x[1])) |>
        distinct()
      
      # combine personal info into required format
      title_text <- sprintf("Patient %d, %s, %d years old, %s",
                            personal_info$subject_id, personal_info$gender,
                            personal_info$age, tolower(personal_info$race))
      
      top3_text <- diagnose_info |>
        slice(1:3) |>
        select(long_title) |>
        mutate(first_five_words = str_split(long_title, "\\s+") %>%
                 sapply(function(x) {
                   if (length(x) < 5) {
                     
                     # in case some diagnosis has less than 5 words
                     paste(x, collapse = " ")
                     } else {
                       paste(x[1:5], collapse = " ")
                       }
                   })) |>
        pull(first_five_words)
      
      subtitle_text <- sprintf("%s\n%s\n%s", top3_text[1], top3_text[2],
                               top3_text[3])
      
      # create the plot and y-axis
      empty_plot <- ggplot() +
        geom_blank() +
        scale_y_discrete(limits = c("Procedure", "Lab", "ADT")) +
        labs(x = "Calendar Time", title = title_text,
             subtitle = tolower(subtitle_text)) +
        theme_bw()
      
      ADT_plot <- empty_plot +
        geom_segment(data = ADT_info, aes(x = intime, xend = outtime,
                                          y = "ADT", yend = "ADT",
                                          color = careunit),
                     linewidth = ifelse(str_detect(ADT_info$careunit, "CCU") |
                                          str_detect(ADT_info$careunit,
                                                     "ICU"), 8, 3)) +
        labs(color = "Care Unit") +
        theme(legend.position = "bottom", legend.box = "vertical",
              text = element_text(size = 8))
      
      Lab_plot <- ADT_plot +
        geom_point(data = lab_info, aes(x = charttime, y = "Lab"),
                   size = 5, shape = "+")
      
      Procedure_plot <- Lab_plot +
        geom_point(data = procedure_info, aes(x = chartdate, y = "Procedure",
                                              shape = short_title),
                   size = 5) +
        
        # set manually the shape of the procedure to be the total procedures
        scale_shape_manual(values =
                             seq(1, length(
                               unique(procedure_info$short_title)))) +
        labs(shape = "Procedure", y = "") +
        guides(shape = guide_legend(ncol = 2, order = 1),
               color = guide_legend(ncol = 2, order = 2),
               linewidth = FALSE) +
        theme(legend.position = "bottom", legend.box = "vertical",
              text = element_text(size = 8))
      
      plot(Procedure_plot)
      
      } else {
      
        patient_id <- as.numeric(input$patient_id)
        
        item_icu_info <- tbl(con_bq, "d_items") |>
          filter(itemid %in% c(220045, 220180, 220179, 220210, 223761)) |>
          select(itemid, abbreviation) |>
          collect() |>
          distinct()
        
        chartevents_info <- tbl(con_bq, "chartevents") |>
          filter(subject_id == patient_id) |>
          filter(itemid %in% c(220045, 220180, 220179, 220210, 223761)) |>
          select(subject_id, stay_id, itemid, charttime, valuenum) |>
          collect() |>
          left_join(item_icu_info, by = "itemid") |>
          mutate(charttime = with_tz(charttime, "UTC"))
        
        title_text <- sprintf("Patient %d ICU stays - Vitals",
                              chartevents_info$subject_id)
        
        icu_plot <- ggplot(chartevents_info, aes(x = charttime, y = valuenum,
                                                 color = abbreviation)) +
          geom_point(size = 1) +
          geom_line() +
          labs(title = title_text,
               x = "",
               y = "") +
          facet_grid(rows = vars(abbreviation), cols = vars(stay_id),
                     scales = "free") +
          theme_light() +
          theme(legend.position = "none") +
          scale_x_datetime(date_labels = "%b %d %H:%M") +
          
          # to avoid overlapping of x-axis labels
          guides(x = guide_axis(n.dodge = 2))
        
        plot(icu_plot)
        }
  })
  
  output$idText <- renderPrint({
    "Patient ID is not found in the dataset. Please enter a valid patient ID."
  })
  
}

shinyApp(mimic_ui, mimic_server)


