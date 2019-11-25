library(tidyverse)
library(shinydashboard)
library(shiny)
library(shinydashboardPlus)
library(shinyjs)
library(plotly)
library(DT)
library(kableExtra)
library(magrittr)
library(ggthemes)
library(memisc) 

################################################################################################
source("starter_code_shiny.R")
################################################################################################


################################################################################################
# Data Manipulation
################################################################################################

# Manipulate datasets for Student Selector
## Weekly Effort
weekly_effort <- weekly_effort %>% 
  mutate(effort_hr = round(effort_sec / 3600, 2)) %>% 
  mutate(effort_range = case_when(effort_hr <= 1 ~ 1,
                                  effort_hr > 1 & effort_hr <= 2 ~ 2,
                                  effort_hr > 2 & effort_hr <= 3 ~ 3,
                                  effort_hr > 3 ~ 4))
  
## Event Types
event_type_short <- unique(event_xtract$event_type)
### Take out all the extraneous information before backslash
event_type_short <- sub(".*/", "", event_type_short)
event_type_short <- unique(event_type_short)
### Take out event types with digits
which_digit <- event_type_short %>% str_detect("[:digit:]")
event_type_short <- event_type_short[which(which_digit == 0)]


## Activity Grade
activity_grade %<>% mutate(percent_grade_range = 
                             case_when(percent_grade == 0 ~ 1,
                                       percent_grade > 0 & percent_grade <= 20 ~ 2,
                                       percent_grade > 20 & percent_grade <= 40 ~ 3,
                                       percent_grade > 40 & percent_grade <= 60 ~ 4,
                                       percent_grade > 60 & percent_grade <= 80 ~ 5,
                                       percent_grade > 80 & percent_grade < 100 ~ 6,
                                       percent_grade == 100 ~ 7))

## Video Interaction
video_int_type <- unique(video_int$event_type)



################################################################################################
# Pretty Student IDs
################################################################################################

# Make pretty student IDs for the anon_id ---------------------------------
## "Pretty Student IDs" for Final Grades
student_id_pretty <- paste("Student", 1:length(final_grade$anon_id))

## "Pretty Student IDs" for Weekly Effort
student_id_pretty_weekly <- paste("Student", 1:length(weekly_effort$anon_screen_name))

## "Pretty Student IDs" for Event Xtract
student_id_pretty_event <- paste("Student", 1:length(event_xtract$anon_screen_name))

## "Pretty Student IDs" for Activity Grade
student_id_pretty_activity <- paste("Student", 1:length(activity_grade$anon_screen_name))

## "Pretty Student IDs" for Video Interaction
student_id_pretty_video <- paste("Student", 1:length(video_int$anon_screen_name))
################################################################################################


################################################################################################
# Overview of Class Tab: Info box Numbers
################################################################################################
num_students <- length(reduce(list(event_xtract$anon_screen_name,
                                   activity_grade$anon_screen_name,
                                   weekly_effort$anon_screen_name), intersect))

dropout_rate <- round(100 * length(total_dropout) / length(reduce(list(event_xtract$anon_screen_name,
                                                                       activity_grade$anon_screen_name,
                                                                       weekly_effort$anon_screen_name), intersect)), 0)


################################################################################################
# Start of Shiny App ------------------------------------------------------
################################################################################################

header <- dashboardHeader(
  title = "MOOC Effort Dashboard",
  titleWidth = 240
)

sidebar <- dashboardSidebar(
  div(img(src = "https://mvideos.stanford.edu/Images/DestinyImages/Department%20Images/460x259/Statistics_Dept.jpg",
          height = 100, width = 200), style="text-align: center;"),
  width = 240,
  sidebarMenu(
    menuItem(text = "Introduction", tabName = "intro", icon = icon("folder")),
    menuItem(text = "Overview of Class", tabName = "basic", icon = icon("folder")),
    menuItem(text = "Student Selector", tabName = "student_selector", icon = icon("folder")),
    menuItem(text = "Effort Level", tabName = "effort_level", icon = icon("folder")),
    menuItem(text = "K-Means Clustering", tabName = "clustering", icon = icon("folder"))
    
  ),
  
  h5("Built with",
     img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
     "by",
     img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px")
  )
)

body <- dashboardBody(
  
  
  tabItems(
    tabItem(tabName = "intro",
            fluidRow(
              box(
                h2("Welcome to the Massive Open Online Course (MOOC) Effort Dashboard", style="text-align: center;"),
                br(),
                p(
                  span("This is an interactive dashboard built by ", style = "font-size:20px"),
                  a(href = "http://insidethetv.rbind.io/","Howard Baek", style = "font-size:20px"),
                  span("as part of the", style = "font-size:20px"),
                  a(href = "https://cs.gmu.edu/~reu/","Research Experience for Undergraduates (REU) program @ George Mason University.", style = "font-size:20px")
                ),
                br(),
                p(
                  span("Intended audience: MOOC instructors and administrators", style = "font-size:20px;font-weight:bold")
                ),
                br(),
                p(
                  h4("Explanation of Tabs", style = "font-size:25px"),
                  span("Overview of Class: Total number of students, dropouts, final grades, and normalized module usage",
                       style = "font-size:20px")),
                p(
                  span("Student Selector: Filters to customize view of dataset", style = "font-size:20px")
                ),
                p(
                  span("Effort Level: Information on Weekly Effort Levels", style = "font-size:20px")
                ),
                br(),
                p(
                  span("Dropout Definition provided by", style = "font-size:20px"),
                  a(href = "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C39&q=halawa+stanford&btnG=",
                    "Halawa, S., Greene, D., & Mitchell, J. (2014). Dropout Prediction in MOOCs using learner activity features. eLearning Papers, 37, 7-16.", style = "font-size:20px")
                ),
                br(),
                p(
                  h4("Why You Should Use This", style = "font-size:25px"),
                  span("According to", style = "font-size:20px"),
                  a(href = "https://www.edweek.org/ew/articles/2016/01/13/data-dashboards-a-high-priority-in-national.html", "Education Week,", style = "font-size:20px"),
                  span("instead of manually managing goal-setting and tracking, 
                       teachers can rely on the data dashboard to do that part. 
                       It has the promise to be a", em("wonderful enabler."), style = "font-size:20px")
                  ),
                br(),
                p(
                  h6("This work is funded by a NSF REU Site on Educational Data Mining at George Mason University; Grant No. 1757064",
                     style = "font-size:13px")
                ), width = 12))),
    tabItem(tabName = "basic",
            fluidRow(
              column(width = 6,
                     infoBoxOutput("students", width = NULL),
                     tabBox(title = "Info on Final Grades",
                            tabPanel("Final Grades Distribution",
                                     div(id = "loading-content-final-grades",
                                         class = "loading-content-final-grades",
                                         h2(class = "animated infinite pulse", "Loading Plot...")),
                                     plotOutput("final_grade_distribution")),
                            tabPanel("Final Grades Data table", dataTableOutput("final_grades")),
                            width = NULL, side = "right")),
              column(width = 6,
                     infoBoxOutput("dropouts_prop", width = NULL),
                     box(title = "Distribution of Normalized Module Usage for Each Student",
                         div(id = "loading-content-module-usage",
                             class = "loading-content-module-usage",
                             h2(class = "animated infinite pulse", "Loading Plot...")),
                         plotOutput("type_module"), width = NULL)
              )
            )),
    tabItem(tabName = "student_selector",
            h2("Choose Filters to Customize Student Query"),
            fluidRow(
              box(
                selectInput("choice_table", "Choose Table:",
                            c("Effort" = "weekly_effort",
                              "Events" = "event_xtract",
                              "Homework Assignments" = "activity_grade",
                              "Video" = "video_int"),
                            selected = "weekly_effort"
                ),
                
                conditionalPanel(
                  condition = "input.choice_table == `weekly_effort`",
                  radioButtons("range_effort",
                               label = "Range of Time Spent on Course",
                               choices = list("<=1hr" = 1,
                                              "1hr-2hrs" = 2,
                                              "2hrs-3hrs" = 3,
                                              ">3hrs" = 4)),
                  selectInput("weekly_effort_column", 
                              "Select columns to display",
                              choices = names(weekly_effort), multiple = TRUE,
                              selected = "anon_screen_name"),
                  checkboxInput("check_effort",
                                label = "Don't show Students with Minimal Effort",
                                value = TRUE)
                  
                ),
                
                conditionalPanel(
                  condition = "input.choice_table == `event_xtract`",
                  selectInput("event_type_choice",
                              label = "Choose Event Type(s)",
                              choices = event_type_short, selected = "about",
                              multiple = T),
                  
                  selectInput("event_xtract_column", 
                              "Select columns to display", 
                              names(event_xtract), multiple = TRUE, selected = "anon_screen_name")
                ),
                conditionalPanel(
                  condition = "input.choice_table == `activity_grade`",
                  selectInput("resource_type",
                              label = "Resource",
                              choices = unique(activity_grade$resource_display_name),
                              multiple = TRUE
                  ),
                  selectInput("range_percent_grade",
                              label = "Range of Assignment Percentage Grade",
                              choices = list("0%" = 1,
                                             "0% ~ 20%" = 2,
                                             "20% ~ 40%" = 3,
                                             "40% ~ 60%" = 4,
                                             "60% ~ 80%" = 5,
                                             "80% ~ 100%" = 6,
                                             "100%" = 7), multiple = T, selected = 1),
                  selectInput("activity_grade_column", 
                              "Select columns to display", 
                              names(activity_grade), multiple = TRUE, selected = "anon_screen_name"),
                  checkboxInput("check_assignment",
                                label = "Don't show Students who received a Zero Grade",
                                value = TRUE)
                  
                ),
                conditionalPanel(
                  condition = "input.choice_table == `video_int`",
                  selectInput("video_event_type_choice",
                              label = "Choose Types of Video Interaction(s)",
                              choices = video_int_type, multiple = T, selected = "load_video"),
                  
                  selectInput("video_int_column", 
                              "Select columns to display", 
                              names(video_int), multiple = TRUE, selected = "anon_screen_name")
                  
                ), width = 2, 
                height = 400),
              box(
                div(id = "loading-content-data-table",
                    class = "loading-content-data-table",
                    h2(class = "animated infinite pulse", "Loading Customizable Data Table...")),
                DT::dataTableOutput("student_table"), width = 10 
              )
            )
    ),
    tabItem(tabName = "effort_level",
            shinyjs::useShinyjs(),
            fluidRow(
              column(width = 6,
                     tabBox(
                       tabPanel("Effort Level by Completion of Course",
                                div(id = "loading-content-3",
                                    class = "loading-content-3",
                                    h2(class = "animated infinite pulse", "Loading Plot...")),
                                plotOutput("completion_effort")),
                       tabPanel("Weekly Effort Level",
                                div(id = "loading-content-4",
                                    class = "loading-content-4",
                                    h2(class = "animated infinite pulse", "Loading Plot...")),
                                
                                plotOutput("effort_level")),
                       width = NULL, side = "left")),
              column(width = 6,
                     tabBox(
                       tabPanel("Dropouts Effort Level",
                                h4("Dropouts = Students who watched less than 50% of Lecture Videos"),
                                div(id = "loading-content",
                                    class = "loading-content",
                                    h2(class = "animated infinite pulse", "Loading Plot...")),
                                plotlyOutput("dropout_effort_plotly")),
                       tabPanel("Non-Dropouts Effort Level",
                                h4("Non-Dropouts = Students who watched at least 50% of Lecture Videos"),
                                div(id = "loading-content-2",
                                    class = "loading-content-2",
                                    h2(class = "animated infinite pulse", "Loading Plot...")),
                                plotlyOutput("non_dropout_effort_plotly")),
                       width = NULL, side = "left"))
            )
    ),
    tabItem(tabName = "clustering",
            title = "Results of K-Means Clustering",
            tabBox(
              tabPanel("Proportion of Dropouts",
                       h2("Distribution of Dropout Proportions among Six clusters in Three Time Periods"),
                       div(id = "loading-content-dropout-prop",
                           class = "loading-content-dropout-prop",
                           h2(class = "animated infinite pulse", "Loading Plot...")),
                       plotOutput("dropout_prop"),
                       "The bar chart shows that three groups 
                       contain an extremely high proportion of dropout students (at least 92%).
                       As a result, we conclude that K-Means clustering algorithm succeeded in detecting
                       the one cluster in each time period that contains dropout students. This signifies that our
                       method of K-Means Clustering with two features, effort level and number of times student pressed 
                       “Play” on video, categorizes dropout students with fairly high accuracy"
              ),
              tabPanel("Effort and Video Level for Weeks 1~3",
                       h2("Distribution of Effort Level and Video Level for First Time Period"),
                       plotlyOutput("first_period"),
                       "We used the two cluster of students, dropouts and non-dropouts, to observe discrepancies in effort and video levels. The above three graphs reveal that for non-dropouts, the effort and video levels tail off after Week 2. For dropouts, effort levels drop off again after Week 2, but video levels show a decreasing trend for the entire period"),
              tabPanel("Effort and Video Level for Weeks 1~6",
                       h2("Distribution of Effort Level and Video Level for Second Time Period"),
                       plotlyOutput("second_period"),
                       "We used the two cluster of students, dropouts and non-dropouts, to observe discrepancies in effort and video levels. The above three graphs reveal that for non-dropouts, the effort and video levels tail off after Week 2. For dropouts, effort levels drop off again after Week 2, but video levels show a decreasing trend for the entire period"),
              tabPanel("Effort and Video Level for Weeks 1~10",
                       h2("Distribution of Effort Level and Video Level for Third Time Period"),
                       plotlyOutput("third_period"),
                       "We used the two cluster of students, dropouts and non-dropouts, to observe discrepancies in effort and video levels. The above three graphs reveal that for non-dropouts, the effort and video levels tail off after Week 2. For dropouts, effort levels drop off again after Week 2, but video levels show a decreasing trend for the entire period"),
              height = 12,
              width = 12
              
              )
            
            
    )
            )
  )  



ui <- dashboardPage(skin = "yellow", header = header,
                    sidebar = sidebar,
                    body = body)

server <- function(input, output, session) {
  
  val <- reactiveVal(0)
  val_dropout <- reactiveVal(0)
  
  output$students <- renderInfoBox({
    infoBox(
      "Number of Students",
      val(),
      icon = icon("user-graduate"), color = "yellow"
    )})
  observe({
    invalidateLater(0.1, session)
    isolate({
      # It will count till num_students
      if(val() < num_students) {
        newVal <- val()+23
        val(newVal)
      }
    })
  })
  
  output$dropouts_prop <- renderInfoBox({
    infoBox(
      "Dropout Rate",
      "65%",
      icon = icon("exclamation"), color = "yellow"
    )
  })
  
  reactive_final_grade <- reactiveFileReader(
    intervalMillis = 1000,
    session = session,
    filePath = "final_grades.csv",
    readFunc = function(filePath) { 
      read.csv(filePath) %>% 
        mutate(final_grade = final_grade * 100,
               anon_id = student_id_pretty) %>%
        arrange(final_grade) %>% 
        dplyr::rename(`Final Grade` = final_grade,
                      `Student ID` = anon_id) %>% 
        mutate_if(is.numeric, funs(round(., 1)))
    }
  )
  
  output$final_grades <- renderDataTable({
    final_grade_data_table <- reactive_final_grade()
    DT::datatable(data = final_grade_data_table,
                  rownames = FALSE,
                  colnames = c("Student ID", "Final Grade (%)"))
  })
  
  output$final_grade_distribution <- renderPlot({
    
    shinyjs::show("loading-content-final-grades") # make the loading pane appear
    
    final_grade_plot <- final_grade %>% 
      ggplot(aes(x = final_grade)) +
      geom_histogram(binwidth = 0.03, fill = "brown") +
      geom_vline(xintercept=0.60, color = "orange", size = 1.3) +
      geom_vline(xintercept=0.90, color = "orange", size = 1.3) +
      scale_x_continuous(breaks=c(0.3,0.6, 0.9),
                         labels = c("30%", "60%", "90%")) +
      labs(x = "Final Grade",
           y = "Counts") +
      ggtitle("Distribution of Final Grades", subtitle = "Statement of Accomplishment: >60%\nStatement with Distinction: >90%") +
      theme_bw() +
      theme(text = element_text(size = 15))
    
    shinyjs::hide("loading-content-final-grades") # make the loading pane disappear
    
    final_grade_plot
    
    
  })
  
  output$student_table <- renderDataTable({
    
    shinyjs::show("loading-content-data-table")
    
    if (input$choice_table == "weekly_effort") {
      student_sample <- reactive({
        req(input$range_effort)
        if (input$check_effort == TRUE) {
          weekly_effort %>%
            mutate(anon_screen_name = student_id_pretty_weekly) %>% 
            filter(effort_range == input$range_effort,
                   effort_hr > 0.5) %>% 
            dplyr::select(input$weekly_effort_column) 
        } else {
          
          weekly_effort %>%
            mutate(anon_screen_name = student_id_pretty_weekly) %>% 
            filter(effort_range == input$range_effort) %>% 
            dplyr::select(input$weekly_effort_column) 
        }
      })
    } else if (input$choice_table == "event_xtract") {
      student_sample <- reactive({
        req(input$event_type_choice)
        event_xtract %>% 
          mutate(anon_screen_name = student_id_pretty_event) %>% 
          filter(event_type %in% input$event_type_choice) %>% 
          dplyr::select(input$event_xtract_column)
      })
    } else if (input$choice_table == "activity_grade") {
      student_sample <- reactive({
        req(input$range_percent_grade)
        req(input$resource_type)
        if (input$check_assignment == TRUE) {
          activity_grade %>%
            mutate(anon_screen_name = student_id_pretty_activity) %>% 
            filter(percent_grade_range %in% input$range_percent_grade,
                   resource_display_name %in% input$resource_type,
                   grade > 0) %>% 
            dplyr::select(input$activity_grade_column)
        } else {
          
          activity_grade %>%
            mutate(anon_screen_name = student_id_pretty_activity) %>% 
            filter(percent_grade_range %in% input$range_percent_grade,
                   resource_display_name %in% input$resource_type) %>% 
            dplyr::select(input$activity_grade_column)
          
        }
      })
    } else  {
      student_sample <- reactive({
        req(input$video_event_type_choice)
        video_int %>%
          mutate(anon_screen_name = student_id_pretty_video) %>% 
          filter(event_type %in% input$video_event_type_choice) %>% 
          dplyr::select(input$video_int_column)
      })
    }
    
    shinyjs::hide("loading-content-data-table")
    
    DT::datatable(data = student_sample(),
                  rownames = FALSE)
  })
  
  output$type_module <- renderPlot({
    
    shinyjs::show("loading-content-module-usage") # make the loading pane appear
    
    course <- activity_grade %>% 
      filter(str_detect(module_id, "course")) %>% 
      add_count(module_id) %>% 
      summarise(norm_module = (sum(unique(n)) / length(unique(module_id))) / (length(unique(anon_screen_name)))) %>% mutate(module_type = "course")
    
    seq <- activity_grade %>% 
      filter(str_detect(module_id, "sequential")) %>% 
      add_count(module_id) %>% 
      summarise(norm_module = (sum(unique(n)) / length(unique(module_id))) / (length(unique(anon_screen_name)))) %>% mutate(module_type = "sequential")
    
    prob <- activity_grade %>% 
      filter(str_detect(module_id, "problem")) %>% 
      add_count(module_id) %>% 
      summarise(norm_module = (sum(unique(n)) / length(unique(module_id))) / (length(unique(anon_screen_name)))) %>% mutate(module_type = "problem")
    
    vid <- activity_grade %>% 
      filter(str_detect(module_id, "video")) %>% 
      add_count(module_id) %>% 
      summarise(norm_module = (sum(unique(n)) / length(unique(module_id))) / (length(unique(anon_screen_name)))) %>% mutate(module_type = "video")
    
    chapter <- activity_grade %>% 
      filter(str_detect(module_id, "chapter")) %>% 
      add_count(module_id) %>% 
      summarise(norm_module = (sum(unique(n)) / length(unique(module_id))) / (length(unique(anon_screen_name)))) %>% mutate(module_type = "chapter")
    
    final_module <- rbind(course, seq, prob, vid, chapter)
    
    final_module_plot <- final_module %>%
      filter(module_type != "course") %>% 
      ggplot(aes(x = module_type, y = norm_module, fill = module_type)) +
      geom_col() +
      theme_hc() +
      guides(fill = FALSE) +
      theme(text = element_text(size = 15)) +
      labs(x = "Module Types",
           y = "Normalized Counts of Modules per Student",
           title = "") +
      scale_x_discrete(limits = c("problem", "chapter", "sequential", "video"),
                       labels = c("Problem", "Chapter", "Sequential", "Video"))
    
    shinyjs::hide("loading-content-module-usage") # make the loading pane appear
    
    final_module_plot
    
    
  })
  output$completion_effort <- renderPlot({
    
    shinyjs::show("loading-content-3") # make the loading pane appear
    
    ce <- event_xtract %>%
      mutate(course_complete = ifelse(anon_screen_name %in% total_dropout, "no",
                                      "yes")) %>%
      dplyr::select(anon_screen_name, course_complete) %>%
      inner_join(weekly_effort, by = "anon_screen_name") %>%
      group_by(week, course_complete, anon_screen_name) %>%
      summarise(mean_effort_hrs = mean(effort_sec) / 3600) %>%
      filter(week < 10) %>% 
      ggplot(aes(x = as.factor(week), y = mean_effort_hrs, fill = course_complete)) +
      geom_boxplot(outlier.alpha = 0.35) +
      scale_fill_hue(labels = c("Not Completed", "Completed")) +
      labs(x = "Weeks",
           y = "Average Effort (Hr)",
           fill = "Completion of Course") +
      ggtitle("Distribution of Average Effort By Completion of Course",
              subtitle = "Students who completed the course put in more effort(time)") +
      theme_light() +
      theme(text = element_text(size = 15)) 
    
    shinyjs::hide("loading-content-3") # Make the loading content disappear
    
    ce
    
  })
  
  output$dropout_effort_plotly <- renderPlotly({
    
    shinyjs::show("loading-content") # make the loading pane appear
    
    # Line graph over 9 weeks of Effort Level FOR DROPOUTS
    drop_line <- weekly_effort %>% 
      filter(anon_screen_name %in% total_dropout) %>% 
      crosstalk::SharedData$new(~anon_screen_name) %>%
      ggplot(aes(x = as.factor(week), y = effort_sec, group = anon_screen_name,
                 text = paste(effort_sec, "seconds in Week", week, "by\n", anon_screen_name))) +
      geom_line(alpha = 0.035) +
      scale_y_log10() +
      labs(x = "Weeks",
           y = "Log Transformed Effort Level (Sec)") +
      theme_light()
    
    drop_line_plotly <- highlight(ggplotly(drop_line,
                                           tooltip = c("text")), dynamic = T, persistent = T, selectize = T)
    
    shinyjs::hide("loading-content") # Make the loading content disappear
    
    drop_line_plotly
  })
  
  output$non_dropout_effort_plotly <- renderPlotly({
    
    shinyjs::show("loading-content-2")
    
    # Line graph over 9 weeks of Effort Level FOR NON-DROPOUTS
    non_drop_line <- weekly_effort %>% 
      filter(!(anon_screen_name %in% total_dropout)) %>% 
      crosstalk::SharedData$new(~anon_screen_name) %>%
      ggplot(aes(x = as.factor(week), y = effort_sec, group = anon_screen_name,
                 text = paste(effort_sec, "seconds in Week", week, "by\n", anon_screen_name))) +
      geom_line(alpha = 0.035) +
      scale_y_log10() +
      labs(x = "Weeks",
           y = "Log Transformed Effort Level (Sec)") +
      theme_light()
    
    non_drop_plotly <- highlight(ggplotly(non_drop_line,
                                          tooltip = c("text")), dynamic = T, persistent = T, selectize = T)
    
    shinyjs::hide("loading-content-2")
    
    non_drop_plotly
  })
  
  output$effort_level <- renderPlot({
    # Get min, q1, median, q3, and maximum value for effort_level
    summary_effort <- summary(weekly_effort$effort_sec)
    
    shinyjs::show("loading-content-4")
    
    
    # Plot
    el <- weekly_effort %>% 
      mutate(effort_level = ifelse(effort_sec < summary_effort[2] & effort_sec >= summary_effort[1], "low", ifelse(effort_sec <= summary_effort[5] & 
                                                                                                                     effort_sec >= summary_effort[2], "med",
                                                                                                                   ifelse(effort_sec > summary_effort[5] & effort_sec <= summary_effort[6], "high", "na")))) %>%
      filter(week != 11) %>% 
      count(week, effort_level) %>%
      ggplot(aes(x = as.factor(week), y = n, fill = effort_level)) +
      geom_col(position = "fill") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_hue(labels = c("High", "Low", "Medium")) +
      labs(x = "Week",
           y = "Percentage",
           fill = "Effort Level") +
      ggtitle("Distribution of Effort Level per Week",
              subtitle = "Proportion of High Effort Level increases with time") +
      theme_light() +
      theme(text = element_text(size = 15))
    
    shinyjs::hide("loading-content-4")
    
    el
    
  })
  
  output$dropout_prop <- renderPlot({
    
    shinyjs::show("loading-content-dropout-prop")
    
    bar_first <- new_clust_first_kmeans %>% 
      mutate(cluster = as.character(cluster)) %>% 
      group_by(cluster) %>% 
      summarise(dropout_prop = mean(anon_screen_name %in% total_dropout)) %>% 
      mutate(group = "first")
    
    # Second Group
    bar_second <- new_clust_second_kmeans %>% 
      mutate(cluster = as.character(cluster)) %>% 
      group_by(cluster) %>% 
      summarise(dropout_prop = mean(anon_screen_name %in% total_dropout)) %>% 
      mutate(group = "second")
    
    # Third Group
    bar_third <- new_clust_third_kmeans %>% 
      mutate(cluster = as.character(cluster)) %>% 
      group_by(cluster) %>% 
      summarise(dropout_prop = mean(anon_screen_name %in% total_dropout)) %>% 
      mutate(group = "third")
    
    # Combine three groups
    bar_total <- rbind(bar_first, bar_second, bar_third)
    
    # Facet labeller
    three_group <- list(
      "first" = "Weeks 1~3",
      "second" = "Weeks 1~6",
      "third" = "Weeks 1~10"
    )
    
    three_group_labeller <- function(variable,value){
      return(three_group[value])
    }
    
    # Draw Barchart
    dropout_prop_barchart <- bar_total %>%
      mutate(dropout_prop = round(dropout_prop, 2)) %>% 
      ggplot(aes(x = cluster, y = dropout_prop)) +
      geom_col(aes(fill = cluster)) +
      geom_text(aes(label = dropout_prop), position = position_stack(vjust = 0.5),
                color = "black") +
      facet_wrap(~group, labeller = three_group_labeller) +
      #scale_fill_viridis(name = "Value") +
      labs(x = "Clusters",
           y = "Dropout Proportions",
           fill = "Clusters") +
      theme_tufte() +
      theme(text = element_text(size = 20))
    
    shinyjs::hide("loading-content-dropout-prop")
    
    dropout_prop_barchart
    
  })
  
  
  output$first_period <- renderPlotly({
    
    # Effort / Video number of seconds for Dropouts vs Non-Dropouts (Group 1)
    first_period_plotly <- new_clust_first_kmeans %>% 
      mutate(cluster = as.character(cluster)) %>% 
      mutate(is_dropout = if_else(cluster == "6", "Dropout", "Non-Dropout"),
             is_dropout = factor(is_dropout, levels = c("Non-Dropout", "Dropout"))) %>% 
      dplyr::select(-anon_screen_name, -cluster) %>% 
      group_by(is_dropout) %>% 
      summarise_all(mean, na.rm = TRUE) %>% 
      gather(week_1_effort:week_3_video, key = "metric", value = avg_sec) %>%
      mutate(week = str_extract(metric, "\\-*\\d+\\.*\\d*"),
             video_effort = str_sub(metric, start = 8),
             video_effort = factor(video_effort, labels = c("Effort Level in Hours",
                                                            "Video Level in Hours"))) %>%
      mutate(week = as.integer(week)) %>%
      mutate(avg_hr = avg_sec / 3600) %>%
      mutate(week = as.character(week),
             week = factor(paste("Week", week, sep = " "),
                           levels = c("Week 1",
                                      "Week 2",
                                      "Week 3"
                           ))) %>% 
      ggplot(aes(x = week, y = avg_hr, text = paste(round(avg_hr, 2), "Hours"))) +
      geom_line(aes(group = is_dropout, col = is_dropout)) +
      facet_wrap(~video_effort, scales = "free_y") +
      labs(x = NULL, y = NULL,
           col = "") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme_bw()
    
    ggplotly(first_period_plotly, tooltip = "text")
  })
  
  
  output$second_period <- renderPlotly({
    
    # Effort / Video number of seconds for Dropouts vs Non-Dropouts (Group 2)
    second_period_plotly <- new_clust_second_kmeans %>% 
      mutate(cluster = as.character(cluster)) %>% 
      mutate(is_dropout = if_else(cluster == "3", "Dropout", "Non-Dropout"),
             is_dropout = factor(is_dropout, levels = c("Non-Dropout", "Dropout"))) %>% 
      dplyr::select(-anon_screen_name, -cluster) %>% 
      group_by(is_dropout) %>% 
      summarise_all(mean, na.rm = TRUE) %>% 
      gather(week_1_effort:week_6_video, key = "metric", value = avg_sec) %>%
      mutate(week = str_extract(metric, "\\-*\\d+\\.*\\d*"),
             video_effort = str_sub(metric, start = 8),
             video_effort = factor(video_effort, labels = c("Effort Level in Hours",
                                                            "Video Level in Hours"))) %>%
      mutate(week = as.integer(week)) %>%
      mutate(avg_hr = avg_sec / 3600) %>%
      mutate(week = as.character(week),
             week = factor(paste("Week", week, sep = " "),
                           levels = c("Week 1",
                                      "Week 2",
                                      "Week 3",
                                      "Week 4",
                                      "Week 5",
                                      "Week 6"))) %>% 
      ggplot(aes(x = week, y = avg_hr, text = paste(round(avg_hr, 2), "Hours"))) +
      geom_line(aes(group = is_dropout, col = is_dropout)) +
      facet_wrap(~video_effort, scales = "free_y") +
      labs(x = NULL,
           y = NULL,
           col = "") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme_bw()
    
    ggplotly(second_period_plotly, tooltip = "text")
    
  })
  
  
  output$third_period <- renderPlotly({
    
    # Effort / Video number of seconds for Dropouts vs Non-Dropouts (Group 3)
    third_period_plotly <- new_clust_third_kmeans %>% 
      mutate(cluster = as.character(cluster)) %>% 
      mutate(is_dropout = if_else(cluster == "4", "Dropout", "Non-Dropout"),
             is_dropout = factor(is_dropout, levels = c("Non-Dropout", "Dropout"))) %>% 
      dplyr::select(-anon_screen_name, -cluster) %>% 
      group_by(is_dropout) %>% 
      summarise_all(mean, na.rm = TRUE) %>% 
      gather(week_1_effort:week_10_video, key = "metric", value = avg_sec) %>%
      mutate(week = str_extract(metric, "\\-*\\d+\\.*\\d*"),
             video_effort = str_sub(metric, start = 8)) %>%
      mutate(week = as.integer(week)) %>%
      mutate(avg_hr = avg_sec / 3600) %>% 
      mutate(video_effort = str_replace(video_effort, "_", ""),
             video_effort = factor(video_effort, labels = c("Effort Level in Hours",
                                                            "Video Level in Hours"))) %>% 
      mutate(week = as.character(week),
             week = factor(paste("Week", week, sep = " "),
                           labels = c("Wk1",
                                      "Wk2",
                                      "Wk3",
                                      "Wk4",
                                      "Wk5",
                                      "Wk6",
                                      "Wk7",
                                      "Wk8",
                                      "Wk9",
                                      "Wk10"))) %>% 
      ggplot(aes(x = week, y = avg_hr, text = paste(round(avg_hr, 2), "Hours"))) +
      geom_line(aes(group = is_dropout, col = is_dropout)) +
      facet_wrap(~video_effort, scales = "free_y") +
      labs(x = NULL,
           y = NULL,
           col = "") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme_bw()
    
    ggplotly(third_period_plotly, tooltip = "text")
    
  })
  
}


shinyApp(ui, server)