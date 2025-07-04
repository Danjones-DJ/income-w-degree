library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(plotly)

# Load the data directly into the app
pure_degrees_data <- read_csv("https://raw.githubusercontent.com/Danjones-DJ/income-w-degree/main/pure_degree_sal.csv")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Pure Degrees & Salary Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Find Your Degree", tabName = "matcher", icon = icon("graduation-cap")),
      menuItem("Salary Analysis", tabName = "salary", icon = icon("pound-sign")),
      menuItem("Requirements Guide", tabName = "requirements", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          margin-bottom: 20px;
        }
        .salary-high { background-color: #d4edda !important; }
        .salary-medium { background-color: #fff3cd !important; }
        .salary-low { background-color: #f8d7da !important; }
        .grade-excellent { background-color: #d1ecf1 !important; }
        .grade-good { background-color: #d4edda !important; }
        .grade-challenging { background-color: #fff3cd !important; }
      "))
    ),
    
    tabItems(
      # Degree Matcher Tab
      tabItem(tabName = "matcher",
              fluidRow(
                box(
                  title = "Your Academic Profile", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 4,
                  
                  h4("Your A-Level Grades"),
                  p("Enter your achieved or predicted grades:"),
                  
                  fluidRow(
                    column(6, 
                           selectInput("grade1", "Grade 1:", 
                                       choices = c("A*", "A", "B", "C", "D", "E"), 
                                       selected = "A")
                    ),
                    column(6,
                           selectInput("grade2", "Grade 2:", 
                                       choices = c("A*", "A", "B", "C", "D", "E"), 
                                       selected = "A")
                    )
                  ),
                  
                  fluidRow(
                    column(6,
                           selectInput("grade3", "Grade 3:", 
                                       choices = c("A*", "A", "B", "C", "D", "E"), 
                                       selected = "A")
                    ),
                    column(6,
                           selectInput("grade4", "Grade 4 (Optional):", 
                                       choices = c("None", "A*", "A", "B", "C", "D", "E"), 
                                       selected = "None")
                    )
                  ),
                  
                  hr(),
                  
                  h4("Filter Options"),
                  
                  selectInput("degree_type_filter", "Degree Type:",
                              choices = c("All" = "All", 
                                          unique(pure_degrees_data$degree_type)),
                              selected = "All"),
                  
                  checkboxInput("year_abroad", "Include Year Abroad Options", FALSE),
                  checkboxInput("industry_year", "Include Industry Year Options", FALSE),
                  checkboxInput("foundation_year", "Include Foundation Year Options", FALSE),
                  
                  br(),
                  actionButton("findDegrees", "🔍 Find Matching Degrees", 
                               class = "btn-primary btn-lg btn-block")
                ),
                
                box(
                  title = "Matching Degrees", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 8,
                  
                  conditionalPanel(
                    condition = "input.findDegrees == 0",
                    div(
                      style = "text-align: center; padding: 50px;",
                      h3("Welcome to Pure Degrees Explorer!"),
                      p("Enter your A-Level grades to find degrees you qualify for, with salary information."),
                      p("💰 See potential earnings 15 months after graduation"),
                      p("🎓 Explore different degree types and requirements"),
                      icon("arrow-left", class = "fa-2x")
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "input.findDegrees > 0",
                    div(
                      h4("Your Grade Profile"),
                      verbatimTextOutput("gradeSummary"),
                      br(),
                      h4("Available Degrees"),
                      DT::dataTableOutput("degreesTable")
                    )
                  )
                )
              )
      ),
      
      # Salary Analysis Tab
      tabItem(tabName = "salary",
              fluidRow(
                box(
                  title = "Salary Analysis by Subject", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 12,
                  
                  fluidRow(
                    column(6,
                           selectInput("salary_subject", "Select Subject Area:",
                                       choices = c("All" = "All", 
                                                   sort(unique(pure_degrees_data$title))),
                                       selected = "All")
                    ),
                    column(6,
                           selectInput("salary_degree_type", "Degree Type:",
                                       choices = c("All" = "All", 
                                                   unique(pure_degrees_data$degree_type)),
                                       selected = "All")
                    )
                  ),
                  
                  plotlyOutput("salaryPlot", height = "400px"),
                  
                  br(),
                  h4("Salary Statistics"),
                  DT::dataTableOutput("salaryTable")
                )
              )
      ),
      
      # Requirements Guide Tab
      tabItem(tabName = "requirements",
              fluidRow(
                box(
                  title = "A-Level Requirements Guide", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 12,
                  
                  h4("Requirements by Grade Level"),
                  p("See what degrees are available at different grade levels:"),
                  
                  plotlyOutput("requirementsPlot", height = "400px"),
                  
                  br(),
                  h4("Subject Requirements Breakdown"),
                  DT::dataTableOutput("requirementsTable")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Helper functions
  grade_to_score <- function(grade) {
    case_when(
      grade == "A*" ~ 4,
      grade == "A" ~ 3,
      grade == "B" ~ 2,
      grade == "C" ~ 1,
      grade == "D" ~ 0.5,
      TRUE ~ 0
    )
  }
  
  calculate_requirement_score <- function(a_level_req) {
    # Extract grades from requirement string
    grades <- str_extract_all(a_level_req, "A\\*|[A-E]")[[1]]
    if(length(grades) == 0) return(0)
    sum(sapply(grades, grade_to_score))
  }
  
  calculate_student_score <- function(student_grades) {
    student_grades <- student_grades[student_grades != "None"]
    sum(sapply(student_grades, grade_to_score))
  }
  
  # Reactive data for degree matching
  matching_degrees <- eventReactive(input$findDegrees, {
    # Get student grades
    grades <- c(input$grade1, input$grade2, input$grade3)
    if (input$grade4 != "None") {
      grades <- c(grades, input$grade4)
    }
    
    student_score <- calculate_student_score(grades)
    
    # Filter data based on user preferences
    filtered_data <- pure_degrees_data
    
    if (input$degree_type_filter != "All") {
      filtered_data <- filtered_data %>% 
        filter(degree_type == input$degree_type_filter)
    }
    
    if (input$year_abroad) {
      filtered_data <- filtered_data %>% 
        filter(year_abroad_available == "Yes")
    }
    
    if (input$industry_year) {
      filtered_data <- filtered_data %>% 
        filter(industry_year_available == "Yes")
    }
    
    if (input$foundation_year) {
      filtered_data <- filtered_data %>% 
        filter(foundation_year_available == "Yes")
    }
    
    # Calculate which degrees student qualifies for
    result <- filtered_data %>%
      mutate(
        requirement_score = map_dbl(A_level, ~calculate_requirement_score(.x)),
        meets_requirements = student_score >= requirement_score,
        grade_excess = student_score - requirement_score
      ) %>%
      filter(meets_requirements) %>%
      arrange(desc(med_sal_15m), desc(grade_excess)) %>%
      mutate(
        match_quality = case_when(
          grade_excess >= 2 ~ "Exceeds Requirements",
          grade_excess >= 1 ~ "Comfortably Meets",
          TRUE ~ "Just Meets"
        ),
        salary_band = case_when(
          med_sal_15m >= 40000 ~ "High (£40k+)",
          med_sal_15m >= 30000 ~ "Medium (£30-40k)",
          med_sal_15m >= 20000 ~ "Lower (£20-30k)",
          TRUE ~ "Data Not Available"
        )
      )
    
    list(
      data = result,
      grades = grades,
      student_score = student_score
    )
  })
  
  # Grade summary output
  output$gradeSummary <- renderText({
    result <- matching_degrees()
    paste0(
      "Your grades: ", paste(result$grades, collapse = ", "), "\n",
      "Your grade score: ", result$student_score, "/", length(result$grades) * 4, "\n",
      "Degrees available: ", nrow(result$data), "\n",
      "Highest median salary: £", scales::comma(max(result$data$med_sal_15m, na.rm = TRUE))
    )
  })
  
  # Degrees table
  output$degreesTable <- DT::renderDataTable({
    result <- matching_degrees()
    
    if (nrow(result$data) == 0) {
      return(NULL)
    }
    
    display_data <- result$data %>%
      select(
        Subject = title,
        Type = degree_type,
        Required = A_level,
        `Match Quality` = match_quality,
        `Median Salary` = med_sal_15m,
        `Salary Range` = salary_band,
        `Year Abroad` = year_abroad_available,
        `Industry Year` = industry_year_available
      ) %>%
      mutate(
        `Median Salary` = ifelse(is.na(`Median Salary`), "N/A", 
                                 paste0("£", scales::comma(`Median Salary`)))
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(4, "desc"))
      )
    ) %>%
      DT::formatStyle(
        "Salary Range",
        backgroundColor = DT::styleEqual(
          c("High (£40k+)", "Medium (£30-40k)", "Lower (£20-30k)"),
          c("#d4edda", "#fff3cd", "#f8d7da")
        )
      ) %>%
      DT::formatStyle(
        "Match Quality",
        backgroundColor = DT::styleEqual(
          c("Exceeds Requirements", "Comfortably Meets", "Just Meets"),
          c("#d1ecf1", "#d4edda", "#fff3cd")
        )
      )
  })
  
  # Salary plot
  output$salaryPlot <- renderPlotly({
    salary_data <- pure_degrees_data %>%
      filter(!is.na(med_sal_15m))
    
    if (input$salary_subject != "All") {
      salary_data <- salary_data %>% filter(title == input$salary_subject)
    }
    
    if (input$salary_degree_type != "All") {
      salary_data <- salary_data %>% filter(degree_type == input$salary_degree_type)
    }
    
    p <- salary_data %>%
      ggplot(aes(x = reorder(title, med_sal_15m), y = med_sal_15m, fill = degree_type)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Median Salary by Subject (15 months after graduation)",
        x = "Subject",
        y = "Median Salary (£)",
        fill = "Degree Type"
      ) +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma_format(prefix = "£"))
    
    ggplotly(p)
  })
  
  # Salary table
  output$salaryTable <- DT::renderDataTable({
    salary_data <- pure_degrees_data %>%
      filter(!is.na(med_sal_15m))
    
    if (input$salary_subject != "All") {
      salary_data <- salary_data %>% filter(title == input$salary_subject)
    }
    
    if (input$salary_degree_type != "All") {
      salary_data <- salary_data %>% filter(degree_type == input$salary_degree_type)
    }
    
    salary_summary <- salary_data %>%
      group_by(title, degree_type) %>%
      summarise(
        `Median Salary` = paste0("£", scales::comma(median(med_sal_15m, na.rm = TRUE))),
        `Salary Range` = paste0("£", scales::comma(min(lq_sal_15m, na.rm = TRUE)), 
                                " - £", scales::comma(max(uq_sal_15m, na.rm = TRUE))),
        `Requirements` = paste(unique(A_level), collapse = ", "),
        .groups = "drop"
      ) %>%
      arrange(desc(parse_number(`Median Salary`)))
    
    DT::datatable(
      salary_summary,
      options = list(pageLength = 10, scrollX = TRUE),
      colnames = c("Subject", "Degree Type", "Median Salary", "Salary Range", "A-Level Requirements")
    )
  })
  
  # Requirements plot
  output$requirementsPlot <- renderPlotly({
    req_data <- pure_degrees_data %>%
      mutate(
        req_score = map_dbl(A_level, ~calculate_requirement_score(.x)),
        grade_category = case_when(
          req_score >= 11 ~ "A*A*A+ (11+ points)",
          req_score >= 9 ~ "AAA (9-10 points)",
          req_score >= 7 ~ "AAB (7-8 points)",
          req_score >= 5 ~ "ABB (5-6 points)",
          TRUE ~ "Below ABB (<5 points)"
        )
      ) %>%
      count(grade_category, degree_type) %>%
      mutate(grade_category = factor(grade_category, levels = c(
        "A*A*A+ (11+ points)", "AAA (9-10 points)", "AAB (7-8 points)", 
        "ABB (5-6 points)", "Below ABB (<5 points)"
      )))
    
    p <- req_data %>%
      ggplot(aes(x = grade_category, y = n, fill = degree_type)) +
      geom_col(position = "dodge") +
      labs(
        title = "Number of Degrees by Grade Requirements",
        x = "Grade Requirements",
        y = "Number of Degrees",
        fill = "Degree Type"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Requirements table
  output$requirementsTable <- DT::renderDataTable({
    req_summary <- pure_degrees_data %>%
      mutate(
        req_score = map_dbl(A_level, ~calculate_requirement_score(.x))
      ) %>%
      group_by(A_level) %>%
      summarise(
        `Number of Degrees` = n(),
        `Average Salary` = paste0("£", scales::comma(mean(med_sal_15m, na.rm = TRUE))),
        `Highest Salary` = paste0("£", scales::comma(max(med_sal_15m, na.rm = TRUE))),
        `Example Subjects` = paste(head(unique(title), 3), collapse = ", "),
        `Grade Score` = first(req_score),
        .groups = "drop"
      ) %>%
      arrange(desc(`Grade Score`))
    
    DT::datatable(
      req_summary,
      options = list(pageLength = 10, scrollX = TRUE),
      colnames = c("A-Level Requirement", "Number of Degrees", "Average Salary", 
                   "Highest Salary", "Example Subjects", "Grade Score")
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)


