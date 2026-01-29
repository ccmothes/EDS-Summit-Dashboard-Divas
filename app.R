library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboard)

# Define colorblind-friendly palette for Likert scales (5 levels)
# Using a blue-to-orange diverging palette that is colorblind-safe
likert_colors <- c(
  "#d73027",  # Red (Not at all important)
  "#fc8d59",  # Orange (Slightly important)
  "#fee090",  # Light yellow (Moderately important)
  "#91bfdb",  # Light blue (Very important)
  "#4575b4"   # Dark blue (Extremely important)
)

# Survey theme colors (matching the screenshot)
teal_green <- "#5DAEA3"     # Teal/turquoise from checkboxes
dark_teal <- "#4A958B"      # Darker shade for header
light_gray <- "#F5F5F5"     # Light background
medium_gray <- "#E8E8E8"    # Medium gray for boxes
accent_blue <- "#6BB8D1"    # Light blue accent

# Read data
df <- read_csv("survey_data/cleaned_data.csv")

# Extract metric names from Likert columns (32-50) - text after last " - "
likert_metrics <- sapply(32:50, function(i) {
  col_name <- colnames(df)[i]
  parts <- str_split(col_name, " - ")[[1]]
  parts[length(parts)]
})

# Create shortened versions for community engagement metrics (for display)
community_short_labels <- c(
  "Community capacity for data skills increases" = "Community data capacity",
  "Trust is built between communities and institutions" = "Trust building",
  "Publishing for a public audience (e.g., blog post, website)" = "Public publishing",
  "Communicating interim findings during the project (not just final publications)" = "Interim findings",
  "Active collaboration with community members throughout the project lifecycle" = "Active collaboration",
  "Traditional/local knowledge is integrated with scientific data" = "Traditional knowledge integration",
  "Community maintains ownership of data and findings" = "Community data ownership",
  "Collaborations continue beyond initial project" = "Sustained collaborations",
  "Partnerships are equitable, collaborative, empowering, and address social inequalities" = "Equitable partnerships",
  "The community has meaningful say and control over the research process" = "Community control",
  "Community members are compensated for their time and effort" = "Community compensation",
  "There is clear communication on how data and outcomes will be used" = "Clear communication",
  "Community members' viewpoints and perspectives are integrated into the project" = "Community viewpoints"
)

# Extract metric names from TOP 3 priorities columns (51-69) - text after last " - "
top3_metrics <- sapply(51:69, function(i) {
  col_name <- colnames(df)[i]
  parts <- str_split(col_name, " - ")[[1]]
  parts[length(parts)]
})

# Create primary role from columns 1-10 (whichever is marked)
# Keep ALL roles selected by each person, not just the first one
# Only include roles that have responses: Academic, Government, Industry, Non-Profit, Student
role_names <- c("Academic", "Government", "Industry", "Non-Profit", "Student")
role_indices <- c(1, 4, 6, 7, 8)  # Column indices for roles with responses

# Create a column for each role to track ALL selected roles per person
df <- df %>%
  mutate(
    has_academic = !is.na(df[[1]]),
    has_government = !is.na(df[[4]]),
    has_industry = !is.na(df[[6]]),
    has_nonprofit = !is.na(df[[7]]),
    has_student = !is.na(df[[8]])
  ) %>%
  # Also keep primary_role for display purposes (first selected role)
  mutate(primary_role = case_when(
    !is.na(df[[1]]) ~ "Academic",
    !is.na(df[[4]]) ~ "Government",
    !is.na(df[[6]]) ~ "Industry",
    !is.na(df[[7]]) ~ "Non-Profit",
    !is.na(df[[8]]) ~ "Student",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(primary_role))  # Remove rows with no role

# For the role plot, count ALL roles selected (not just primary)
all_roles_data <- df %>%
  select(all_of(role_indices)) %>%
  summarise(across(everything(), ~sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "role", values_to = "count") %>%
  mutate(role = role_names)

# Helper function to wrap long text labels into multiple lines
wrap_label <- function(text, max_chars = 40) {
  if(is.na(text)) return(text)
  if(nchar(text) <= max_chars) return(text)
  
  words <- str_split(text, " ")[[1]]
  lines <- character()
  current_line <- ""
  
  for(word in words) {
    test_line <- if(current_line == "") word else paste(current_line, word)
    if(nchar(test_line) <= max_chars) {
      current_line <- test_line
    } else {
      if(current_line != "") lines <- c(lines, current_line)
      current_line <- word
    }
  }
  
  if(current_line != "") lines <- c(lines, current_line)
  paste(lines, collapse = "<br>")
}

# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Community-Based Science Survey",
                  titleWidth = 450),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About You", tabName = "about", icon = icon("user")),
      menuItem("Defining Success", tabName = "success", icon = icon("bullseye")),
      menuItem("Effective Operations", tabName = "operations", icon = icon("cogs")),
      menuItem("Actionable Outcomes", tabName = "outcomes", icon = icon("chart-line"))
    ),
    br(),
    selectInput("role_filter", "Compare by Primary Role:",
                choices = c("All Roles" = "all", "Academic", "Government", "Industry", 
                            "Non-Profit", "Student"),
                selected = "all"),
    hr(),
    div(
      style = "padding: 0 15px; font-size: 13px; color: #666; line-height: 1.4;",
      p("These findings are the result of a pilot survey of EDS Summit participants aiming to gain initial insight on how different stakeholders define success and actionable outcomes for community-based projects.")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Custom survey-inspired theme with teal colors */
        .skin-blue .main-header .navbar {
          background-color: #5DAEA3;
        }
        .skin-blue .main-header .logo {
          background-color: #4A958B;
          color: #ffffff;
          font-weight: 500;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #4A958B;
        }
        .skin-blue .sidebar-menu > li.active > a {
          border-left-color: #5DAEA3;
          background-color: #5DAEA3;
        }
        .skin-blue .sidebar-menu > li:hover > a {
          border-left-color: #5DAEA3;
          background-color: #5DAEA3;
        }
        .box.box-solid.box-primary > .box-header {
          background: #5DAEA3;
          color: #5DAEA3;
        }
        .box.box-primary {
          border-top-color: #5DAEA3;
        }
        /* Clean, minimal styling */
        .box {
          box-shadow: 0 1px 2px rgba(0,0,0,0.08);
          border-radius: 4px;
        }
        .content-wrapper {
          background-color: #5DAEA3;
        }
        body {
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
        }
        .box-header {
          border-bottom: 1px solid #e8e8e8;
        }
      "))
    ),
    tabItems(
      # About You Tab
      tabItem(tabName = "about",
              fluidRow(
                box(title = "Primary Roles", width = 6, plotlyOutput("role_plot")),
                box(title = "Years of Involvement", width = 6, plotlyOutput("years_plot"))
              ),
              fluidRow(
                box(title = "Sectors Worked In", width = 6, plotlyOutput("sectors_plot")),
                box(title = "Geographic Scope", width = 6, plotlyOutput("geo_plot"))
              )
      ),
      
      # Defining Success Tab
      tabItem(tabName = "success",
              fluidRow(
                box(title = "Importance Ratings: Impact & Outcomes", width = 12, 
                    plotlyOutput("success_impact_plot", height = "400px"))
              ),
              fluidRow(
                box(title = "Importance Ratings: Community Engagement", width = 12,
                    plotlyOutput("success_community_plot", height = "400px"))
              ),
              fluidRow(
                box(title = "Top 3 Priorities by Role", width = 12,
                    plotlyOutput("top_indicators_plot", height = "500px"))
              )
      ),
      
      # Effective Operations Tab
      tabItem(tabName = "operations",
              fluidRow(
                box(title = "Communication Frequency Methods", width = 4,
                    plotlyOutput("comm_freq_plot", height = "300px")),
                box(title = "Data Sharing Methods", width = 8,
                    plotlyOutput("data_sharing_plot", height = "300px"))
              ),
              fluidRow(
                box(title = "Project Duration", width = 4,
                    plotlyOutput("duration_plot", height = "300px")),
                box(title = "Key Stakeholders Involved", width = 8,
                    plotlyOutput("stakeholders_plot", height = "300px"))
              )
      ),
      
      # Actionable Outcomes Tab
      tabItem(tabName = "outcomes",
              fluidRow(
                box(title = "Confidence in Project Sustainability", width = 4,
                    plotlyOutput("confidence_plot", height = "400px")),
                box(title = "Common Challenges Faced", width = 8,
                    plotlyOutput("challenges_plot", height = "400px"))
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- df  # Get the full dataset
    
    if (input$role_filter == "all") {
      return(data)
    } else {
      # Filter based on which role is selected
      filtered <- data %>% filter(
        case_when(
          input$role_filter == "Academic" ~ has_academic,
          input$role_filter == "Government" ~ has_government,
          input$role_filter == "Industry" ~ has_industry,
          input$role_filter == "Non-Profit" ~ has_nonprofit,
          input$role_filter == "Student" ~ has_student,
          TRUE ~ FALSE
        )
      )
      return(filtered)
    }
  })
  
  # About You Plots
  output$role_plot <- renderPlotly({
    # Use all_roles_data which counts ALL selected roles, not just primary
    plot_ly(all_roles_data, x = ~reorder(role, count), y = ~count, type = "bar",
            marker = list(color = teal_green)) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Count"),
             margin = list(b = 100),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$years_plot <- renderPlotly({
    years_col <- colnames(df)[12]
    
    years_data <- filtered_data() %>%
      filter(!is.na(.data[[years_col]])) %>%
      count(.data[[years_col]]) %>%
      rename(years = 1, n = 2)
    
    # 1. Define the number of colors needed
    num_colors <- nrow(years_data)
    
    # 2. Generate the "Paired" palette 
    # Using colorRampPalette ensures you get enough colors even if you have >12 categories
    pal <- colorRampPalette(RColorBrewer::brewer.pal(min(num_colors, 12), "Paired"))(num_colors)
    
    plot_ly(years_data, labels = ~years, values = ~n, type = "pie",
            textposition = "inside", 
            textinfo = "label+percent",
            marker = list(colors = pal)) %>% # 3. Apply the colors here
      layout(showlegend = FALSE)
  })
  
  output$sectors_plot <- renderPlotly({
    # Columns 13-24 are sector checkboxes
    sector_cols <- 13:24
    sector_data <- filtered_data() %>%
      select(all_of(sector_cols)) %>%
      summarise(across(everything(), ~sum(!is.na(.)))) %>%
      pivot_longer(everything(), names_to = "sector", values_to = "count") %>%
      mutate(sector = str_extract(sector, "[^-]+$") %>% str_trim()) %>%
      arrange(desc(count)) %>%
      head(10)
    
    plot_ly(sector_data, y = ~reorder(sector, count), x = ~count, type = "bar",
            orientation = "h", marker = list(color = teal_green)) %>%
      layout(yaxis = list(title = ""), xaxis = list(title = "Count"),
             margin = list(l = 300),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$geo_plot <- renderPlotly({
    # Columns 26-29 are geographic scope checkboxes
    geo_cols <- 26:29
    geo_names <- c("Local", "Regional", "National", "International")
    
    geo_data <- filtered_data() %>%
      select(all_of(geo_cols)) %>%
      summarise(across(everything(), ~sum(!is.na(.)))) %>%
      pivot_longer(everything(), names_to = "scope", values_to = "count") %>%
      mutate(scope = geo_names) %>%
      arrange(desc(count))
    
    plot_ly(geo_data, x = ~reorder(scope, count), y = ~count, type = "bar",
            marker = list(color = accent_blue)) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Count"),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  # Defining Success Plots
  output$success_impact_plot <- renderPlotly({
    # Columns 32, 33, 36, 37, 38, 39 for impact metrics
    impact_indices <- c(32, 33, 36, 37, 38, 39)
    impact_names <- likert_metrics[impact_indices - 31]
    
    impact_data <- filtered_data() %>%
      select(all_of(impact_indices)) %>%
      setNames(impact_names) %>%
      pivot_longer(everything(), names_to = "metric", values_to = "rating") %>%
      filter(!is.na(rating)) %>%
      mutate(
        metric_wrapped = sapply(metric, wrap_label, max_chars = 35),
        metric_wrapped = factor(metric_wrapped, levels = sapply(impact_names, wrap_label, max_chars = 35))
      ) %>%
      count(metric_wrapped, rating) %>%
      mutate(rating = factor(rating, levels = c("Not at all important", "Slightly important",
                                                "Moderately important", "Very important",
                                                "Extremely important")))
    
    plot_ly(impact_data, x = ~metric_wrapped, y = ~n, color = ~rating, type = "bar",
            colors = likert_colors) %>%
      layout(barmode = "stack", 
             xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Count"), 
             legend = list(
               title = list(text = "Importance"),
               orientation = "v",
               x = 1.02,
               xanchor = "left",
               y = 0.5,
               yanchor = "middle"
             ),
             margin = list(l = 60, r = 200, b = 180, t = 20),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$success_community_plot <- renderPlotly({
    # Community engagement columns: 34, 35, 40-50
    community_indices <- c(34, 35, 40:50)
    community_names <- likert_metrics[community_indices - 31]
    
    community_data <- filtered_data() %>%
      select(all_of(community_indices)) %>%
      setNames(community_names) %>%
      pivot_longer(everything(), names_to = "metric", values_to = "rating") %>%
      filter(!is.na(rating)) %>%
      mutate(
        # Use shortened labels if available, otherwise use original
        metric_display = ifelse(metric %in% names(community_short_labels),
                                community_short_labels[metric],
                                metric),
        metric_display = factor(metric_display, 
                                levels = sapply(community_names, function(x) {
                                  ifelse(x %in% names(community_short_labels),
                                         community_short_labels[x], x)
                                }))
      ) %>%
      count(metric_display, rating) %>%
      mutate(rating = factor(rating, levels = c("Not at all important", "Slightly important",
                                                "Moderately important", "Very important",
                                                "Extremely important")))
    
    plot_ly(community_data, x = ~metric_display, y = ~n, color = ~rating, type = "bar",
            colors = likert_colors) %>%
      layout(barmode = "stack", 
             xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Count"), 
             legend = list(
               title = list(text = "Importance"),
               orientation = "v",
               x = 1.02,
               xanchor = "left",
               y = 0.5,
               yanchor = "middle"
             ),
             margin = list(l = 60, r = 200, b = 150, t = 20),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$top_indicators_plot <- renderPlotly({
    # Columns 51-69 are TOP 3 priorities checkboxes
    top3_indices <- 51:69
    
    # Get top 10 metrics overall
    top_metrics_data <- df %>%
      select(all_of(top3_indices)) %>%
      summarise(across(everything(), ~sum(!is.na(.)))) %>%
      pivot_longer(everything(), names_to = "indicator", values_to = "count") %>%
      mutate(indicator = top3_metrics) %>%
      arrange(desc(count)) %>%
      head(10)
    
    top_metrics <- top_metrics_data$indicator
    
    # Get counts by role for each top metric (only roles with responses)
    metric_role_counts <- list()
    for(i in 1:length(top_metrics)) {
      metric <- top_metrics[i]
      col_idx <- 51 + which(top3_metrics == metric) - 1
      
      # Count for each role (only the 5 roles with responses)
      role_counts <- data.frame(
        role = role_names,
        n = sapply(role_indices, function(j) {
          sum(!is.na(df[[col_idx]]) & !is.na(df[[j]]))
        }),
        indicator = metric
      )
      
      metric_role_counts[[i]] <- role_counts %>% filter(n > 0)
    }
    
    plot_data <- bind_rows(metric_role_counts) %>%
      mutate(indicator_wrapped = sapply(indicator, wrap_label, max_chars = 50))
    
    # Create stacked horizontal bar chart
    plot_ly(plot_data, y = ~reorder(indicator_wrapped, n, sum), x = ~n, 
            color = ~role, colors = "Paired", type = "bar", orientation = "h") %>%
      layout(
        barmode = "stack",
        yaxis = list(title = ""),
        xaxis = list(title = "Count"),
        legend = list(
          title = list(text = "Primary Role"),
          orientation = "v",
          x = 1.02,
          xanchor = "left",
          y = 0.5,
          yanchor = "middle"
        ),
        margin = list(l = 350, r = 200, t = 20, b = 40),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)'
      )
  })
  
  # Effective Operations Plots
  output$comm_freq_plot <- renderPlotly({
    # Columns 72-77 are communication preference checkboxes
    comm_indices <- 72:77
    comm_names <- c("Regular scheduled meetings", "Email updates", 
                    "In-person meetings", "Informal check-ins",
                    "Collaborative platforms", "Other")
    
    comm_data <- filtered_data() %>%
      select(all_of(comm_indices)) %>%
      summarise(across(everything(), ~sum(!is.na(.)))) %>%
      pivot_longer(everything(), names_to = "method", values_to = "count") %>%
      mutate(method = comm_names)
    

    plot_ly(comm_data, labels = ~method, values = ~count, type = "pie",
            textposition = "auto",
            marker = list(colors = c("#1f78b4", "#b2df8a", "#33a02c", "#95C9C5", "#CCCCCC"))) %>%
      layout(showlegend = FALSE,
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$data_sharing_plot <- renderPlotly({
    # Columns 79-87 are data access preference checkboxes
    data_indices <- 79:87
    
    data_share <- filtered_data() %>%
      select(all_of(data_indices)) %>%
      summarise(across(everything(), ~sum(!is.na(.)))) %>%
      pivot_longer(everything(), names_to = "method", values_to = "count") %>%
      mutate(
        method = str_extract(method, "[^-]+$") %>% str_trim(),
        method_wrapped = sapply(method, wrap_label, max_chars = 35)  # 2 rows
      ) %>%
      arrange(desc(count))
    
    plot_ly(data_share, y = ~reorder(method_wrapped, count), x = ~count,
            type = "bar", orientation = "h", marker = list(color = teal_green)) %>%
      layout(yaxis = list(title = ""), xaxis = list(title = "Count"),
             margin = list(l = 200, r = 20, t = 20, b = 40),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$duration_plot <- renderPlotly({
    duration_col <- colnames(df)[88]
    
    duration_data <- filtered_data() %>%
      filter(!is.na(.data[[duration_col]])) %>%
      count(.data[[duration_col]]) %>%
      rename(duration = 1, n = 2)
    
    plot_ly(duration_data, labels = ~duration, values = ~n, type = "pie",
            textposition = "inside", textinfo = "label+percent",
            marker = list(colors = c("#1f78b4", "#b2df8a", "#33a02c", "#95C9C5", "#CCCCCC"))) %>%
      layout(showlegend = FALSE,
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$stakeholders_plot <- renderPlotly({
    # Columns 89-100 are stakeholder type checkboxes
    stakeholder_indices <- 89:100
    
    stakeholder_data <- filtered_data() %>%
      select(all_of(stakeholder_indices)) %>%
      summarise(across(everything(), ~sum(!is.na(.)))) %>%
      pivot_longer(everything(), names_to = "stakeholder", values_to = "count") %>%
      mutate(
        stakeholder = str_extract(stakeholder, "[^-]+$") %>% str_trim(),
        stakeholder_wrapped = sapply(stakeholder, wrap_label, max_chars = 35)  # 2 rows
      ) %>%
      arrange(desc(count)) %>%
      head(10)
    
    plot_ly(stakeholder_data, y = ~reorder(stakeholder_wrapped, count), x = ~count,
            type = "bar", orientation = "h", marker = list(color = accent_blue)) %>%
      layout(yaxis = list(title = ""), xaxis = list(title = "Count"),
             margin = list(l = 200, r = 20, t = 20, b = 40),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  # Actionable Outcomes Plots
  output$confidence_plot <- renderPlotly({
    conf_col <- colnames(df)[102]
    
    conf_data <- filtered_data() %>%
      filter(!is.na(.data[[conf_col]])) %>%
      count(.data[[conf_col]]) %>%
      rename(confidence = 1, n = 2)
    
    plot_ly(conf_data, x = ~confidence, y = ~n, type = "bar",
            marker = list(color = accent_blue)) %>%
      layout(xaxis = list(title = "Confidence Level (0-5)"),
             yaxis = list(title = "Count"),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$challenges_plot <- renderPlotly({
    # Columns 103-115 are barrier checkboxes
    challenge_indices <- 103:115
    
    challenges_data <- filtered_data() %>%
      select(all_of(challenge_indices)) %>%
      summarise(across(everything(), ~sum(!is.na(.)))) %>%
      pivot_longer(everything(), names_to = "challenge", values_to = "count") %>%
      mutate(
        challenge = str_extract(challenge, "[^-]+$") %>% str_trim(),
        challenge_wrapped = sapply(challenge, wrap_label, max_chars = 35)
      ) %>%
      filter(count > 0, nchar(challenge) > 2) %>%  # Filter out very short entries like "in"
      arrange(desc(count)) %>%
      head(10)
    
    plot_ly(challenges_data, y = ~reorder(challenge_wrapped, count), x = ~count,
            type = "bar", orientation = "h", marker = list(color = teal_green)) %>%
      layout(yaxis = list(title = ""), xaxis = list(title = "Count"),
             margin = list(l = 200, r = 20, t = 20, b = 40),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
}

# Run app
shinyApp(ui = ui, server = server)