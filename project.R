library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyr)
library(stringr)
library(maps)     # For US map
library(tibble)

# ============================================================
# 1. Load Your Data
# ============================================================

job_postings <- readRDS("final_jobs.rds")

# Parse location → extract state
job_postings <- job_postings %>%
  mutate(
    state_abbrev = str_extract(location, "[A-Z]{2}$"),
    city = str_replace(location, ", [A-Z]{2}$", "")
  )

# Convert state abbreviation to full name for mapping
state_map <- tibble(
  state_abbrev = state.abb,
  state_name   = tolower(state.name)
)

job_postings <- job_postings %>%
  left_join(state_map, by = "state_abbrev")

# UI menu choices
skill_choices   <- sort(unique(job_postings$skill_name))
company_choices <- sort(unique(job_postings$company_name))

# Map data
us_states <- map_data("state")

# ============================================================
# 2. UI
# ============================================================

ui <- navbarPage(
  "Skill–Company Navigator",
  
  # ---------------------- Skill Explorer ---------------------
  tabPanel(
    "Skill Explorer",
    sidebarLayout(
      sidebarPanel(
        h4("Analyze by Skill"),
        selectInput("selected_skill", "Choose a Skill:", 
                    choices = skill_choices, selected = skill_choices[1]),
        checkboxInput("skill_top10", "Show top 10 companies only", TRUE),
        
        br(),
        h5("What this tool shows:"),
        tags$ul(
          tags$li("Salary distribution for the selected skill"),
          tags$li("Top companies hiring for this skill"),
          tags$li("Industries hiring this skill"),
          tags$li("U.S. map showing job density and median wages")
        ),
        tags$p("Use this explorer to understand how skills relate to pay, job demand, location, and industry presence.")
      ),
      
      mainPanel(
        h4("Salary Distribution"),
        plotOutput("skill_salary_plot"),
        
        br(),
        h4("Companies Hiring This Skill"),
        DTOutput("skill_company_table"),
        
        br(),
        h4("Industries Hiring This Skill"),
        plotOutput("skill_industry_plot"),
        
        br(),
        h4("U.S. Job Distribution Map"),
        
        # ================== MAP TOGGLE ABOVE MAP ==================
        fluidRow(
          column(
            width = 12,
            radioButtons(
              "map_metric",
              "Map Metric:",
              choices = c("Job Postings" = "postings",
                          "Median Hourly Pay" = "median_pay"),
              selected = "postings",
              inline = TRUE   # ← puts buttons in one line (optional)
            )
          )
        ),
        
        plotOutput("skill_us_map", height = "550px")
        # ==================================================================
        
      )
    )
  ),
  
  # ---------------------- Company Explorer --------------------
  tabPanel(
    "Company Explorer",
    sidebarLayout(
      sidebarPanel(
        h4("Explore Employers"),
        
        # Optional skill filter so this page is not just a duplicate of Skill Explorer
        selectInput(
          "ce_skill_filter",
          "Filter by skills (optional):",
          choices  = skill_choices,
          multiple = TRUE
        ),
        
        radioButtons(
          "ce_sort_metric",
          "Sort companies by:",
          choices = c(
            "Median hourly pay"   = "median_pay",
            "Total postings"      = "postings",
            "Employee count"      = "employee_count"
          ),
          selected = "median_pay"
        ),
        
        sliderInput(
          "ce_top_n",
          "Number of companies to show:",
          min = 10, max = 50, value = 30, step = 10
        ),
        
        
        # Highlight company – choices will be updated from the summary (only those in table/plots)
        selectInput(
          "ce_highlight_company",
          "Highlight company:",
          choices  = "None",
          selected = "None"
        ),
        
        helpText("Highlight options always match the companies currently shown in the plots and table.")
      ),
      
      mainPanel(
        h4("Company Landscape for Selected Skill Set"),
        plotOutput("ce_scatter_pay_vs_size", height = "420px"),
        
        br(),
        h4("Top Companies Overview"),
        plotOutput("ce_bar_top_companies", height = "320px"),
        
        br(),
        h4("Company Specialties Mix"),
        plotOutput("ce_bar_specialty", height = "320px"),
        
        br(),
        h4("Company Metrics Table"),
        DTOutput("ce_company_table")
      )
    )
  )
)


# ============================================================
# 3. SERVER LOGIC
# ============================================================

server <- function(input, output, session) {
  
  # ---------------------------- Skill View ----------------------------
  filtered_by_skill <- reactive({
    job_postings %>% filter(skill_name == input$selected_skill)
  })
  
  # Salary Distribution ------------------------------------------------
  output$skill_salary_plot <- renderPlot({
    df <- filtered_by_skill()
    if (nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = normalized_hourly)) +
      geom_histogram(fill = "#0073C2", bins = 20, color = "white") +
      labs(
        x = "Hourly Wage ($)", y = "Count",
        title = paste("Hourly Salary Distribution for", input$selected_skill)
      ) +
      theme_minimal()
  })
  
  # Companies Hiring Table --------------------------------------------
  output$skill_company_table <- renderDT({
    df <- filtered_by_skill()
    if (nrow(df) == 0) return(NULL)
    
    df_summary <- df %>%
      filter(!is.na(company_name)) %>%
      group_by(company_name) %>%
      summarise(
        postings   = n(),
        median_pay = round(median(normalized_hourly, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      arrange(desc(postings))
    
    if (input$skill_top10) df_summary <- head(df_summary, 10)
    
    datatable(df_summary)
  })
  
  # Industries Hiring -------------------------------------------------
  output$skill_industry_plot <- renderPlot({
    df <- filtered_by_skill()
    if (nrow(df) == 0) return(NULL)
    
    df_counts <- df %>%
      filter(!is.na(industry_name)) |>
      count(industry_name) %>%
      arrange(desc(n)) %>%
      slice_head(n = 20)
    
    ggplot(df_counts, aes(x = reorder(industry_name, n), y = n)) +
      geom_col(fill = "darkgreen") +
      coord_flip() +
      labs(
        x = "Industry", y = "Posting Count",
        title = paste("Top 20 Industries Hiring:", input$selected_skill)
      ) +
      theme_minimal(base_size = 12)
  })
  
  
  # ====================== U.S. MAP (UPDATED) ============================
  skill_state_summary <- reactive({
    df <- filtered_by_skill()
    
    df %>%
      filter(!is.na(state_name)) %>%
      group_by(state_name) %>%
      summarise(
        postings   = n(),
        median_pay = median(normalized_hourly, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  output$skill_us_map <- renderPlot({
    state_df <- skill_state_summary()
    
    metric     <- input$map_metric
    fill_label <- ifelse(metric == "postings", 
                         "Job Postings              ", 
                         "Median Hourly Pay ($)")
    
    # Join with shape map
    plot_df <- us_states %>%
      left_join(state_df, by = c("region" = "state_name"))
    
    ggplot(plot_df, aes(long, lat, group = group, fill = .data[[metric]])) +
      geom_polygon(color = "white") +
      coord_fixed(1.3) +
      scale_fill_continuous(
        name    = fill_label,
        na.value = "grey90"
      ) +
      labs(
        title = paste("U.S. Map for Skill:", input$selected_skill,
                      "\nMetric:", fill_label)
      ) +
      theme_minimal(base_size = 14)
  })
  # ==================================================================
  
  
  # ------------------------ Company Explorer (new) -----------------------
  
  # 1) Job-level filter (by optional skill list)
  ce_filtered_jobs <- reactive({
    df <- job_postings
    
    skills <- input$ce_skill_filter
    if (!is.null(skills) && length(skills) > 0) {
      df <- df %>% dplyr::filter(skill_name %in% skills)
    }
    
    df
  })
  
  # 2) Company-level summary
  ce_company_summary <- reactive({
    df <- ce_filtered_jobs()
    if (nrow(df) == 0) return(df[0, ])
    
    sum_df <- df %>%
      dplyr::filter(!is.na(company_name)) %>%
      dplyr::group_by(company_id, company_name) %>%
      dplyr::summarise(
        postings       = dplyr::n(),
        median_pay     = stats::median(normalized_hourly, na.rm = TRUE),
        employee_count = dplyr::first(employee_count),
        speciality     = dplyr::first(speciality),
        .groups = "drop"
      )
    
    
    # sort by selected metric
    metric <- input$ce_sort_metric
    if (!is.null(metric) && metric %in% names(sum_df)) {
      sum_df <- sum_df %>% dplyr::arrange(dplyr::desc(.data[[metric]]))
    } else {
      sum_df <- sum_df %>% dplyr::arrange(dplyr::desc(postings))
    }
    
    # keep only top N (slice_head safely handles N > nrow)
    top_n <- if (is.null(input$ce_top_n)) 30 else input$ce_top_n
    sum_df %>% dplyr::slice_head(n = top_n)
  })
  
  # 3) Update highlight choices so user can only pick companies currently plotted / in table
  observe({
    df <- ce_company_summary()
    choices <- c("None", df$company_name)
    
    current <- isolate(input$ce_highlight_company)
    selected <- if (!is.null(current) && current %in% choices) current else "None"
    
    updateSelectInput(
      session,
      "ce_highlight_company",
      choices  = choices,
      selected = selected
    )
  })
  
  # 4) Scatter: median pay vs employee size
  output$ce_scatter_pay_vs_size <- renderPlot({
    df <- ce_company_summary()
    if (nrow(df) == 0) return(NULL)
    
    # avoid zeros / NAs for log scale
    df <- df %>%
      dplyr::mutate(
        employee_count = ifelse(is.na(employee_count) | employee_count <= 0,
                                NA, employee_count)
      ) %>%
      dplyr::filter(!is.na(employee_count))
    
    if (nrow(df) == 0) return(NULL)
    
    highlight <- input$ce_highlight_company
    
    p <- ggplot(df, aes(x = employee_count, y = median_pay)) +
      geom_point(
        aes(size = postings, colour = speciality),
        alpha = 0.7
      ) +
      scale_size(range = c(3, 14)) +
      scale_x_log10(
        labels = scales::label_comma(accuracy = 1),
        breaks = scales::log_breaks(n = 5)
      ) +
      labs(
        x = "Employee count (log scale)",
        y = "Median hourly pay ($)",
        size   = "Postings",
        colour = "Speciality",
        title  = "Median pay vs company size"
      ) +
      theme_minimal() +
      guides(colour = "none")   # <--- hide huge speciality legend
    
    
    # overlay highlighted company with a big yellow point
    if (!is.null(highlight) && highlight != "None" && highlight %in% df$company_name) {
      p <- p +
        geom_point(
          data = dplyr::filter(df, company_name == highlight),
          aes(x = employee_count, y = median_pay),
          inherit.aes = FALSE,
          size   = 6,
          shape  = 21,
          colour = "black",
          fill   = "yellow"
        )
    }
    
    p
  })
  
  
  # 5) Bar chart of top companies (by chosen metric)
  output$ce_bar_top_companies <- renderPlot({
    df <- ce_company_summary()
    if (nrow(df) == 0) return(NULL)
    
    metric <- input$ce_sort_metric
    label  <- dplyr::case_when(
      metric == "median_pay"     ~ "Median hourly pay ($)",
      metric == "employee_count" ~ "Employee count",
      TRUE                       ~ "Total postings"
    )
    
    highlight <- input$ce_highlight_company
    
    df <- df %>%
      dplyr::mutate(
        is_highlight = dplyr::case_when(
          !is.null(highlight) && highlight != "None" & company_name == highlight ~ "Highlight",
          TRUE                                                                  ~ "Other"
        )
      )
    
    ggplot(df,
           aes(x = reorder(company_name, .data[[metric]]),
               y = .data[[metric]],
               fill = is_highlight)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(
        values = c("Other" = "#1A73E8", "Highlight" = "#FFC107"),
        guide  = "none"
      ) +
      labs(
        x = "Company",
        y = label,
        title = paste("Top companies by", tolower(label))
      ) +
      theme_minimal()
  })
  
  
  # 6) Specialty mix bar chart
  output$ce_bar_specialty <- renderPlot({
    df <- ce_company_summary()
    if (nrow(df) == 0 || !"speciality" %in% names(df)) return(NULL)
    
    spec_df <- df %>%
      dplyr::filter(!is.na(speciality), speciality != "") %>%
      # keep only text before the first comma
      dplyr::mutate(
        speciality_short = sub(",.*", "", speciality)
      ) %>%
      dplyr::group_by(speciality_short) %>%
      dplyr::summarise(
        postings = sum(postings),
        .groups  = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(postings))
    
    if (nrow(spec_df) == 0) return(NULL)
    
    ggplot(spec_df,
           aes(x = reorder(speciality_short, postings), y = postings)) +
      geom_col(fill = "#FF7043") +
      coord_flip() +
      labs(
        x = "Speciality",
        y = "Total postings",
        title = "Specialty mix among selected companies"
      ) +
      theme_minimal()
  })
  
  
  # 7) Company metrics table
  output$ce_company_table <- DT::renderDT({
    df <- ce_company_summary()
    if (nrow(df) == 0) return(NULL)
    
    highlight <- input$ce_highlight_company
    
    dat <- DT::datatable(
      df,
      options  = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
    
    if (!is.null(highlight) &&
        highlight != "None" &&
        highlight %in% df$company_name) {
      
      dat <- dat %>%
        DT::formatStyle(
          "company_name",
          backgroundColor = DT::styleEqual(highlight, "khaki"),
          fontWeight      = DT::styleEqual(highlight, "bold")
        )
    }
    
    dat
  })
  
}

# ============================================================
# 4. RUN APP
# ============================================================

shinyApp(ui = ui, server = server)
