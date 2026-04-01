
# NFL Late-Game Decision Support Tool
# Shiny App



# 0. Libraries

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)


# 1. Load processed datasets
#    These are the outputs from the data processing script.

saveRDS(simulator_base, "simulator_base.rds")
saveRDS(simulator_summary_mvp, "simulator_summary_mvp.rds")

# 2. Startup QC prints
#    These print to the console when the app launches.
#    Keeping them because they have helped validate file paths
#    and confirm the app is loading the correct rebuilt files.

print(getwd())
print(list.files())
print(range(simulator_base$season, na.rm = TRUE))
print(unique(simulator_summary_mvp$score_bucket))


# 3. Helper formatting functions

pct_fmt <- function(x) {
  if (all(is.na(x))) return("NA")
  scales::percent(x, accuracy = 0.1)
}

num_fmt <- function(x, digits = 2) {
  if (all(is.na(x))) return("NA")
  format(round(x, digits), nsmall = digits)
}

# Used when a plot would otherwise be empty
empty_plot <- function(msg) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = msg, size = 6) +
    xlim(0, 1) + ylim(0, 1) +
    theme_void()
}

kpi_box <- function(title, value_output_id) {
  wellPanel(
    style = "text-align: center; min-height: 120px;",
    div(
      style = "font-weight: 700; color: black; font-size: 16px; margin-bottom: 10px;",
      title
    ),
    div(
      style = "font-weight: 700; color: #2C7BB6; font-size: 22px;",
      textOutput(value_output_id, inline = TRUE)
    )
  )
}


# 4. UI

ui <- navbarPage(
  title = "NFL Late-Game Decision Support Tool",
  

  # TAB 1: How To + Assumptions

  tabPanel(
    "How To + Assumptions",
      mainPanel(
        h2("NFL Late-Game Decision Support Tool"),
        p("This app helps users explore how different immediate play choices have historically performed in late-game NFL situations."),
        p("The tool compares likely outcomes for the next play choice - such as a run, pass, spike, kneel, or field goal attempt - based on historical NFL play-by-play data."),
        
        h4("How to use this app"),
        tags$ol(
          tags$li("Go to the Situation Simulator tab."),
          tags$li("Choose whether you want to analyze an End of Half or End of Game situation."),
          tags$li("Select the time remaining bucket, score bucket, and field position bucket."),
          tags$li("Review the chart and table to compare likely outcomes across next play choices."),
          tags$li("Use the Historical Evidence tab to inspect the underlying plays and supporting trends.")
        ),
        
        h4("Key assumptions"),
        tags$ul(
          tags$li("Results are based on historical associations, not causal estimates."),
          tags$li("Drive outcomes are attributed to the immediate next play choice observed in the historical data."),
          tags$li("The simulator uses broad buckets for time, score differential, and field position."),
          tags$li("Sample size matters: low-number groups should be interpreted cautiously.")
        ),
        
        h4("Definitions"),
        DTOutput("definitions_table"),
        
      )
  ),
  
  

  # TAB 3: Situation Simulator

  tabPanel(
    "Situation Simulator",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "sim_situation_type",
          "Situation Type",
          choices = c("End of Half", "End of Game"),
          selected = "End of Half"
        ),
        selectInput(
          "sim_time_bucket",
          "Time Remaining (seconds)",
          choices = c("0-30", "31-60", "61-90", "91-120"),
          selected = "31-60"
        ),
        selectInput(
          "sim_score_bucket",
          "Score Differential",
          choices = c("Down 8+", "Down 4-7", "Down 1-3", "Tied", "Up 1-3", "Up 4-7", "Up 8+"),
          selected = "Down 1-3"
        ),
        selectInput(
          "sim_yardline_bucket",
          "Field Position (yards from goal line)",
          choices = c("1-20", "21-40", "41-60", "61-80", "81-99"),
          selected = "21-40"
        )
      ),
      mainPanel(
        h3("Situation Simulator"),
        htmlOutput("sim_recommendation"),
        textOutput("sim_warning"),
        br(),
        plotOutput("sim_plot", height = "425px"),
        br(),
        DTOutput("sim_table")
      )
    )
  ),
  

  # TAB 4: Historical Evidence

  tabPanel(
    "Historical Evidence",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "hist_situation_type",
          "Situation Type",
          choices = c("End of Half", "End of Game"),
          selected = "End of Half"
        ),
        sliderInput(
          "hist_seasons",
          "Season Range",
          min = 2015,
          max = 2025,
          value = c(2015, 2025),
          step = 1,
          sep = ""
        ),
        selectInput(
          "hist_play_bucket",
          "Next Play Type",
          choices = c("All", sort(unique(simulator_base$next_play_bucket))),
          selected = "All"
        ),
        selectInput(
          "hist_posteam",
          "Posteam",
          choices = "All",
          selected = "All"
        ),
        selectInput(
          "hist_defteam",
          "Defteam",
          choices = "All",
          selected = "All"
        )
      ),
      mainPanel(
        h3("Historical Evidence"),
        fluidRow(
          column(3, kpi_box("Number of Plays", "kpi_n")),
          column(3, kpi_box("Avg Score Probability", "kpi_score_prob")),
          column(3, kpi_box("Avg EPA", "kpi_epa")),
          column(3, kpi_box("Avg Seconds Burned", "kpi_sec"))
        ),
        fluidRow(
          column(3, kpi_box("Run Sec Burned", "kpi_run_sec")),
          column(3, kpi_box("Pass Sec Burned", "kpi_pass_sec")),
          column(3, kpi_box("Spike Sec Burned", "kpi_spike_sec")),
          column(3, kpi_box("Kneel Sec Burned", "kpi_kneel_sec"))
        ),
        br(),
        h4("Historical Plays"),
        DTOutput("hist_table"),
        br(),
        h4("Selected Drive From This Play Forward"),
        textOutput("selected_drive_note"),
        DTOutput("selected_drive_table"),
        br(),
        verbatimTextOutput("hist_table_qc")
      )
    )
  )
)


# 5. Server

server <- function(input, output, session) {
  

  # 5A. Shared reactives and app initialization

  
  # Landing-tab subset used for the intro plot
  intro_data <- reactive({
    simulator_summary_mvp %>%
      filter(situation_type == input$intro_situation_type)
  })
  
  # Force the Historical Evidence season slider to match the
  # actual dataset loaded into the app at launch.
  observe({
    updateSliderInput(
      session,
      "hist_seasons",
      min = min(simulator_base$season, na.rm = TRUE),
      max = max(simulator_base$season, na.rm = TRUE),
      value = c(
        min(simulator_base$season, na.rm = TRUE),
        max(simulator_base$season, na.rm = TRUE)
      )
    )
  })
  

  # 5B. QC tab outputs

  output$qc_base_info <- renderPrint({
    list(
      simulator_base_dim = dim(simulator_base),
      simulator_base_season_range = range(simulator_base$season, na.rm = TRUE),
      simulator_base_situation_types = table(simulator_base$situation_type, useNA = "ifany"),
      simulator_base_score_buckets = table(simulator_base$score_bucket, useNA = "ifany"),
      simulator_base_time_buckets = table(simulator_base$time_bucket, useNA = "ifany"),
      simulator_base_yardline_buckets = table(simulator_base$yardline_bucket, useNA = "ifany"),
      simulator_base_play_buckets = table(simulator_base$next_play_bucket, useNA = "ifany")
    )
  })
  
  output$qc_summary_info <- renderPrint({
    list(
      simulator_summary_dim = dim(simulator_summary_mvp),
      simulator_summary_n_summary = summary(simulator_summary_mvp$n),
      simulator_summary_situation_types = table(simulator_summary_mvp$situation_type, useNA = "ifany"),
      simulator_summary_score_buckets = table(simulator_summary_mvp$score_bucket, useNA = "ifany"),
      simulator_summary_time_buckets = table(simulator_summary_mvp$time_bucket, useNA = "ifany"),
      simulator_summary_yardline_buckets = table(simulator_summary_mvp$yardline_bucket, useNA = "ifany"),
      simulator_summary_play_buckets = table(simulator_summary_mvp$next_play_bucket, useNA = "ifany")
    )
  })
  
  output$qc_sim_filter <- renderPrint({
    df <- simulator_summary_mvp %>%
      filter(
        situation_type == input$sim_situation_type,
        time_bucket == input$sim_time_bucket,
        score_bucket == input$sim_score_bucket,
        yardline_bucket == input$sim_yardline_bucket
      )
    
    list(
      selected_inputs = list(
        situation_type = input$sim_situation_type,
        time_bucket = input$sim_time_bucket,
        score_bucket = input$sim_score_bucket,
        yardline_bucket = input$sim_yardline_bucket
      ),
      rows_returned = nrow(df),
      play_types_returned = if (nrow(df) > 0) unique(df$next_play_bucket) else "NONE"
    )
  })
  
  output$qc_hist_filter <- renderPrint({
    df <- hist_filtered()
    
    list(
      selected_inputs = list(
        situation_type = input$hist_situation_type,
        season_range = input$hist_seasons,
        play_bucket = input$hist_play_bucket,
        posteam = input$hist_posteam,
        defteam = input$hist_defteam
      ),
      rows_returned = nrow(df),
      season_range_returned = if (nrow(df) > 0) range(df$season, na.rm = TRUE) else "NONE",
      play_types_returned = if (nrow(df) > 0) unique(df$next_play_bucket) else "NONE",
      posteams_returned = if (nrow(df) > 0) unique(df$posteam) else "NONE",
      defteams_returned = if (nrow(df) > 0) unique(df$defteam) else "NONE"
    )
  })
  
  output$qc_summary_table <- renderDT({
    df <- simulator_summary_mvp %>%
      arrange(desc(n)) %>%
      select(
        situation_type,
        time_bucket,
        score_bucket,
        yardline_bucket,
        next_play_bucket,
        n,
        p_any_score,
        avg_epa
      ) %>%
      head(25)
    
    datatable(df, rownames = FALSE, options = list(dom = "t"))
  })
  

  # 5C. Tab 1: Definitions + intro plot

  output$definitions_table <- renderDT({
    defs <- data.frame(
      Term = c(
        "End of Half",
        "End of Game",
        "P(TD)",
        "P(FG)",
        "P(Any Score)",
        "P(Turnover)",
        "Avg Sec Elapsed",
        "Avg EPA",
        "Number of Plays"
      ),
      Definition = c(
        "Late-game situations from the second quarter with 120 or fewer seconds left in the half.",
        "Late-game situations from the fourth quarter with 120 or fewer seconds left in regulation.",
        "Historical probability that the drive eventually ends in a touchdown.",
        "Historical probability that the drive eventually ends in a made field goal.",
        "Historical probability that the drive eventually produces either a touchdown or a field goal.",
        "Historical probability that the drive eventually ends in a turnover.",
        "Average number of seconds burned on the immediate next play.",
        "Average expected points added on the immediate next play.",
        "Number of historical plays in the selected bucket."
      )
    )
    
    datatable(defs, rownames = FALSE, options = list(dom = "t", pageLength = 10))
  })
  
  output$intro_plot <- renderPlot({
    df <- intro_data() %>%
      filter(!is.na(next_play_bucket), !is.na(p_any_score), !is.na(n)) %>%
      group_by(next_play_bucket) %>%
      summarize(
        number_of_plays = sum(n, na.rm = TRUE),
        p_any_score = weighted.mean(p_any_score, w = n, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(number_of_plays > 0, !is.na(p_any_score))
    
    if (nrow(df) == 0) return(empty_plot("No data available"))
    
    ggplot(df, aes(x = reorder(next_play_bucket, p_any_score), y = p_any_score, fill = next_play_bucket)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        expand = expansion(mult = c(0, 0.08))
      ) +
      labs(
        x = "Next Play Type",
        y = "Probability of Any Score",
        title = paste("Average Probability of Any Score -", input$intro_situation_type)
      ) +
      theme_minimal(base_size = 16) +
      theme(
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold")
      )
  })
  

  # 5D. Tab 2: Situation Simulator reactives and outputs

  sim_filtered <- reactive({
    simulator_summary_mvp %>%
      filter(
        situation_type == input$sim_situation_type,
        time_bucket == input$sim_time_bucket,
        score_bucket == input$sim_score_bucket,
        yardline_bucket == input$sim_yardline_bucket
      ) %>%
      arrange(desc(p_any_score))
  })
  
  output$sim_recommendation <- renderUI({
    df <- sim_filtered()
    
    if (nrow(df) == 0) {
      return(HTML("<b>Recommendation:</b> No historical observations match this situation."))
    }
    
    best_row <- df %>% slice_max(order_by = p_any_score, n = 1, with_ties = FALSE)
    
    HTML(
      paste0(
        "<b>Recommended play based on highest historical scoring probability:</b> ",
        best_row$next_play_bucket,
        " (",
        pct_fmt(best_row$p_any_score),
        ", Number of Plays = ",
        best_row$n,
        ")"
      )
    )
  })
  
  output$sim_warning <- renderText({
    df <- sim_filtered()
    
    if (nrow(df) == 0) {
      return("No historical observations match this situation.")
    }
    
    if (any(df$n < 10, na.rm = TRUE)) {
      "Warning: At least one play type has fewer than 10 historical observations in this bucket. Interpret cautiously."
    } else {
      "All displayed play types have at least 10 observations."
    }
  })
  
  output$sim_plot <- renderPlot({
    df <- sim_filtered() %>%
      filter(!is.na(next_play_bucket), !is.na(p_any_score), !is.na(n))
    
    if (nrow(df) == 0) return(empty_plot("No historical observations match this situation"))
    
    ggplot(df, aes(x = reorder(next_play_bucket, p_any_score), y = p_any_score, fill = n >= 10)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(
        values = c("TRUE" = "#2C7BB6", "FALSE" = "grey70"),
        labels = c("FALSE" = "Number of Plays < 10", "TRUE" = "Number of Plays >= 10"),
        name = "Sample Size"
      ) +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        expand = expansion(mult = c(0, 0.08))
      ) +
      labs(
        x = "Next Play Type",
        y = "Probability of Any Score",
        title = "Expected Scoring Success by Next Play Type",
        subtitle = paste(
          input$sim_situation_type, "|",
          input$sim_time_bucket, "|",
          input$sim_score_bucket, "|",
          input$sim_yardline_bucket
        )
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold")
      )
  })
  
  output$sim_table <- renderDT({
    df <- sim_filtered() %>%
      filter(!is.na(n)) %>%
      mutate(
        `Number of Plays` = n,
        `Avg Yard Line` = round(avg_yardline, 1),
        `Avg Sec Elapsed` = round(avg_sec_elapsed, 1),
        `Median Sec Elapsed` = round(median_sec_elapsed, 1),
        `Avg EPA` = round(avg_epa, 2),
        `P(TD)` = pct_fmt(p_td),
        `P(FG)` = pct_fmt(p_fg),
        `P(Any Score)` = pct_fmt(p_any_score),
        `P(Turnover)` = pct_fmt(p_turnover)
      ) %>%
      select(
        `Next Play Type` = next_play_bucket,
        `Number of Plays`,
        `Avg Yard Line`,
        `Avg Sec Elapsed`,
        `Median Sec Elapsed`,
        `Avg EPA`,
        `P(TD)`,
        `P(FG)`,
        `P(Any Score)`,
        `P(Turnover)`
      )
    
    datatable(
      df,
      rownames = FALSE,
      options = list(
        dom = "t",
        ordering = TRUE,
        autoWidth = TRUE
      )
    )
  })
  

  # 5E. Tab 3: Historical Evidence reactives

  
  hist_filter_base <- reactive({
    df <- simulator_base %>%
      filter(
        situation_type == input$hist_situation_type,
        season >= input$hist_seasons[1],
        season <= input$hist_seasons[2]
      )
    
    if (input$hist_play_bucket != "All") {
      df <- df %>% filter(next_play_bucket == input$hist_play_bucket)
    }
    
    df
  })
  
  observe({
    df <- hist_filter_base()
    
    if (!is.null(input$hist_defteam) && input$hist_defteam != "All") {
      df <- df %>% filter(defteam == input$hist_defteam)
    }
    
    posteam_choices <- c("All", sort(unique(df$posteam)))
    current_posteam <- isolate(input$hist_posteam)
    
    updateSelectInput(
      session,
      "hist_posteam",
      choices = posteam_choices,
      selected = if (!is.null(current_posteam) && current_posteam %in% posteam_choices) current_posteam else "All"
    )
  })
  
  observe({
    df <- hist_filter_base()
    
    if (!is.null(input$hist_posteam) && input$hist_posteam != "All") {
      df <- df %>% filter(posteam == input$hist_posteam)
    }
    
    defteam_choices <- c("All", sort(unique(df$defteam)))
    current_defteam <- isolate(input$hist_defteam)
    
    updateSelectInput(
      session,
      "hist_defteam",
      choices = defteam_choices,
      selected = if (!is.null(current_defteam) && current_defteam %in% defteam_choices) current_defteam else "All"
    )
  })
  
  hist_filtered <- reactive({
    df <- hist_filter_base()
    
    if (!is.null(input$hist_posteam) && input$hist_posteam != "All") {
      df <- df %>% filter(posteam == input$hist_posteam)
    }
    
    if (!is.null(input$hist_defteam) && input$hist_defteam != "All") {
      df <- df %>% filter(defteam == input$hist_defteam)
    }
    
    df
  })
  
  sec_by_play <- reactive({
    hist_filtered() %>%
      group_by(next_play_bucket) %>%
      summarize(
        avg_sec = if (all(is.na(sec_elapsed_next_play))) NA_real_ else mean(sec_elapsed_next_play, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  
  hist_display_data <- reactive({
    hist_filtered() %>%
      mutate(score_prob_num = p_any_score) %>%
      arrange(desc(score_prob_num)) %>%
      select(
        game_id,
        drive,
        play_id,
        season,
        situation_type,
        posteam,
        defteam,
        clock_remaining,
        score_differential,
        yardline_100,
        down,
        ydstogo,
        next_play_bucket,
        sec_elapsed_next_play,
        epa,
        score_prob_num,
        avg_epa,
        drive_outcome,
        desc
      )
  })
  
  
  
  
  selected_drive_data <- reactive({
    req(input$hist_table_rows_selected)
    
    selected_row <- input$hist_table_rows_selected
    base_df <- hist_display_data()
    
    req(nrow(base_df) >= selected_row)
    
    selected_play <- base_df[selected_row, ]
    
    simulator_base %>%
      filter(
        game_id == selected_play$game_id,
        drive == selected_play$drive,
        play_id >= selected_play$play_id
      ) %>%
      arrange(play_id)
  })
  

  # 5F. Tab 3: KPI outputs

  output$kpi_n <- renderText({
    df <- hist_filtered()
    if (nrow(df) == 0) return("0")
    format(nrow(df), big.mark = ",")
  })
  
  output$kpi_score_prob <- renderText({
    df <- hist_filtered()
    vals <- df$p_any_score
    if (length(vals) == 0 || all(is.na(vals))) return("NA")
    pct_fmt(mean(vals, na.rm = TRUE))
  })
  
  output$kpi_epa <- renderText({
    df <- hist_filtered()
    vals <- df$epa
    if (length(vals) == 0 || all(is.na(vals))) return("NA")
    num_fmt(mean(vals, na.rm = TRUE), 2)
  })
  
  output$kpi_sec <- renderText({
    df <- hist_filtered()
    vals <- df$sec_elapsed_next_play
    if (length(vals) == 0 || all(is.na(vals))) return("NA")
    num_fmt(mean(vals, na.rm = TRUE), 1)
  })
  
  output$kpi_run_sec <- renderText({
    x <- sec_by_play() %>% filter(next_play_bucket == "Run") %>% pull(avg_sec)
    if (length(x) == 0 || is.na(x)) "NA" else num_fmt(x, 1)
  })
  
  output$kpi_pass_sec <- renderText({
    x <- sec_by_play() %>% filter(next_play_bucket == "Pass") %>% pull(avg_sec)
    if (length(x) == 0 || is.na(x)) "NA" else num_fmt(x, 1)
  })
  
  output$kpi_spike_sec <- renderText({
    x <- sec_by_play() %>% filter(next_play_bucket == "Spike") %>% pull(avg_sec)
    if (length(x) == 0 || is.na(x)) "NA" else num_fmt(x, 1)
  })
  
  output$kpi_kneel_sec <- renderText({
    x <- sec_by_play() %>% filter(next_play_bucket == "Kneel") %>% pull(avg_sec)
    if (length(x) == 0 || is.na(x)) "NA" else num_fmt(x, 1)
  })
  

  # 5G. Tab 3: Historical Evidence table + QC

  output$hist_table <- renderDT({
    df <- hist_display_data() %>%
      transmute(
        Season = season,
        `Situation Type` = situation_type,
        'Poession Team' = posteam,
        'Defending Team' = defteam,
        `Seconds Remaining` = clock_remaining,
        `Score Differential` = score_differential,
        `Field Position` = yardline_100,
        Down = down,
        `Yards To Go` = ydstogo,
        `Next Play Type` = next_play_bucket,
        `Seconds Burned` = sec_elapsed_next_play,
        EPA = round(epa, 2),
        `Score Probability` = round(score_prob_num, 3),
        `Avg EPA` = round(avg_epa, 2),
        `Drive Outcome` = drive_outcome,
        Description = desc
      )
    
    validate(
      need(nrow(df) > 0, "No plays match the selected filters.")
    )
    
    datatable(
      df,
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 8,
        lengthMenu = c(5, 8, 10, 25, 50),
        scrollX = TRUE,
        ordering = FALSE
      )
    )
  })
  
  
  output$selected_drive_note <- renderText({
    if (is.null(input$hist_table_rows_selected)) {
      return("Select a play in the Historical Plays table to see the rest of that drive from that point forward.")
    }
    
    df <- hist_display_data()
    selected_row <- input$hist_table_rows_selected
    
    if (length(selected_row) == 0 || nrow(df) < selected_row) {
      return("Select a play in the Historical Plays table to see the rest of that drive from that point forward.")
    }
    
    selected_play <- df[selected_row, ]
    
    paste0(
      "Showing drive ", selected_play$drive,
      " from game ", selected_play$game_id,
      ", starting at play_id ", selected_play$play_id, "."
    )
  })
  
  output$selected_drive_table <- renderDT({
    if (is.null(input$hist_table_rows_selected)) {
      return(datatable(
        data.frame(Message = "Select a play above to view the rest of that drive from that point forward."),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }
    
    selected_row <- input$hist_table_rows_selected
    
    df <- selected_drive_data() %>%
      mutate(
        `Selected Play` = dplyr::if_else(
          row_number() == 1,
          "SELECTED PLAY",
          ""
        )
      ) %>%
      transmute(
        `Selected Play`,
        `Time Remaining` = clock_remaining,
        Down = down,
        `Yards To Go` = ydstogo,
        `Field Position` = yardline_100,
        `Next Play Type` = next_play_bucket,
        `Sec Burned` = sec_elapsed_next_play,
        EPA = round(epa, 2),
        `Score Probability` = round(p_any_score, 3),
        `Drive Outcome` = drive_outcome,
        Description = desc
      )
    
    validate(
      need(nrow(df) > 0, "No drive data available from the selected play forward.")
    )
    
    datatable(
      df,
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = 10,
        scrollX = TRUE,
        ordering = FALSE
      )
    )
  })

  

  
}


# 6. Run app

shinyApp(ui, server)