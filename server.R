# Server logic
library(shiny)
library(tidyverse)
source("translations.R")
library(ggplot2)
library(rstatix)
library(gridExtra)
library(DT)
library(ggprism)
library(ggsignif)
library(viridis)
library(shinyjs)
library(openxlsx)
library(sortable)
library(ggsci)
library(ggthemes)
library(ggbeeswarm)

shinyServer(function(input, output, session) {
  
  # Setup plot encoding function inline
  if (Sys.info()["sysname"] == "Darwin") {
    Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
  } else if (Sys.info()["sysname"] == "Windows") {
    Sys.setlocale("LC_CTYPE", "Chinese")
  }
  
  # Safe plot title function
  safe_plot_title <- function(text, lang) {
    if (lang == "zh") {
      return("Relative Expression Analysis")
    } else {
      return(text)
    }
  }
  
  current_language <- reactive({
    input$language %||% "en"  # Default to English
  })
  
  output$app_title <- renderText({ tr("app_title", current_language()) })
  output$analysis_tab <- renderText({ tr("analysis_tab", current_language()) })
  output$help_tab <- renderText({ tr("help_tab", current_language()) })
  
  output$step1_title <- renderText({ tr("step1_title", current_language()) })
  output$upload_file <- renderText({ tr("upload_file", current_language()) })
  output$upload_help <- renderText({ tr("upload_help", current_language()) })
  
  output$step2_title <- renderText({ tr("step2_title", current_language()) })
  output$control_sample <- renderText({ tr("control_sample", current_language()) })
  output$auto_detect_hk <- renderText({ tr("auto_detect_hk", current_language()) })
  output$manual_hk_genes <- renderText({ tr("manual_hk_genes", current_language()) })
  output$advanced_hk_settings <- renderText({ tr("advanced_hk_settings", current_language()) })
  output$detection_pattern <- renderText({ tr("detection_pattern", current_language()) })
  output$geometric_mean <- renderText({ tr("geometric_mean", current_language()) })
  output$run_analysis <- renderText({ tr("run_analysis", current_language()) })
  
  output$statistical_settings <- renderText({ tr("statistical_settings", current_language()) })
  output$statistical_test <- renderText({ tr("statistical_test", current_language()) })
  output$test_on <- renderText({ tr("test_on", current_language()) })
  output$advanced_statistics <- renderText({ tr("advanced_statistics", current_language()) })
  output$p_adjust_method <- renderText({ tr("p_adjust_method", current_language()) })
  output$significance_threshold <- renderText({ tr("significance_threshold", current_language()) })
  
  output$plot_customization <- renderText({ tr("plot_customization", current_language()) })
  output$plot_title <- renderText({ tr("plot_title", current_language()) })
  output$plot_title_default <- renderText({ tr("plot_title_default", current_language()) })
  output$data_display_type <- renderText({ tr("data_display_type", current_language()) })
  output$plot_type <- renderText({ tr("plot_type", current_language()) })
  output$error_bar_type <- renderText({ tr("error_bar_type", current_language()) })
  output$advanced_plot_settings <- renderText({ tr("advanced_plot_settings", current_language()) })
  output$sample_order <- renderText({ tr("sample_order", current_language()) })
  output$update_plot <- renderText({ tr("update_plot", current_language()) })
  output$show_individual_points <- renderText({ tr("show_individual_points", current_language()) })
  output$show_significance <- renderText({ tr("show_significance", current_language()) })
  output$show_non_significant <- renderText({ tr("show_non_significant", current_language()) })
  output$significance_display <- renderText({ tr("significance_display", current_language()) })
  output$color_palette <- renderText({ tr("color_palette", current_language()) })
  output$size_adjustments <- renderText({ tr("size_adjustments", current_language()) })
  output$plot_height <- renderText({ tr("plot_height", current_language()) })
  output$plot_width <- renderText({ tr("plot_width", current_language()) })
  output$font_size <- renderText({ tr("font_size", current_language()) })
  output$point_size <- renderText({ tr("point_size", current_language()) })
  output$facet_cols <- renderText({ tr("facet_cols", current_language()) })
  
  output$data_preview <- renderText({ tr("data_preview", current_language()) })
  output$raw_data <- renderText({ tr("raw_data", current_language()) })
  output$quality_control <- renderText({ tr("quality_control", current_language()) })
  output$analysis_results <- renderText({ tr("analysis_results", current_language()) })
  output$statistical_analysis <- renderText({ tr("statistical_analysis", current_language()) })
  
  output$exclude_selected <- renderText({ tr("exclude_selected", current_language()) })
  output$clear_exclusions <- renderText({ tr("clear_exclusions", current_language()) })
  output$data_preview_help <- renderText({ tr("data_preview_help", current_language()) })
  output$qc_plots <- renderText({ tr("qc_plots", current_language()) })
  output$plot_format <- renderText({ tr("plot_format", current_language()) })
  output$download_plot <- renderText({ tr("download_plot", current_language()) })
  output$footer_attribution <- renderText({ tr("footer_attribution", current_language()) })
  output$footer_github_text <- renderText({ tr("footer_github_text", current_language()) })
  output$footer_license_support <- renderText({ tr("footer_license_support", current_language()) })
  
  # Data privacy translations
  output$data_privacy_title <- renderText({ tr("data_privacy_title", current_language()) })
  output$data_secure_title <- renderText({ tr("data_secure_title", current_language()) })
  output$session_management_title <- renderText({ tr("session_management_title", current_language()) })
  output$no_server_storage <- renderText({ tr("no_server_storage", current_language()) })
  output$session_only_processing <- renderText({ tr("session_only_processing", current_language()) })
  output$local_processing <- renderText({ tr("local_processing", current_language()) })
  output$no_data_transmission <- renderText({ tr("no_data_transmission", current_language()) })
  output$starting_fresh <- renderText({ tr("starting_fresh", current_language()) })
  output$save_your_work <- renderText({ tr("save_your_work", current_language()) })
  output$session_duration <- renderText({ tr("session_duration", current_language()) })
  output$memory_cleanup <- renderText({ tr("memory_cleanup", current_language()) })
  
  observe({
    if (is.null(input$plotTitle) || input$plotTitle == "") {
      updateTextInput(session, "plotTitle", 
                      value = tr("plot_title_default", current_language()))
    }
  })
  
  observe({
    # Update statistical test choices
    updateSelectInput(session, "statsTest",
                      choices = setNames(
                        c("anova", "kruskal"),
                        c(tr("one_way_anova", current_language()), 
                          tr("kruskal_wallis", current_language()))
                      )
    )
    
    # Update stats data type choices
    updateSelectInput(session, "statsDataType",
                      choices = setNames(
                        c("ddct", "dct"),
                        c(tr("ddct_values", current_language()), 
                          tr("dct_values", current_language()))
                      )
    )
    
    # Update p-adjust method choices
    updateSelectInput(session, "pAdjustMethod",
                      choices = setNames(
                        c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "BY"),
                        c(tr("none", current_language()),
                          tr("bonferroni", current_language()),
                          tr("holm", current_language()),
                          tr("hochberg", current_language()),
                          tr("hommel", current_language()),
                          tr("fdr_bh", current_language()),
                          tr("benjamini_yekutieli", current_language()))
                      )
    )
    
    # Update data display type choices
    updateSelectInput(session, "dataDisplayType",
                      choices = setNames(
                        c("fold_change", "ddct", "neg_ddct", "dct", "neg_dct"),
                        c(tr("fold_change", current_language()),
                          tr("ddct_values", current_language()),
                          tr("neg_ddct_values", current_language()),
                          tr("dct_values", current_language()),
                          tr("neg_dct_values", current_language()))
                      )
    )
    
    # Update plot type choices
    updateSelectInput(session, "plotType",
                      choices = setNames(
                        c("bar", "box", "violin", "beeswarm"),
                        c(tr("bar_plot", current_language()),
                          tr("box_plot", current_language()),
                          tr("violin_plot", current_language()),
                          tr("beeswarm_plot", current_language()))
                      )
    )
    
    # Update error bar choices
    updateSelectInput(session, "errorBar",
                      choices = setNames(
                        c("se", "sd", "ci"),
                        c(tr("standard_error", current_language()),
                          tr("standard_deviation", current_language()),
                          tr("confidence_interval", current_language()))
                      )
    )
    
    # Update significance display choices
    updateSelectInput(session, "significanceType",
                      choices = setNames(
                        c("stars", "p.value", "both"),
                        c(tr("stars", current_language()),
                          tr("p_values", current_language()),
                          tr("both", current_language()))
                      )
    )
    
    # Update color palette choices
    updateSelectInput(session, "colorPalette",
                      choices = setNames(
                        c("classic", "viridis", "bw", "grey", "npg", "aaas", "nejm", "lancet", "jama", "set1", "set2", "dark2", "paired"),
                        c(tr("classic_default", current_language()),
                          tr("colorblind_friendly", current_language()),
                          tr("black_white", current_language()),
                          tr("grayscale", current_language()),
                          tr("nature_npg", current_language()),
                          tr("science_aaas", current_language()),
                          tr("nejm", current_language()),
                          tr("lancet", current_language()),
                          tr("jama", current_language()),
                          tr("set1_bright", current_language()),
                          tr("set2_pastel", current_language()),
                          tr("dark2", current_language()),
                          tr("paired", current_language()))
                      )
    )
    
    # Update plot format choices
    updateSelectInput(session, "plotFormat",
                      choices = setNames(
                        c("pdf", "png"),
                        c(tr("pdf_format", current_language()),
                          tr("png_format", current_language()))
                      )
    )
    
    # Update plot title with translated default
    if (is.null(input$plotTitle) || input$plotTitle == "" || 
        input$plotTitle == tr("plot_title_default", "en") || 
        input$plotTitle == tr("plot_title_default", "zh")) {
      updateTextInput(session, "plotTitle", 
                      value = tr("plot_title_default", current_language()))
    }
  })
  
  showNotificationWithLog <- function(message, type = "default", duration = 5) {
    if(type == "error") {
      cat("[ERROR]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", message, "\n")
    } else if(type == "warning") {
      cat("[WARNING]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", message, "\n")
    } else {
      cat("[INFO]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", message, "\n")
    }
    
    showNotification(message, type = type, duration = duration)
  }
  
  values <- reactiveValues(
    data = NULL,
    excluded_points = character(0),
    housekeeping_genes = NULL,
    validation_messages = NULL,
    results = NULL,
    stats_results = NULL,
    sample_order = NULL,
    edit_history=list(),
    analysis_run = FALSE,
    excel_sheets = NULL,
    selected_sheet = NULL
  )
  
  output$analysisMessage <- renderUI({
    if (!values$analysis_run) {
      div(
        class = "alert alert-warning",
        style = "margin-top: 20px;",
        tags$h4(tr("analysis_not_run", current_language())),
        tr("run_analysis_first", current_language())
      )
    }
  })
  
  observeEvent(input$runAnalysis, {
    req(values$data, input$controlSample, values$housekeeping_genes)
    
    cat("[INFO]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Analysis started\n")
    cat("[INFO]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Control sample:", input$controlSample, "\n")
    cat("[INFO]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Housekeeping genes:", paste(values$housekeeping_genes, collapse = ", "), "\n")
    
    withProgress(message = 'Running analysis...', {
      analysis_data <- values$data %>% 
        filter(!row_id %in% values$excluded_points)
      
      if(nrow(analysis_data) == 0) {
        showNotificationWithLog("No valid data for analysis after exclusions", type = "error")
        values$analysis_run <- FALSE
        return()
      }
      
      results <- try({
        calculateFoldChanges(
          data = analysis_data,
          controlSample = input$controlSample,
          housekeeping_genes = values$housekeeping_genes
        )
      }, silent = TRUE)
      
      if(inherits(results, "try-error")) {
        values$analysis_run <- FALSE
        showNotificationWithLog(paste("Analysis error:", attr(results, "condition")$message), type = "error")
        return()
      }
      
      values$results <- results
      
      if(!is.null(results) && nrow(results) > 0) {
        values$stats_results <- performStatisticalAnalysis(results)
        values$analysis_run <- TRUE
        showNotificationWithLog("Analysis complete!", type = "message")
      } else {
        values$analysis_run <- FALSE
        showNotificationWithLog("Analysis produced no results", type = "warning")
      }
    })
  })
  
  output$downloadAllResultsButton <- renderUI({
    if (!values$analysis_run) {
      tags$div(
        style = "position: relative;",
        tags$div(
          title = tr("run_analysis_tooltip", current_language()),
          downloadButton("downloadAllResults", tr("download_all_results", current_language()),
                         class = "btn-success disabled",
                         style = "width: 100%; opacity: 0.65;")
        )
      )
    } else {
      downloadButton("downloadAllResults", tr("download_all_results", current_language()),
                     class = "btn-success",
                     style = "width: 100%;")
    }
  })
  
  validateData <- function(data) {
    if(is.null(data) || nrow(data) == 0) {
      return("No data available")
    }
    
    required_cols <- c("Sample", "Target", "Cq")
    missing_cols <- required_cols[!required_cols %in% names(data)]
    
    if(length(missing_cols) > 0) {
      return(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    }
    
    invalid_cq <- sum(is.na(data$Cq) | !is.finite(data$Cq))
    if(invalid_cq > 0) {
      return(paste("Found", invalid_cq, "invalid Cq values"))
    }
    
    if(length(unique(data$Sample)) < 2) {
      return("Need at least 2 different samples for analysis")
    }
    
    if(length(unique(data$Target)) < 2) {
      return("Need at least 2 different targets for analysis")
    }
    
    return(NULL)
  }
  
  # Sheet selection UI
  output$sheetSelection <- renderUI({
    req(input$file)
    file_ext <- tolower(tools::file_ext(input$file$datapath))
    
    if(file_ext %in% c("xlsx", "xls") && !is.null(values$excel_sheets)) {
      if(length(values$excel_sheets$valid_sheets) > 0) {
        selectInput("excelSheet", 
                   "Select Sheet:",
                   choices = values$excel_sheets$valid_sheets,
                   selected = values$excel_sheets$valid_sheets[1])
      } else {
        tags$div(
          class = "alert alert-warning",
          "No sheets with valid data found in Excel file"
        )
      }
    }
  })
  
  # Detect sheets in Excel file
  observeEvent(input$file, {
    req(input$file)
    values$analysis_run <- FALSE
    values$data <- NULL
    values$excel_sheets <- NULL
    
    cat("[INFO]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- File upload started:", input$file$name, "\n")
    
    file_ext <- tolower(tools::file_ext(input$file$datapath))
    
    if(file_ext %in% c("xlsx", "xls")) {
        sheets <- openxlsx::getSheetNames(input$file$datapath)
      cat("[INFO]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Found", length(sheets), "sheets:", paste(sheets, collapse = ", "), "\n")
      
      valid_sheets <- character()
      sheet_info <- list()
      
      for(sheet in sheets) {
        cat("[INFO]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Checking sheet:", sheet, "\n")
        
        sheet_data <- try({
          openxlsx::read.xlsx(input$file$datapath, sheet = sheet, skipEmptyRows = TRUE, skipEmptyCols = TRUE)
        }, silent = TRUE)
        
        if(!inherits(sheet_data, "try-error") && !is.null(sheet_data) && nrow(sheet_data) > 0) {
          header_text <- tolower(paste(names(sheet_data), collapse = " "))
          
          has_required <- grepl("sample", header_text) && 
                         grepl("target", header_text) && 
                         (grepl("\\bcq\\b", header_text) || grepl("\\bct\\b", header_text))
          
          if(has_required) {
            valid_sheets <- c(valid_sheets, sheet)
            cat("[INFO]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Sheet", sheet, "has valid qPCR data\n")
          }
        }
      }
      
      values$excel_sheets <- list(
        all_sheets = sheets,
        valid_sheets = valid_sheets,
        sheet_info = sheet_info
      )
      
      if(length(valid_sheets) == 0) {
        showNotificationWithLog("No sheets with valid qPCR data found in Excel file", type = "error")
      } else {
        cat("[INFO]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Valid sheets found:", paste(valid_sheets, collapse = ", "), "\n")
      }
    } else {
      loadDataFromFile(input$file$datapath, file_ext)
    }
  })
  
  observeEvent(input$excelSheet, {
    req(input$file, input$excelSheet)
    
    file_ext <- tolower(tools::file_ext(input$file$datapath))
    if(file_ext %in% c("xlsx", "xls")) {
      cat("[INFO]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Loading sheet:", input$excelSheet, "\n")
      loadDataFromFile(input$file$datapath, file_ext, sheet = input$excelSheet)
    }
  })
  
  loadDataFromFile <- function(filepath, file_ext, sheet = NULL) {
    values$analysis_run <- FALSE

    tryCatch({
      cat("[INFO]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- File type:", file_ext, "\n")
      
      if(file_ext %in% c("xlsx", "xls")) {
        if(is.null(sheet)) {
          sheet <- 1  # Default to first sheet if not specified
        }
        
        # Read with skipEmptyRows to handle files with metadata/empty rows at top
        data <- openxlsx::read.xlsx(filepath, sheet = sheet, skipEmptyRows = TRUE, skipEmptyCols = TRUE)
        
        cat("[INFO]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Reading Excel sheet:", sheet, "\n")
        cat("[INFO]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Column names found:", paste(names(data), collapse = ", "), "\n")
        
        # Check if the first row looks like valid headers
        required_patterns <- c("sample", "target", "cq|ct")
        header_text <- tolower(paste(names(data), collapse = " "))
        
        has_valid_headers <- grepl("sample", header_text) && 
                            grepl("target", header_text) && 
                            (grepl("\\bcq\\b", header_text) || grepl("\\bct\\b", header_text))
        
        if(!has_valid_headers) {
          showNotificationWithLog("Invalid headers detected. The first row with data should contain column headers including 'Sample', 'Target', and 'Cq' (or 'Ct'). Please check your Excel file.", type = "error")
          cat("[ERROR]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Invalid headers. Found columns:", paste(names(data), collapse = ", "), "\n")
          return()
        }
        
      } else {
        con <- file(filepath, "r")
        all_lines <- readLines(con, warn = FALSE)
        close(con)
        
        header_patterns <- c(
          "Target.*Sample.*Cq",
          "Sample.*Target.*Cq", 
          "Target.*Sample.*C[Tt]",
          "Sample.*Target.*C[Tt]"
        )
        
        header_index <- NA
        for(pattern in header_patterns) {
          header_index <- which(grepl(pattern, all_lines, ignore.case = TRUE))[1]
          if(!is.na(header_index)) break
        }
        
        if (is.na(header_index)) {
          # Try to find by looking for a line with all three keywords
          for(i in 1:min(20, length(all_lines))) {
            line_upper <- toupper(all_lines[i])
            if(grepl("TARGET", line_upper) && grepl("SAMPLE", line_upper) && 
               (grepl("CQ", line_upper) || grepl("CT", line_upper))) {
              header_index <- i
              break
            }
          }
        }
        
        if (is.na(header_index)) {
          showNotificationWithLog("Could not find data headers in file", type = "error")
          return()
        }
        
        data_lines <- all_lines[header_index:length(all_lines)]
        temp_file <- tempfile(fileext = ".csv")
        writeLines(data_lines, temp_file)
        
        data <- read.csv(temp_file, stringsAsFactors = FALSE)
      }
      
      data <- data[rowSums(is.na(data) | data == "") != ncol(data), ]
      
      names(data) <- make.names(names(data), unique = TRUE)
      
      col_mapping <- list()
      for(col in names(data)) {
        col_lower <- tolower(col)
        if(grepl("sample", col_lower) && !("Sample" %in% names(col_mapping))) {
          col_mapping[["Sample"]] <- col
        } else if(grepl("target", col_lower) && !("Target" %in% names(col_mapping))) {
          col_mapping[["Target"]] <- col
        } else if((grepl("^cq$|^ct$", col_lower) || grepl("cq$|ct$", col_lower)) && !("Cq" %in% names(col_mapping))) {
          col_mapping[["Cq"]] <- col
        }
      }
      
      for(standard_name in names(col_mapping)) {
        original_name <- col_mapping[[standard_name]]
        if(original_name != standard_name) {
          names(data)[names(data) == original_name] <- standard_name
        }
      }
      
      required_cols <- c("Sample", "Target", "Cq")
      missing_cols <- required_cols[!required_cols %in% names(data)]
      
      if(length(missing_cols) > 0) {
        cat("[ERROR]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Missing columns:", paste(missing_cols, collapse = ", "), "\n")
        cat("[INFO]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Available columns:", paste(names(data), collapse = ", "), "\n")
      
      cat("[INFO]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- First few rows of data:\n")
      print(head(data[, required_cols], n = 3))
        showNotificationWithLog(paste("Missing required columns:", paste(missing_cols, collapse = ", ")), type = "error")
        return()
      }
      
      data <- data %>%
        filter(!is.na(Sample) & Sample != "" & 
                 !is.na(Target) & Target != "" & 
                 !is.na(Cq) & Cq != "") %>%
        mutate(
          Sample = as.character(Sample),
          Target = as.character(Target),
          Cq = as.numeric(as.character(Cq)),
          row_id = paste(row_number(), Target, Sample, Cq, sep = "__|__")
        ) %>%
        mutate(
          Sample = factor(Sample),
          Target = factor(Target)
        ) %>%
        arrange(Sample, Target)
      
      invalid_cq <- is.na(data$Cq)
      if(any(invalid_cq)) {
        invalid_rows <- which(invalid_cq)
        showNotificationWithLog(
          sprintf("Removed %d rows with invalid Cq values", length(invalid_rows)),
          type = "warning"
        )
        data <- data[!invalid_cq, ]
      }
      
      values$sample_order <- levels(data$Sample)
      
      validation_result <- validateData(data)
      if(!is.null(validation_result)) {
        showNotificationWithLog(validation_result, type = "error")
        return()
      }
      
      values$data <- data
      
      cat("[INFO]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Data loaded with columns:", paste(names(data), collapse = ", "), "\n")
      cat("[INFO]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Unique samples:", paste(unique(data$Sample), collapse = ", "), "\n")
      cat("[INFO]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Unique targets:", paste(unique(data$Target), collapse = ", "), "\n")
      
      updateSelectInput(session, "controlSample",
                        choices = levels(data$Sample),
                        selected = levels(data$Sample)[1])
      
      updateSelectInput(session, "manualHKGenes",
                        choices = levels(data$Target))
      
      detectHousekeepingGenes()
      
      showNotificationWithLog(
        paste("Successfully loaded", nrow(data), "data points"),
        type = "message"
      )
      
    }, error = function(e) {
      cat("[ERROR]", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- File reading error details:\n")
      print(e)
      showNotificationWithLog(paste("Error reading file:", e$message), type = "error")
    })
  }
  
  output$sampleOrderUI <- renderUI({
    req(values$data)
    sortable::rank_list(
      text = tr("drag_reorder_samples", current_language()),
      labels = unique(values$data$Sample),
      input_id = "sample_order_list"
    )
  })
  
  observeEvent(input$updatePlot, {
    # Update sample order if available
    if(!is.null(input$sample_order_list)) {
      # Remove duplicates and ensure valid levels
      sample_order <- unique(input$sample_order_list)
      values$sample_order <- sample_order
      
      if(!is.null(values$data)) {
        # Only use levels that exist in the data
        existing_samples <- unique(values$data$Sample)
        valid_levels <- sample_order[sample_order %in% existing_samples]
        if(length(valid_levels) > 0) {
          values$data$Sample <- factor(values$data$Sample, 
                                       levels = valid_levels)
        }
      }
      if(!is.null(values$results)) {
        # Only use levels that exist in the results
        existing_samples <- unique(values$results$Sample)
        valid_levels <- sample_order[sample_order %in% existing_samples]
        if(length(valid_levels) > 0) {
          values$results$Sample <- factor(values$results$Sample, 
                                          levels = valid_levels)
        }
      }
    }
    
    # The plot will automatically re-render due to the reactive trigger in renderPlot
  })
  
  observeEvent(input$excludeSelected, {
    req(values$ordered_data, input$rawDataPreview_rows_selected)
    
    if(length(input$rawDataPreview_rows_selected) > 0) {
      selected_rows <- values$ordered_data %>%
        filter(row_index %in% input$rawDataPreview_rows_selected)
      
      for(i in 1:nrow(selected_rows)) {
        new_exclusion <- list(
          timestamp = Sys.time(),
          type = "exclusion",
          row_num = i,
          target = selected_rows$Target[i],
          sample = selected_rows$Sample[i],
          cq = selected_rows$Cq[i]
        )
        values$edit_history <- c(values$edit_history, list(new_exclusion))
      }
      
      new_exclusions <- selected_rows$row_id
      values$excluded_points <- union(values$excluded_points, new_exclusions)
    }
  })
  
  observeEvent(input$clearExclusions, {
    if(length(values$excluded_points) > 0) {
      clearing_log <- list(
        timestamp = Sys.time(),
        type = "clear_exclusions",
        count = length(values$excluded_points)
      )
      values$edit_history <- c(values$edit_history, list(clearing_log))
    }
    
    values$excluded_points <- character(0)
    
    showNotificationWithLog("Cleared all exclusions", type = "message")
  })
  
  output$excludedPointsSummary <- renderText({
    req(values$data)
    
    summary_parts <- character()
    
    if(length(values$excluded_points) > 0) {
      excluded_data <- values$data %>%
        filter(row_id %in% values$excluded_points) %>%
        group_by(Sample, Target) %>%
        summarise(
          n = n(),
          .groups = 'drop'
        )
      
      if(nrow(excluded_data) > 0) {
        summary_parts <- c(summary_parts,
                           "Currently Excluded Points:",
                           paste(sprintf("  %s-%s (%d points)",
                                         excluded_data$Sample,
                                         excluded_data$Target,
                                         excluded_data$n),
                                 collapse = "\n"))
      }
    }
    
    if(length(values$edit_history) > 0) {
      summary_parts <- c(summary_parts, "\nModification History:")
      
      for(i in length(values$edit_history):1) {
        entry <- values$edit_history[[i]]
        timestamp <- format(entry$timestamp, "%Y-%m-%d %H:%M:%S")
        
        if(entry$type == "edit") {
          log_entry <- sprintf("  %s: Edited %s in row %s (%s, %s) from %s to %s",
                               timestamp,
                               entry$column,
                               entry$row_num,
                               entry$target,
                               entry$sample,
                               entry$old_value,
                               entry$new_value)
        } else if(entry$type == "exclusion") {
          log_entry <- sprintf("  %s: Excluded point - Row %s (%s, %s, Cq=%.2f)",
                               timestamp,
                               entry$row_num,
                               entry$target,
                               entry$sample,
                               entry$cq)
        } else if(entry$type == "clear_exclusions") {
          log_entry <- sprintf("  %s: Cleared %d exclusions",
                               timestamp,
                               entry$count)
        }
        
        summary_parts <- c(summary_parts, log_entry)
      }
    }
    
    if(length(summary_parts) == 0) {
      return("No data modifications")
    }
    
    paste(summary_parts, collapse = "\n\n")
  })
  
  detectHousekeepingGenes <- reactive({
    req(values$data)
    
    if (input$autoDetectHK) {
      pattern <- input$hkPattern
      potential_hk <- levels(values$data$Target)[grep(pattern, levels(values$data$Target), ignore.case = TRUE)]
      
      if (length(potential_hk) == 0) {
        common_hk <- c("RNA18SN5", "RNA18S", "GAPDH", "ACTB", "B2M", "HPRT1", "TBP")
        potential_hk <- levels(values$data$Target)[levels(values$data$Target) %in% common_hk]
      }
      
      values$housekeeping_genes <- potential_hk
      
      if (length(potential_hk) == 0) {
        showNotificationWithLog(
          "No housekeeping genes detected automatically. Please select manually.",
          type = "warning"
        )
      } else {
        showNotificationWithLog(
          paste("Detected housekeeping genes:", paste(potential_hk, collapse = ", ")),
          type = "message"
        )
      }
    } else {
      values$housekeeping_genes <- input$manualHKGenes
    }
  })
  
  observeEvent(c(input$autoDetectHK, input$hkPattern, input$manualHKGenes), {
    detectHousekeepingGenes()
  })
  
  calculateFoldChanges <- function(data, controlSample, housekeeping_genes) {
    if(is.null(data) || nrow(data) == 0) {
      stop("No data available for analysis")
    }
    
    if(is.null(controlSample) || !(controlSample %in% unique(data$Sample))) {
      stop("Invalid control sample selected")
    }
    
    if(is.null(housekeeping_genes) || length(housekeeping_genes) == 0) {
      stop("No housekeeping genes selected")
    }
    
    ref_values <- data %>%
      filter(Target %in% housekeeping_genes) %>%
      group_by(Sample) %>%
      summarise(
        ref_Cq = if(input$useGeometricMean && n() > 1) {
          exp(mean(log(Cq), na.rm = TRUE))
        } else {
          mean(Cq, na.rm = TRUE)
        },
        ref_sd = sd(Cq, na.rm = TRUE),
        .groups = 'drop'
      )
    
    if(nrow(ref_values) == 0) {
      stop("No valid reference values calculated")
    }
    
    dct_values <- data %>%
      filter(!Target %in% housekeeping_genes) %>%
      left_join(ref_values, by = "Sample") %>%
      mutate(
        dCt = Cq - ref_Cq,
        dCt_sd = if_else(is.na(ref_sd), 0, ref_sd)
      )
    
    control_values <- dct_values %>%
      filter(Sample == controlSample) %>%
      group_by(Target) %>%
      summarise(
        control_dCt = mean(dCt, na.rm = TRUE),
        control_sd = sqrt(mean(dCt_sd^2, na.rm = TRUE)),
        .groups = 'drop'
      )
    
    if(nrow(control_values) == 0) {
      stop("No valid control values calculated")
    }
    
    final_results <- dct_values %>%
      left_join(control_values, by = "Target") %>%
      mutate(
        ddCt = dCt - control_dCt,
        individual_fold_change = 2^(-ddCt),
        fold_change_error = sqrt(dCt_sd^2 + control_sd^2)
      ) %>%
      group_by(Target, Sample) %>%
      mutate(
        mean_fold_change = mean(individual_fold_change, na.rm = TRUE),
        sd_fold_change = sd(individual_fold_change, na.rm = TRUE),
        sem_fold_change = sd_fold_change/sqrt(n()),
        n_replicates = n()
      ) %>%
      ungroup() %>%
      arrange(Sample, Target)
    
    return(final_results)
  }
  
  performStatisticalAnalysis <- function(results) {
    req(results)
    
    stats_results <- list()
    
    test_column <- if(input$statsDataType == "dct") "dCt" else "ddCt"
    
    for(target in unique(results$Target)) {
      target_data <- results %>% 
        filter(Target == target)
      
      if(test_column == "dCt") {
        target_data <- target_data %>%
          filter(!is.na(dCt), is.finite(dCt))
      } else {
        target_data <- target_data %>%
          filter(!is.na(ddCt), is.finite(ddCt))
      }
      
      if(nrow(target_data) < 2 || length(unique(target_data$Sample)) < 2) {
        stats_results[[target]] <- list(
          test = "Not performed",
          summary = "Insufficient data for statistical analysis",
          control_comparisons = NULL,
          data_type = test_column
        )
        next
      }
      
      if(input$statsTest == "anova") {
        tryCatch({
          formula_str <- paste(test_column, "~ Sample")
          model <- aov(as.formula(formula_str), data = target_data)
          anova_summary <- summary(model)
          
          if(!is.null(anova_summary[[1]]$"Pr(>F)") && 
             anova_summary[[1]]$"Pr(>F)"[1] < input$pThreshold) {
            
            posthoc <- TukeyHSD(model)
            posthoc_df <- as.data.frame(posthoc$Sample) %>%
              rownames_to_column("comparison") %>%
              mutate(
                significant = `p adj` < input$pThreshold,
                stars = case_when(
                  `p adj` < 0.001 ~ "***",
                  `p adj` < 0.01 ~ "**",
                  `p adj` < 0.05 ~ "*",
                  TRUE ~ "ns"
                )
              )
            
            control_comparisons <- posthoc_df %>%
              filter(grepl(paste0("^", input$controlSample, "-|", 
                                  "-", input$controlSample, "$"), comparison))
          } else {
            posthoc_df <- NULL
            control_comparisons <- NULL
          }
          
          stats_results[[target]] <- list(
            test = "ANOVA",
            summary = anova_summary,
            posthoc = posthoc_df,
            control_comparisons = control_comparisons,
            data_type = test_column,
            p_value = anova_summary[[1]]$"Pr(>F)"[1]
          )
          
        }, error = function(e) {
          message(paste("ANOVA error for target", target, ":", e$message))
          stats_results[[target]] <- list(
            test = "ANOVA failed",
            summary = paste("Error:", e$message),
            control_comparisons = NULL
          )
        })
        
      } else if(input$statsTest == "kruskal") {
        tryCatch({
          formula_str <- paste(test_column, "~ Sample")
          kw_test <- kruskal.test(as.formula(formula_str), data = target_data)
          
          if(!is.null(kw_test$p.value) && kw_test$p.value < input$pThreshold) {
            posthoc <- dunn_test(
              data = target_data,
              formula = as.formula(formula_str),
              p.adjust.method = input$pAdjustMethod
            )
            
            control_comparisons <- posthoc %>%
              filter(group1 == input$controlSample | group2 == input$controlSample)
          } else {
            posthoc <- NULL
            control_comparisons <- NULL
          }
          
          stats_results[[target]] <- list(
            test = "Kruskal-Wallis",
            summary = kw_test,
            posthoc = posthoc,
            control_comparisons = control_comparisons,
            data_type = test_column,
            p_value = kw_test$p.value
          )
          
        }, error = function(e) {
          message(paste("Kruskal-Wallis error for target", target, ":", e$message))
          stats_results[[target]] <- list(
            test = "Kruskal-Wallis failed",
            summary = paste("Error:", e$message),
            control_comparisons = NULL
          )
        })
      }
    }
    
    return(stats_results)
  }
  
  output$rawDataPreview <- DT::renderDataTable({
    req(values$data)
    
    display_data <- values$data %>%
      mutate(
        Cq = as.numeric(as.character(Cq)),
        Status = case_when(
          row_id %in% values$excluded_points ~ "Excluded",
          TRUE ~ "Included"
        )
      ) %>%
      mutate(row_index = seq_len(n()))
    
    values$ordered_data <- display_data
    
    display_table <- display_data %>%
      select(row_index, everything(), -row_id)
    
    DT::datatable(
      display_table,
      selection = 'multiple',
      editable = list(target = 'cell', disable = list(columns = c(0))),  # Disable editing only for row_index column
      extensions = c('Buttons'),
      options = list(
        pageLength = -1,
        scrollX = TRUE,
        scrollY = "600px",
        scroller = TRUE,
        stateSave = FALSE,
        rowId = 'row_index',
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', exportOptions = list(modifier = list(page = 'all'))),
          list(extend = 'csv', exportOptions = list(modifier = list(page = 'all'))),
          list(extend = 'excel', exportOptions = list(modifier = list(page = 'all')))
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Status',
        target = 'row',
        backgroundColor = styleEqual(
          c('Excluded', 'Included'),
          c('#ffebee', 'white')
        )
      ) %>%
      formatRound('Cq', digits = 2)
  })
  
  observeEvent(input$rawDataPreview_cell_edit, {
    info <- input$rawDataPreview_cell_edit
    
    isolate({
      edited_row_index <- info$row
      display_cols <- names(values$ordered_data %>% select(row_index, everything(), -row_id))
      col_name <- display_cols[info$col + 1]
      v <- info$value
      
      if(col_name == "row_index") {
        showNotificationWithLog("Cannot edit row index", type = "warning")
        return()
      }
      
      actual_row_index <- values$ordered_data$row_index[edited_row_index]
      
      if(length(actual_row_index) == 1 && !is.na(actual_row_index)) {
        old_value <- values$data[actual_row_index, col_name]
        old_row_id <- values$data$row_id[actual_row_index]
        
        if(col_name == "Cq") {
          v <- suppressWarnings(as.numeric(as.character(v)))
          
          if(is.na(v) || v < 0 || v > 50) {
            showNotificationWithLog("Invalid Cq value. Please enter a number between 0 and 50.", 
                             type = "error")
            return()
          }
        } else if(col_name %in% c("Sample", "Target")) {
          current_levels <- levels(values$data[[col_name]])
          if(!v %in% current_levels) {
            values$data[[col_name]] <- factor(values$data[[col_name]], 
                                              levels = c(current_levels, v))
          }
        }
        
        values$data[actual_row_index, col_name] <- v
        
        if(col_name %in% c("Sample", "Target", "Cq")) {
          new_row_id <- paste(
            actual_row_index,
            values$data$Target[actual_row_index],
            values$data$Sample[actual_row_index],
            values$data$Cq[actual_row_index],
            sep = "__|__"
          )
          
          if(old_row_id %in% values$excluded_points) {
            values$excluded_points <- setdiff(values$excluded_points, old_row_id)
            values$excluded_points <- c(values$excluded_points, new_row_id)
          }
          
          values$data$row_id[actual_row_index] <- new_row_id
        }
        
        new_edit <- list(
          timestamp = Sys.time(),
          type = "edit",
          row_num = actual_row_index,
          target = values$data[actual_row_index, "Target"],
          sample = values$data[actual_row_index, "Sample"],
          column = col_name,
          old_value = old_value,
          new_value = v
        )
        
        values$edit_history <- c(values$edit_history, list(new_edit))
        
        showNotificationWithLog(sprintf("Updated %s from %s to %s in row %d", 
                                 col_name, old_value, v, actual_row_index), 
                         type = "message")
        
        values$data <- values$data
      } else {
        showNotificationWithLog("Error: Could not locate row to update", type = "error")
      }
    })
  })
 
  
  createQCPlot <- function(data, excluded_points, palette = "classic") {
    p1 <- data %>%
      mutate(
        Status = ifelse(row_id %in% excluded_points, "Excluded", "Included")
      ) %>%
      ggplot(aes(x = Target, y = Cq, fill = Sample)) +
      geom_boxplot(alpha = 0.7, outlier.shape = NA) +
      geom_point(aes(color = Status),
                 position = position_jitterdodge(jitter.width = 0.2),
                 size = 2) +
      scale_color_manual(values = c("Included" = "black", "Excluded" = "red")) +
      theme_prism() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      labs(title = "Quality Control: Cq Values Distribution",
           y = "Cq Value")
    
    p1 <- addColorPalette(p1, palette)
    
    cv_data <- data %>%
      filter(!row_id %in% excluded_points) %>%
      group_by(Target, Sample) %>%
      summarise(
        CV = (sd(Cq, na.rm = TRUE) / mean(Cq, na.rm = TRUE)) * 100,
        .groups = 'drop'
      )
    
    p2 <- ggplot(cv_data, aes(x = Target, y = CV, fill = Sample)) +
      geom_bar(stat = "identity", position = position_dodge(0.9)) +
      geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
      theme_prism() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      labs(title = "Coefficient of Variation (CV)",
           y = "CV (%)") +
      scale_y_continuous(
        limits = function(x) c(0, max(x) * 1.2),
        breaks = function(x) seq(0, ceiling(max(x)), by = 2)
      ) +
      geom_text(
        aes(label = sprintf("%.1f%%", CV)),
        position = position_dodge(0.9),
        vjust = -0.5,
        size = 3
      )
    
    p2 <- addColorPalette(p2, palette)
    
    return(list(p1 = p1, p2 = p2))
  }
  
  createFoldChangePlot <- function(plot_data, values, input) {
    if(nrow(plot_data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "No valid data to plot") +
               theme_void())
    }
    
    if(!is.null(values$sample_order)) {
      # Remove duplicates and ensure valid levels
      sample_order <- unique(values$sample_order)
      existing_samples <- unique(plot_data$Sample)
      valid_levels <- sample_order[sample_order %in% existing_samples]
      
      if(length(valid_levels) > 0) {
        plot_data$Sample <- factor(plot_data$Sample, levels = valid_levels)
      }
    }
    
    # Determine which data to plot based on user selection
    if(input$dataDisplayType == "ddct") {
      plot_data$plot_value <- plot_data$ddCt
      y_label <- expression(Delta * Delta * "Ct")
    } else if(input$dataDisplayType == "neg_ddct") {
      plot_data$plot_value <- -plot_data$ddCt
      y_label <- expression("-" * Delta * Delta * "Ct")
    } else if(input$dataDisplayType == "dct") {
      plot_data$plot_value <- plot_data$dCt
      y_label <- expression(Delta * "Ct")
    } else if(input$dataDisplayType == "neg_dct") {
      plot_data$plot_value <- -plot_data$dCt
      y_label <- expression("-" * Delta * "Ct")
    } else {
      plot_data$plot_value <- plot_data$individual_fold_change
      y_label <- expression("Fold Change (2"^"-" * Delta * Delta * "Ct)")
    }
    
    p <- ggplot(plot_data, aes(x = Sample, y = plot_value)) +
      facet_wrap(~Target, scales = "free", ncol = input$facetCols)
    
    if(input$plotType == "bar") {
      p <- p + stat_summary(aes(fill = Sample),
                            fun = mean,
                            geom = "bar",
                            position = position_dodge(0.9),
                            alpha = 0.7,
                            na.rm = TRUE)
      
      # Add error bars only for bar plots
      if(input$errorBar == "se") {
        p <- p + stat_summary(fun.data = mean_se,
                              geom = "errorbar",
                              width = 0.2,
                              color = "black",
                              na.rm = TRUE)
      } else if(input$errorBar == "sd") {
        p <- p + stat_summary(fun.data = mean_sdl,
                              geom = "errorbar",
                              width = 0.2,
                              color = "black",
                              na.rm = TRUE)
      } else if(input$errorBar == "ci") {
        p <- p + stat_summary(fun.data = mean_cl_normal,
                              geom = "errorbar",
                              width = 0.2,
                              color = "black",
                              na.rm = TRUE)
      }
    } else if(input$plotType == "box") {
      p <- p + geom_boxplot(aes(fill = Sample),
                            alpha = 0.7,
                            outlier.shape = if(input$showIndividualPoints) NA else 19,
                            na.rm = TRUE)
    } else if(input$plotType == "violin") {
      # Violin plot with normal smoothing
      p <- p + geom_violin(aes(fill = Sample),
                           alpha = 0.7,
                           scale = "width",
                           trim = FALSE,
                           na.rm = TRUE)
      # Add boxplot inside violin
      p <- p + geom_boxplot(aes(fill = Sample),
                            width = 0.1,
                            alpha = 0.5,
                            outlier.shape = if(input$showIndividualPoints) NA else 19,
                            na.rm = TRUE)
    } else if(input$plotType == "beeswarm") {
      # Beeswarm plot with mean dash
      if(!requireNamespace("ggbeeswarm", quietly = TRUE)) {
        p <- p + geom_jitter(aes(fill = Sample),
                            width = 0.3,
                            height = 0,
                            size = input$dotSize,
                            alpha = 0.6,
                            shape = 21,  # Filled circle with border
                            color = "black",
                            stroke = 0.2,
                            na.rm = TRUE)
      } else {
        p <- p + ggbeeswarm::geom_beeswarm(aes(fill = Sample),
                                            size = input$dotSize,
                                            alpha = 0.6,
                                            shape = 21,  # Filled circle with border
                                            color = "black",
                                            stroke = 0.2,
                                            na.rm = TRUE)
      }
      # Add mean dash with matching fill color
      p <- p + stat_summary(aes(fill = Sample),
                            fun = mean,
                            geom = "crossbar",
                            width = 0.5,
                            fatten = 2,
                            alpha = 1,  # Full opacity for the mean dash
                            color = "black",  # Black outline
                            na.rm = TRUE)
    }
    
    # Show individual points only if requested and not already shown
    if(input$showIndividualPoints && input$plotType != "beeswarm") {
      if(input$plotType %in% c("box", "violin")) {
        p <- p + geom_jitter(width = 0.1,
                             height = 0,
                             alpha = 0.4,
                             size = input$dotSize * 0.8,
                             color = "black",
                             na.rm = TRUE)
      } else if(input$plotType == "bar") {
        p <- p + geom_jitter(width = 0.2,
                             height = 0,
                             alpha = 0.6,
                             size = input$dotSize,
                             color = "black",
                             na.rm = TRUE)
      }
    }
    
    if(input$showSignificance) {
      p <- addSignificanceMarkers(p, plot_data, values, input)
    }
    
    p <- p +
      theme_prism(base_size = input$fontSize) +
      labs(title = safe_plot_title(input$plotTitle, current_language()),
           y = y_label,
           x = "Sample") +
      theme(
        legend.position = "right",
        strip.text = element_text(size = input$fontSize * 1.2),
        axis.title = element_text(size = input$fontSize * 1.1),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          vjust = 1,
          size = input$fontSize * 0.9
        ),
        axis.text.y = element_text(size = input$fontSize * 0.9),
        legend.text = element_text(size = input$fontSize * 0.9),
        legend.title = element_text(size = input$fontSize),
        plot.title = element_text(size = input$fontSize * 1.3, hjust = 0.5),
        panel.spacing.x = unit(2, "lines"),
        panel.spacing.y = unit(3, "lines")
      )
    
    p <- addColorPalette(p, input$colorPalette)
    
    return(p)
  }
  
  output$foldChangePlot <- renderPlot({
    req(values$results)
    
    # Trigger plot update when update button is clicked
    input$updatePlot
    
    plot_data <- values$results %>%
      filter(!is.na(ddCt), 
             is.finite(ddCt))
    
    if(nrow(plot_data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "No valid data to plot") +
               theme_void())
    }
    
    createFoldChangePlot(plot_data, values, input)
  }, height = function() {
    # Dynamic height based on number of targets and columns
    if(!is.null(values$results)) {
      n_targets <- length(unique(values$results$Target))
      n_cols <- input$facetCols
      n_rows <- ceiling(n_targets / n_cols)
      return(max(400, 300 * n_rows))
    }
    return(600)
  })
  
  output$qcPlot <- renderPlot({
    req(values$data)
    plots <- createQCPlot(values$data, values$excluded_points, input$colorPalette)
    gridExtra::grid.arrange(plots$p1, plots$p2, ncol = 1, heights = c(2, 2))
  }, height = function() {
    # Use consistent sizing based on user input
    return(input$plotHeight * 100)  # Convert inches to pixels (approximate)
  })
  
  output$resultsTable <- DT::renderDataTable({
    
    if (!values$analysis_run) {
      return(NULL)
    }
    
    req(values$results, values$stats_results)
    
    results_table <- values$results %>%
      select(
        Sample, Target, Cq,
        dCt, ddCt, individual_fold_change,
        mean_fold_change, sd_fold_change, sem_fold_change,
        n_replicates
      ) %>%
      arrange(Target, Sample, Cq) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))
    
    DT::datatable(
      results_table,
      extensions = c('Buttons'),
      options = list(
        pageLength = -1,
        scrollX = TRUE,
        scrollY = "600px",
        scroller = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', exportOptions = list(modifier = list(page = 'all'))),
          list(extend = 'csv', exportOptions = list(modifier = list(page = 'all'))),
          list(extend = 'excel', exportOptions = list(modifier = list(page = 'all')))
        )
      ),
      rownames = FALSE
    )
  })
  
  
  output$statsOutput <- renderText({
    req(values$stats_results)
    
    output_text <- character()
    
    data_type_label <- if(input$statsDataType == "dct") "ΔCt values" else "ΔΔCt values"
    output_text <- c(output_text,
                     paste("=== STATISTICAL ANALYSIS REPORT ===\n"),
                     paste("Data tested:", data_type_label),
                     paste("Statistical test:", ifelse(input$statsTest == "anova", "One-way ANOVA", "Kruskal-Wallis")),
                     paste("P-value adjustment:", input$pAdjustMethod),
                     paste("Significance threshold:", input$pThreshold),
                     paste("\n"))
    
    for(target in names(values$stats_results)) {
      result <- values$stats_results[[target]]
      
      output_text <- c(output_text,
                       paste("\n=== Analysis for", target, "===\n"))
      
      if(result$test == "Not performed") {
        output_text <- c(output_text, result$summary)
      } else if(result$test == "ANOVA") {
        output_text <- c(output_text,
                         paste("Test performed on:", result$data_type, "values"),
                         paste("Overall ANOVA p-value:", format.pval(result$p_value)),
                         "\nANOVA Summary:",
                         capture.output(print(result$summary)),
                         "\nPost-hoc Analysis (Tukey HSD vs. Control):",
                         if(!is.null(result$control_comparisons)) {
                           capture.output(print(result$control_comparisons))
                         } else {
                           "No significant differences detected"
                         })
      } else if(result$test == "Kruskal-Wallis") {
        output_text <- c(output_text,
                         paste("Test performed on:", result$data_type, "values"),
                         "\nKruskal-Wallis Test:",
                         paste("chi-squared =", round(result$summary$statistic, 4)),
                         paste("degrees of freedom =", result$summary$parameter),
                         paste("p-value =", format.pval(result$summary$p.value)),
                         "\nPost-hoc Analysis (Dunn's test vs. Control):",
                         if(!is.null(result$control_comparisons)) {
                           capture.output(print(result$control_comparisons))
                         } else {
                           "No significant differences detected"
                         })
      }
    }
    
    paste(output_text, collapse = "\n")
  })
  
  output$downloadAllResults <- downloadHandler(
    filename = function() {
      req(values$analysis_run)
      paste0("qPCR_complete_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      req(values$analysis_run, values$data, values$results, values$stats_results)
      
      # Create temporary directory for files
      temp_dir <- tempdir()
      temp_files <- c()
      
      # Create Excel workbook
      wb <- createWorkbook()
      
      addWorksheet(wb, "1_Raw_Data")
      writeData(wb, "1_Raw_Data", values$data %>% 
                  select(-row_id) %>%
                  arrange(Sample, Target))
      
      addWorksheet(wb, "2_Housekeeping_Data")
      hk_data <- values$data %>%
        filter(Target %in% values$housekeeping_genes) %>%
        group_by(Sample, Target) %>%
        summarise(
          mean_Cq = mean(Cq, na.rm = TRUE),
          sd_Cq = sd(Cq, na.rm = TRUE),
          cv_percent = (sd_Cq / mean_Cq) * 100,
          n_replicates = n(),
          .groups = 'drop'
        )
      writeData(wb, "2_Housekeeping_Data", hk_data)
      
      addWorksheet(wb, "3_Quality_Control")
      qc_data <- values$data %>%
        group_by(Target, Sample) %>%
        summarise(
          n_replicates = n(),
          mean_Cq = mean(Cq, na.rm = TRUE),
          sd_Cq = sd(Cq, na.rm = TRUE),
          cv_percent = (sd_Cq / mean_Cq) * 100,
          min_Cq = min(Cq, na.rm = TRUE),
          max_Cq = max(Cq, na.rm = TRUE),
          range_Cq = max_Cq - min_Cq,
          quality_flag = ifelse(cv_percent > 5, "High CV", "OK"),
          .groups = 'drop'
        )
      writeData(wb, "3_Quality_Control", qc_data)
      
      addWorksheet(wb, "4_Analysis_Results")
      writeData(wb, "4_Analysis_Results", values$results %>%
                  select(-row_id) %>%
                  arrange(Target, Sample))
      
      addWorksheet(wb, "5_Summary_Results")
      summary_results <- values$results %>%
        filter(!row_id %in% values$excluded_points) %>%
        group_by(Target, Sample) %>%
        summarise(
          n_replicates = n(),
          mean_Cq = mean(Cq, na.rm = TRUE),
          sd_Cq = sd(Cq, na.rm = TRUE),
          cv_percent = (sd_Cq / mean_Cq) * 100,
          mean_dCt = mean(dCt, na.rm = TRUE),
          mean_ddCt = mean(ddCt, na.rm = TRUE),
          fold_change = mean(individual_fold_change, na.rm = TRUE),
          fold_change_sem = sd(individual_fold_change, na.rm = TRUE) / sqrt(n()),
          .groups = 'drop'
        )
      writeData(wb, "5_Summary_Results", summary_results)
      
      # Statistical Summary
      addWorksheet(wb, "6_Statistical_Summary")
      stat_summary_list <- list()
      stat_comparisons_list <- list()
      
      for(target in names(values$stats_results)) {
        result <- values$stats_results[[target]]
        if(result$test != "Not performed") {
          stat_summary <- data.frame(
            Target = target,
            Test = result$test,
            DataType = result$data_type,
            P_Value = if(!is.null(result$p_value)) result$p_value else 
                      if(!is.null(result$summary$p.value)) result$summary$p.value else NA,
            stringsAsFactors = FALSE
          )
          stat_summary_list[[target]] <- stat_summary
          
          if(!is.null(result$control_comparisons)) {
            comp_df <- as.data.frame(result$control_comparisons)
            comp_df$Target <- target
            stat_comparisons_list[[target]] <- comp_df
          }
        }
      }
      
      if(length(stat_summary_list) > 0) {
        all_summaries <- do.call(rbind, stat_summary_list)
        writeData(wb, "6_Statistical_Summary", all_summaries)
      } else {
        writeData(wb, "6_Statistical_Summary", data.frame(Note = "No statistical tests performed"))
      }
      
      # Statistical Comparisons (separate sheet)
      if(length(stat_comparisons_list) > 0) {
        addWorksheet(wb, "7_Statistical_Comparisons")
        all_comparisons <- do.call(rbind, stat_comparisons_list)
        writeData(wb, "7_Statistical_Comparisons", all_comparisons)
      }
      
      addWorksheet(wb, "8_Modification_History")
      if(length(values$edit_history) > 0) {
        mod_history <- do.call(rbind, lapply(values$edit_history, function(x) {
          data.frame(
            Timestamp = format(x$timestamp, "%Y-%m-%d %H:%M:%S"),
            Type = x$type,
            Details = if(x$type == "edit") {
              sprintf("Edited %s in row %s (%s, %s) from %s to %s",
                      x$column, x$row_num, x$target, x$sample, x$old_value, x$new_value)
            } else if(x$type == "exclusion") {
              sprintf("Excluded point - Row %s (%s, %s, Cq=%.2f)",
                      x$row_num, x$target, x$sample, x$cq)
            } else {
              sprintf("Cleared %d exclusions", x$count)
            }
          )
        }))
        writeData(wb, "8_Modification_History", mod_history)
      } else {
        writeData(wb, "8_Modification_History", "No modifications recorded")
      }
      
      addWorksheet(wb, "9_Analysis_Parameters")
      analysis_params <- data.frame(
        Parameter = c(
          "Analysis Date",
          "Control Sample",
          "Housekeeping Genes",
          "HK Detection Method",
          "HK Detection Pattern",
          "Geometric Mean for HK",
          "Excluded Points",
          "Statistical Test",
          "Statistical Data Type",
          "P-value Adjustment Method",
          "Significance Threshold",
          "Data Display Type",
          "Plot Type",
          "Error Bar Type",
          "Show Individual Points",
          "Show Statistical Significance",
          "Color Palette",
          "Font Size",
          "Point Size"
        ),
        Value = c(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          input$controlSample,
          paste(values$housekeeping_genes, collapse = ", "),
          ifelse(input$autoDetectHK, "Auto-detect", "Manual"),
          ifelse(input$autoDetectHK, input$hkPattern, "N/A"),
          ifelse(input$useGeometricMean, "Yes", "No"),
          length(values$excluded_points),
          ifelse(input$statsTest == "anova", "One-way ANOVA", "Kruskal-Wallis"),
          ifelse(input$statsDataType == "ddct", "ΔΔCt Values", "ΔCt Values"),
          input$pAdjustMethod,
          input$pThreshold,
          switch(input$dataDisplayType, 
                 "fold_change" = "Fold Change (2^-ΔΔCt)",
                 "ddct" = "ΔΔCt Values",
                 "neg_ddct" = "-ΔΔCt Values",
                 "dct" = "ΔCt Values",
                 "neg_dct" = "-ΔCt Values"),
          switch(input$plotType, 
                 "bar" = "Bar Plot",
                 "box" = "Box Plot",
                 "violin" = "Violin Plot",
                 "beeswarm" = "Beeswarm Plot"),
          if(input$plotType == "bar") {
            switch(input$errorBar,
                   "se" = "Standard Error",
                   "sd" = "Standard Deviation",
                   "ci" = "95% Confidence Interval")
          } else {
            "N/A"
          },
          ifelse(input$showIndividualPoints, "Yes", "No"),
          ifelse(input$showSignificance, "Yes", "No"),
          input$colorPalette,
          input$fontSize,
          input$dotSize
        )
      )
      writeData(wb, "9_Analysis_Parameters", analysis_params)
      
      for(sheet in names(wb)) {
        addStyle(wb, sheet, createStyle(textDecoration = "bold"), rows = 1, cols = 1:50)
        setColWidths(wb, sheet, cols = 1:50, widths = "auto")
      }
      
      # Save Excel file
      excel_file <- file.path(temp_dir, "qPCR_analysis_data.xlsx")
      saveWorkbook(wb, excel_file, overwrite = TRUE)
      temp_files <- c(temp_files, excel_file)
      
      # Generate QC plots
      if(!is.null(values$data)) {
        qc_plots <- createQCPlot(values$data, values$excluded_points, input$colorPalette)
        
        # Use fallback values if inputs are NULL
        plot_width <- if(is.null(input$plotWidth) || input$plotWidth == 0) 10 else input$plotWidth
        plot_height <- if(is.null(input$plotHeight) || input$plotHeight == 0) 8 else input$plotHeight
        
        # Save QC plots as PNG
        qc_png_file <- file.path(temp_dir, "QC_plots.png")
        png(qc_png_file, width = plot_width * 300, height = plot_height * 300, res = 300)
        gridExtra::grid.arrange(qc_plots$p1, qc_plots$p2, ncol = 1)
        dev.off()
        temp_files <- c(temp_files, qc_png_file)
        
        # Save QC plots as PDF
        qc_pdf_file <- file.path(temp_dir, "QC_plots.pdf")
        pdf(qc_pdf_file, width = plot_width, height = plot_height)
        gridExtra::grid.arrange(qc_plots$p1, qc_plots$p2, ncol = 1)
        dev.off()
        temp_files <- c(temp_files, qc_pdf_file)
      }
      
      # Generate main analysis plot
      if(!is.null(values$results)) {
        plot_data <- values$results %>%
          filter(!is.na(ddCt), is.finite(ddCt))
        
        if(nrow(plot_data) > 0) {
          main_plot <- createFoldChangePlot(plot_data, values, input)
          
          # Use same fallback values as QC plots
          plot_width <- if(is.null(input$plotWidth) || input$plotWidth == 0) 10 else input$plotWidth
          plot_height <- if(is.null(input$plotHeight) || input$plotHeight == 0) 8 else input$plotHeight
          
          # Save main plot as PNG - using user's plot settings with fallbacks
          main_png_file <- file.path(temp_dir, "Analysis_plot.png")
          png(main_png_file, width = plot_width * 300, height = plot_height * 300, res = 300)
          print(main_plot)
          dev.off()
          temp_files <- c(temp_files, main_png_file)
          
          # Save main plot as PDF - using user's plot settings with fallbacks
          main_pdf_file <- file.path(temp_dir, "Analysis_plot.pdf")
          pdf(main_pdf_file, width = plot_width, height = plot_height)
          print(main_plot)
          dev.off()
          temp_files <- c(temp_files, main_pdf_file)
        }
      }
      
      # Create zip file
      if (length(temp_files) > 0) {
        # Change to temp directory for relative paths
        old_wd <- getwd()
        setwd(temp_dir)
        
        # Get just the file names (not full paths)
        file_names <- basename(temp_files)
        
        # Create zip using base R
        utils::zip(file, file_names)
        
        # Restore working directory
        setwd(old_wd)
      } else {
        # Fallback if no files were created
        file.create(file)
      }
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      req(values$analysis_run)
      paste0("qPCR_analysis_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$plotFormat)
    },
    content = function(file) {
      req(values$analysis_run, values$results)
      
      # Use fallback values if inputs are NULL
      plot_width <- if(is.null(input$plotWidth) || input$plotWidth == 0) 10 else input$plotWidth
      plot_height <- if(is.null(input$plotHeight) || input$plotHeight == 0) 8 else input$plotHeight
      
      if(input$plotFormat == "pdf") {
        pdf(file, width = plot_width, height = plot_height)
      } else {
        png(file, width = plot_width * 300, height = plot_height * 300, res = 300)
      }
      
      plot_data <- values$results %>%
        filter(!is.na(ddCt), 
               is.finite(ddCt))
      
      print(createFoldChangePlot(plot_data, values, input))
      
      dev.off()
    }
  )
  
  
  validateDataCompleteness <- function(data) {
    validation_messages <- character()
    
    missing_values <- colSums(is.na(data))
    if(any(missing_values > 0)) {
      validation_messages <- c(
        validation_messages,
        paste("Missing values found in columns:",
              paste(names(missing_values[missing_values > 0]), collapse = ", "))
      )
    }
    
    replicate_counts <- data %>%
      group_by(Target, Sample) %>%
      summarise(n = n(), .groups = 'drop')
    
    if(any(replicate_counts$n < 3)) {
      low_replicates <- replicate_counts %>%
        filter(n < 3)
      validation_messages <- c(
        validation_messages,
        paste("Warning: Less than 3 replicates found for:",
              paste(paste(low_replicates$Target, low_replicates$Sample,
                          sep = "-"), collapse = ", "))
      )
    }
    
    if(any(data$Cq < 0 | data$Cq > 40)) {
      validation_messages <- c(
        validation_messages,
        "Warning: Cq values outside expected range (0-40) detected"
      )
    }
    
    return(validation_messages)
  }
  
  addSignificanceMarkers <- function(p, plot_data, values, input) {
    for(target in names(values$stats_results)) {
      target_data <- plot_data %>% filter(Target == target)
      
      if(nrow(target_data) > 0) {
        result <- values$stats_results[[target]]
        
        # Calculate y position for significance markers
        # Get the max value considering error bars if shown
        summary_data <- target_data %>%
          group_by(Sample) %>%
          summarise(
            mean_val = mean(plot_value, na.rm = TRUE),
            se_val = sd(plot_value, na.rm = TRUE) / sqrt(n()),
            sd_val = sd(plot_value, na.rm = TRUE),
            .groups = 'drop'
          )
        
        # Determine the upper bound based on error bar type
        if(input$plotType == "bar" && input$errorBar == "se") {
          max_y <- max(summary_data$mean_val + summary_data$se_val, na.rm = TRUE)
        } else if(input$plotType == "bar" && input$errorBar == "sd") {
          max_y <- max(summary_data$mean_val + summary_data$sd_val, na.rm = TRUE)
        } else if(input$plotType == "bar" && input$errorBar == "ci") {
          max_y <- max(summary_data$mean_val + 1.96 * summary_data$se_val, na.rm = TRUE)
        } else {
          # For non-bar plots or no error bars, use raw max
          max_y <- max(target_data$plot_value, na.rm = TRUE)
        }
        
        # Position significance markers above the bars/points
        y_position <- max_y * 1.05  # Start just above the highest point
        
        if(result$test == "ANOVA" && !is.null(result$posthoc)) {
          control_comparisons <- result$posthoc %>%
            filter(grepl(input$controlSample, comparison)) %>%
            mutate(
              group1 = input$controlSample,
              group2 = gsub(paste0(input$controlSample, "-|\\-", input$controlSample), "", comparison),
              annotation = case_when(
                input$significanceType == "stars" ~ stars,
                input$significanceType == "p.value" ~ sprintf("p=%.3f", `p adj`),
                input$significanceType == "both" ~ sprintf("%s\np=%.3f", stars, `p adj`),
                TRUE ~ stars
              )
            )
          
          if(!input$showNonSignificant) {
            control_comparisons <- control_comparisons %>% filter(significant == TRUE)
          }
          
          if(nrow(control_comparisons) > 0) {
            # Calculate spacing based on plot height
            y_spacing <- (max_y - min(target_data$plot_value, na.rm = TRUE)) * 0.08
            y_positions <- seq(y_position, 
                               y_position + (nrow(control_comparisons) - 1) * y_spacing, 
                               by = y_spacing)
            
            for(i in 1:nrow(control_comparisons)) {
              if(control_comparisons$significant[i] || input$showNonSignificant) {
                p <- p + geom_signif(
                  data = target_data,
                  comparisons = list(c(control_comparisons$group1[i], 
                                       control_comparisons$group2[i])),
                  annotations = control_comparisons$annotation[i],
                  y_position = y_positions[i],
                  tip_length = 0.01,
                  vjust = 0.1,
                  margin_top = 0.2,
                )
              }
            }
          }
        }
      }
    }
    return(p)
  }
  
  formatPValue <- function(p_value) {
    case_when(
      p_value < 0.001 ~ "< 0.001",
      p_value < 0.01 ~ sprintf("%.3f", p_value),
      p_value < 0.05 ~ sprintf("%.3f", p_value),
      TRUE ~ sprintf("%.3f", p_value)
    )
  }
  
  addColorPalette <- function(p, palette) {
    if(palette == "classic") {
      p + scale_fill_manual(values = ggthemes::colorblind_pal()(8))
    } else if(palette == "bw") {
      n_groups <- length(unique(p$data$Sample))
      if(n_groups <= 2) {
        p + scale_fill_manual(values = c("black", "white"))
      } else if(n_groups <= 4) {
        p + scale_fill_manual(values = c("black", "gray40", "gray70", "white"))
      } else {
        grays <- gray.colors(n_groups, start = 0, end = 0.9)
        p + scale_fill_manual(values = grays)
      }
    } else if(palette == "grey") {
      p + scale_fill_grey(start = 0.3, end = 0.7)
    } else if(palette == "viridis") {
      p + scale_fill_viridis_d()
    } else if(palette == "npg") {
      p + scale_fill_manual(values = ggsci::pal_npg()(10))
    } else if(palette == "aaas") {
      p + scale_fill_manual(values = ggsci::pal_aaas()(10))
    } else if(palette == "nejm") {
      p + scale_fill_manual(values = ggsci::pal_nejm()(8))
    } else if(palette == "lancet") {
      p + scale_fill_manual(values = ggsci::pal_lancet()(9))
    } else if(palette == "jama") {
      p + scale_fill_manual(values = ggsci::pal_jama()(7))
    } else if(palette == "jco") {
      p + scale_fill_manual(values = ggsci::pal_jco()(10))
    } else if(palette == "ucscgb") {
      p + scale_fill_manual(values = ggsci::pal_ucscgb()(10))
    } else if(palette == "d3") {
      p + scale_fill_manual(values = ggsci::pal_d3()(10))
    } else if(palette == "igv") {
      p + scale_fill_manual(values = ggsci::pal_igv()(10))
    } else if(palette == "material") {
      p + scale_fill_manual(values = ggsci::pal_material()(10))
    } else if(palette == "economist") {
      p + scale_fill_economist()
    } else if(palette == "fivethirtyeight") {
      p + scale_fill_fivethirtyeight()
    } else if(palette == "tableau") {
      p + scale_fill_tableau()
    } else if(palette == "stata") {
      p + scale_fill_stata()
    } else if(palette == "excel") {
      p + scale_fill_excel_new()
    } else if(palette == "wsj") {
      p + scale_fill_wsj()
    } else if(palette == "calc") {
      p + scale_fill_calc()
    } else if(palette == "hc") {
      p + scale_fill_hc()
    } else if(palette == "pander") {
      p + scale_fill_pander()
    } else if(palette == "set1") {
      p + scale_fill_brewer(palette = "Set1")
    } else if(palette == "set2") {
      p + scale_fill_brewer(palette = "Set2")
    } else if(palette == "set3") {
      p + scale_fill_brewer(palette = "Set3")
    } else if(palette == "dark2") {
      p + scale_fill_brewer(palette = "Dark2")
    } else if(palette == "paired") {
      p + scale_fill_brewer(palette = "Paired")
    } else {
      p + scale_fill_grey(start = 0.2, end = 0.8)
    }
  }
  
  session$onSessionEnded(function() {
    if (exists("values")) {
      isolate({
        values$data <- NULL
        values$excluded_points <- NULL
        values$results <- NULL
        values$stats_results <- NULL
      })
    }
  })
})