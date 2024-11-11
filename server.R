# server.R
library(shiny)
library(tidyverse)
library(ggplot2)
library(rstatix)
library(gridExtra)
library(DT)
library(ggprism)
library(ggsignif)
library(viridis)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(openxlsx)
library(sortable)

shinyServer(function(input, output, session) {
  # Reactive values for storing data and results
  values <- reactiveValues(
    data = NULL,
    excluded_points = character(0),
    housekeeping_genes = NULL,
    validation_messages = NULL,
    results = NULL,
    stats_results = NULL,
    sample_order = NULL,
    edit_history=list(),
    analysis_run = FALSE
  )
  
  output$analysisMessage <- renderUI({
    if (!values$analysis_run) {
      div(
        class = "alert alert-warning",
        style = "margin-top: 20px;",
        tags$h4("Analysis Not Run"),
        "Please run the analysis first using the 'Run Analysis' button in the sidebar."
      )
    }
  })
  
  # analysis handler
  observeEvent(input$runAnalysis, {
    req(values$data, input$controlSample, values$housekeeping_genes)
    
    withProgress(message = 'Running analysis...', {
      # Get data excluding excluded points
      analysis_data <- values$data %>% 
        filter(!row_id %in% values$excluded_points)
      
      if(nrow(analysis_data) == 0) {
        showNotification("No valid data for analysis after exclusions", type = "error")
        values$analysis_run <- FALSE
        return()
      }
      
      tryCatch({
        # Calculate results
        results <- calculateFoldChanges(
          data = analysis_data,
          controlSample = input$controlSample,
          housekeeping_genes = values$housekeeping_genes
        )
        
        # Store results
        values$results <- results
        
        # Run statistical analysis only if we have valid results
        if(!is.null(results) && nrow(results) > 0) {
          values$stats_results <- performStatisticalAnalysis(results)
          values$analysis_run <- TRUE  # Set analysis status to true
          showNotification("Analysis complete!", type = "message")
        } else {
          values$analysis_run <- FALSE
          showNotification("Analysis produced no results", type = "warning")
        }
        
      }, error = function(e) {
        values$analysis_run <- FALSE
        showNotification(paste("Analysis error:", e$message), type = "error")
      })
    })
  })
  
  # download results handler
  output$downloadAllResultsButton <- renderUI({
    if (!values$analysis_run) {
      # If analysis hasn't been run, show disabled button with tooltip
      tags$div(
        style = "position: relative;",
        tags$div(
          title = "Please run analysis first",
          downloadButton("downloadAllResults", "Download All Results",
                         class = "btn-success disabled",
                         style = "width: 100%; opacity: 0.65;")
        )
      )
    } else {
      # If analysis has been run, show enabled button
      downloadButton("downloadAllResults", "Download All Results",
                     class = "btn-success",
                     style = "width: 100%;")
    }
  })
  
  # Validation function
  validateData <- function(data) {
    if(is.null(data) || nrow(data) == 0) {
      return("No data available")
    }
    
    required_cols <- c("Sample", "Target", "Cq")
    missing_cols <- required_cols[!required_cols %in% names(data)]
    
    if(length(missing_cols) > 0) {
      return(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    }
    
    # Check for valid numeric Cq values
    invalid_cq <- sum(is.na(data$Cq) | !is.finite(data$Cq))
    if(invalid_cq > 0) {
      return(paste("Found", invalid_cq, "invalid Cq values"))
    }
    
    # Check for minimum number of samples and targets
    if(length(unique(data$Sample)) < 2) {
      return("Need at least 2 different samples for analysis")
    }
    
    if(length(unique(data$Target)) < 2) {
      return("Need at least 2 different targets for analysis")
    }
    
    return(NULL)  # NULL indicates validation passed
  }
  
  # File upload handler with enhanced data processing
  observeEvent(input$file, {
    req(input$file)
    values$analysis_run <- FALSE

    tryCatch({
      # Read the file with explicit line ending handling
      con <- file(input$file$datapath, "r")
      all_lines <- readLines(con, warn = FALSE)
      close(con)
      
      # Find where the actual data headers start
      header_index <- which(grepl("Target,Content,Sample,Cq|Sample,Target,Cq", all_lines))[1]
      
      if (is.na(header_index)) {
        showNotification("Could not find data headers in file", type = "error")
        return()
      }
      
      # Read only from the header line onwards
      data_lines <- all_lines[header_index:length(all_lines)]
      temp_file <- tempfile(fileext = ".csv")
      writeLines(data_lines, temp_file)
      
      # Read the cleaned data
      data <- read.csv(temp_file, stringsAsFactors = FALSE)
      
      # Remove any completely empty rows
      data <- data[rowSums(is.na(data) | data == "") != ncol(data), ]
      
      # Clean and sort the data with explicit type conversion
      data <- data %>%
        filter(!is.na(Sample) & Sample != "" & 
                 !is.na(Target) & Target != "" & 
                 !is.na(Cq) & Cq != "") %>%
        mutate(
          Sample = factor(Sample),
          Target = factor(Target),
          Cq = as.numeric(as.character(Cq)),  # Ensure Cq is numeric
          # Create a unique identifier using all relevant information
          row_id = paste(Well, Target, Sample, Cq, sep = "__|__")
        ) %>%
        arrange(Sample, Target)
      
      # Remove any rows where Cq conversion failed
      invalid_cq <- is.na(data$Cq)
      if(any(invalid_cq)) {
        invalid_rows <- which(invalid_cq)
        showNotification(
          sprintf("Removed %d rows with invalid Cq values", length(invalid_rows)),
          type = "warning"
        )
        data <- data[!invalid_cq, ]
      }
      
      # Initialize sample order
      values$sample_order <- levels(data$Sample)
      
      # Validate data
      validation_result <- validateData(data)
      if(!is.null(validation_result)) {
        showNotification(validation_result, type = "error")
        return()
      }
      
      # Store cleaned data
      values$data <- data
      
      # Update control sample selection
      updateSelectInput(session, "controlSample",
                        choices = levels(data$Sample),
                        selected = levels(data$Sample)[1])
      
      # Update manual housekeeping gene selection
      updateSelectInput(session, "manualHKGenes",
                        choices = levels(data$Target))
      
      # Auto-detect housekeeping genes if enabled
      detectHousekeepingGenes()
      
      # Show success message
      showNotification(
        paste("Successfully loaded", nrow(data), "data points"),
        type = "message"
      )
      
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })
  
  # Sample order UI
  output$sampleOrderUI <- renderUI({
    req(values$data)
    sortable::rank_list(
      text = "Drag to reorder samples",
      labels = unique(values$data$Sample),
      input_id = "sample_order_list"
    )
  })
  
  # Update sample order
  observeEvent(input$updateOrder, {
    req(input$sample_order_list)
    values$sample_order <- input$sample_order_list
    
    updateTextInput(session, "plotTitle",
                    value = "Relative Expression Analysis")
    
    # Update the Sample factor in data and results as before
    if(!is.null(values$data)) {
      values$data$Sample <- factor(values$data$Sample, 
                                   levels = values$sample_order)
    }
    if(!is.null(values$results)) {
      values$results$Sample <- factor(values$results$Sample, 
                                      levels = values$sample_order)
    }
  })
  
  # exclude selected and clear exclusion
  observeEvent(input$excludeSelected, {
    req(values$ordered_data, input$rawDataPreview_rows_selected)
    
    if(length(input$rawDataPreview_rows_selected) > 0) {
      selected_rows <- values$ordered_data %>%
        filter(row_index %in% input$rawDataPreview_rows_selected)
      
      # Log each exclusion
      for(i in 1:nrow(selected_rows)) {
        new_exclusion <- list(
          timestamp = Sys.time(),
          type = "exclusion",
          well = selected_rows$Well[i],
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
  
  # Add clear exclusions observer
  observeEvent(input$clearExclusions, {
    print("Clearing all exclusions")
    print("Previous excluded points:")
    print(values$excluded_points)
    
    if(length(values$excluded_points) > 0) {
      # Log the clearing of exclusions
      clearing_log <- list(
        timestamp = Sys.time(),
        type = "clear_exclusions",
        count = length(values$excluded_points)
      )
      values$edit_history <- c(values$edit_history, list(clearing_log))
    }
    
    values$excluded_points <- character(0)
    
    print("After clearing:")
    print(values$excluded_points)
    
    showNotification("Cleared all exclusions", type = "message")
  })
  
  output$excludedPointsSummary <- renderText({
    req(values$data)
    
    # Initialize summary text
    summary_parts <- character()
    
    # 1. Current Exclusions Summary
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
    
    # 2. Edit History
    if(length(values$edit_history) > 0) {
      summary_parts <- c(summary_parts, "\nModification History:")
      
      # Process each edit/exclusion in reverse chronological order
      for(i in length(values$edit_history):1) {
        entry <- values$edit_history[[i]]
        timestamp <- format(entry$timestamp, "%Y-%m-%d %H:%M:%S")
        
        if(entry$type == "edit") {
          log_entry <- sprintf("  %s: Edited %s in Well %s (%s, %s) from %s to %s",
                               timestamp,
                               entry$column,
                               entry$well,
                               entry$target,
                               entry$sample,
                               entry$old_value,
                               entry$new_value)
        } else if(entry$type == "exclusion") {
          log_entry <- sprintf("  %s: Excluded point - Well %s (%s, %s, Cq=%.2f)",
                               timestamp,
                               entry$well,
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
    
    # If no modifications at all
    if(length(summary_parts) == 0) {
      return("No data modifications")
    }
    
    # Combine all parts with proper spacing
    paste(summary_parts, collapse = "\n\n")
  })
  
  # Server Part 2: Data Analysis and Calculations
  
  # Housekeeping gene detection
  detectHousekeepingGenes <- reactive({
    req(values$data)
    
    if (input$autoDetectHK) {
      # Auto-detect based on pattern
      pattern <- input$hkPattern
      potential_hk <- levels(values$data$Target)[grep(pattern, levels(values$data$Target), ignore.case = TRUE)]
      
      # If no housekeeping genes found with pattern, check for common names
      if (length(potential_hk) == 0) {
        common_hk <- c("RNA18SN5", "RNA18S", "GAPDH", "ACTB", "B2M", "HPRT1", "TBP")
        potential_hk <- levels(values$data$Target)[levels(values$data$Target) %in% common_hk]
      }
      
      values$housekeeping_genes <- potential_hk
      
      if (length(potential_hk) == 0) {
        showNotification(
          "No housekeeping genes detected automatically. Please select manually.",
          type = "warning"
        )
      } else {
        showNotification(
          paste("Detected housekeeping genes:", paste(potential_hk, collapse = ", ")),
          type = "message"
        )
      }
    } else {
      values$housekeeping_genes <- input$manualHKGenes
    }
  })
  
  # Observe changes in housekeeping gene settings
  observeEvent(c(input$autoDetectHK, input$hkPattern, input$manualHKGenes), {
    detectHousekeepingGenes()
  })
  
  # Modified fold change calculation function with detailed intermediate values
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
    
    # Calculate reference values from housekeeping genes
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
    
    # Ensure we have reference values for all samples
    if(nrow(ref_values) == 0) {
      stop("No valid reference values calculated")
    }
    
    # Calculate dCt values
    dct_values <- data %>%
      filter(!Target %in% housekeeping_genes) %>%
      left_join(ref_values, by = "Sample") %>%
      mutate(
        dCt = Cq - ref_Cq,
        dCt_sd = if_else(is.na(ref_sd), 0, ref_sd)  # Handle single replicates
      )
    
    # Calculate control values
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
    
    # Calculate final results with all intermediate values
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
    
    for(target in unique(results$Target)) {
      target_data <- results %>% 
        filter(Target == target) %>%
        filter(!is.na(individual_fold_change), 
               is.finite(individual_fold_change))
      
      if(nrow(target_data) < 2 || length(unique(target_data$Sample)) < 2) {
        stats_results[[target]] <- list(
          test = "Not performed",
          summary = "Insufficient data for statistical analysis",
          control_comparisons = NULL
        )
        next
      }
      
      if(input$statsTest == "anova") {
        tryCatch({
          model <- aov(individual_fold_change ~ Sample, data = target_data)
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
            control_comparisons = control_comparisons
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
          kw_test <- kruskal.test(individual_fold_change ~ Sample, data = target_data)
          
          if(!is.null(kw_test$p.value) && kw_test$p.value < input$pThreshold) {
            posthoc <- dunn_test(
              data = target_data,
              formula = individual_fold_change ~ Sample,
              p.adjust.method = "bonferroni"
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
            control_comparisons = control_comparisons
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
      select(-row_id)
    
    DT::datatable(
      display_table,
      selection = 'multiple',
      editable = TRUE,
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
  
  # Complete cell edit observer
  observeEvent(input$rawDataPreview_cell_edit, {
    info <- input$rawDataPreview_cell_edit
    print("Cell edit info:")
    print(info)
    
    isolate({
      # Get the edited row index and column
      edited_row_index <- info$row
      col_name <- names(values$ordered_data)[info$col + 1]  # Convert from 0-based to 1-based index
      v <- info$value
      
      print(paste("Edited row index:", edited_row_index))
      print(paste("Editing column:", col_name))
      print(paste("New value:", v))
      
      # Find the corresponding row using row_index
      original_row <- which(values$ordered_data$row_index == edited_row_index)
      
      print(paste("Original row index:", original_row))
      print("Original data row:")
      print(values$data[original_row, ])
      
      if(length(original_row) == 1) {
        # Store old value and row_id for logging
        old_value <- values$data[original_row, col_name]
        old_row_id <- values$data$row_id[original_row]
        
        # Handle different column types
        if(col_name == "Cq") {
          # Convert and validate Cq value
          v <- tryCatch({
            as.numeric(as.character(v))
          }, warning = function(w) NA, error = function(e) NA)
          
          if(is.na(v) || v < 0 || v > 50) {
            showNotification("Invalid Cq value. Please enter a number between 0 and 50.", 
                             type = "error")
            return()
          }
        } else if(col_name %in% c("Sample", "Target")) {
          # For factor columns, check if the new value exists in levels
          current_levels <- levels(values$data[[col_name]])
          if(!v %in% current_levels) {
            values$data[[col_name]] <- factor(values$data[[col_name]], 
                                              levels = c(current_levels, v))
          }
        }
        
        # Update the value
        values$data[original_row, col_name] <- v
        
        print("After update:")
        print(values$data[original_row, ])
        
        # Update row_id if key fields changed
        if(col_name %in% c("Sample", "Target", "Cq")) {
          # Create new row_id using current values
          new_row_id <- paste(
            values$data$Well[original_row],
            values$data$Target[original_row],
            values$data$Sample[original_row],
            values$data$Cq[original_row],
            sep = "__|__"
          )
          
          # Update excluded points if necessary
          if(old_row_id %in% values$excluded_points) {
            values$excluded_points <- setdiff(values$excluded_points, old_row_id)
            values$excluded_points <- c(values$excluded_points, new_row_id)
          }
          
          # Update row_id
          values$data$row_id[original_row] <- new_row_id
        }
        
        # Log the edit
        new_edit <- list(
          timestamp = Sys.time(),
          type = "edit",
          well = values$data[original_row, "Well"],
          target = values$data[original_row, "Target"],
          sample = values$data[original_row, "Sample"],
          column = col_name,
          old_value = old_value,
          new_value = v,
          row_index = original_row
        )
        
        # Add to edit history
        values$edit_history <- c(values$edit_history, list(new_edit))
        
        # Show success notification
        showNotification(sprintf("Updated %s from %s to %s in row %s", 
                                 col_name, 
                                 old_value, 
                                 v,
                                 values$data[original_row, "Well"]), 
                         type = "message")
        
        # Force refresh of the data
        values$data <- values$data  # This triggers reactivity
        
        # Re-run analysis if needed
        if(!is.null(values$results)) {
          # Re-run analysis with current settings
          req(input$controlSample, values$housekeeping_genes)
          
          withProgress(message = 'Updating analysis...', {
            analysis_data <- values$data %>% 
              filter(!row_id %in% values$excluded_points)
            
            if(nrow(analysis_data) > 0) {
              tryCatch({
                # Calculate results
                results <- calculateFoldChanges(
                  data = analysis_data,
                  controlSample = input$controlSample,
                  housekeeping_genes = values$housekeeping_genes
                )
                
                # Store results
                values$results <- results
                
                # Run statistical analysis
                if(!is.null(results) && nrow(results) > 0) {
                  values$stats_results <- performStatisticalAnalysis(results)
                  showNotification("Analysis updated!", type = "message")
                }
                
              }, error = function(e) {
                showNotification(paste("Analysis update error:", e$message), 
                                 type = "error")
              })
            }
          })
        }
      } else {
        showNotification("Error: Could not locate row to update", 
                         type = "error")
      }
    })
  })
  
  # Add reactive trigger for analysis
  observeEvent(values$data, {
    # If there's a previous analysis, re-run it with updated data
    if(!is.null(values$results)) {
      # Re-run analysis with current settings
      req(input$controlSample, values$housekeeping_genes)
      
      withProgress(message = 'Updating analysis...', {
        analysis_data <- values$data %>% 
          filter(!row_id %in% values$excluded_points)
        
        if(nrow(analysis_data) > 0) {
          tryCatch({
            # Calculate results
            results <- calculateFoldChanges(
              data = analysis_data,
              controlSample = input$controlSample,
              housekeeping_genes = values$housekeeping_genes
            )
            
            # Store results
            values$results <- results
            
            # Run statistical analysis
            if(!is.null(results) && nrow(results) > 0) {
              values$stats_results <- performStatisticalAnalysis(results)
              showNotification("Analysis updated!", type = "message")
            }
            
          }, error = function(e) {
            showNotification(paste("Analysis update error:", e$message), 
                             type = "error")
          })
        }
      })
    }
  })
 
  
  createQCPlot <- function(data, excluded_points) {
    # Create distribution plot
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
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Quality Control: Cq Values Distribution",
           y = "Cq Value") +
      scale_fill_viridis_d()
    
    # Create CV plot
    cv_data <- data %>%
      filter(!row_id %in% excluded_points) %>%  # Exclude manually excluded points
      group_by(Target, Sample) %>%
      summarise(
        CV = (sd(Cq, na.rm = TRUE) / mean(Cq, na.rm = TRUE)) * 100,
        .groups = 'drop'
      )
    
    p2 <- ggplot(cv_data, aes(x = Target, y = CV, fill = Sample)) +
      geom_bar(stat = "identity", position = position_dodge(0.9)) +
      geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
      theme_prism() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Coefficient of Variation (CV)",
           y = "CV (%)") +
      scale_fill_viridis_d() +
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
    
    return(list(p1 = p1, p2 = p2))
  }
  
  # fold change plot
  createFoldChangePlot <- function(plot_data, values, input) {
    if(nrow(plot_data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "No valid data to plot") +
               theme_void())
    }
    
    # Use custom sample order if available
    if(!is.null(values$sample_order)) {
      plot_data$Sample <- factor(plot_data$Sample, levels = values$sample_order)
    }
    
    # Create base plot
    p <- ggplot(plot_data, aes(x = Sample, y = individual_fold_change)) +
      facet_wrap(~Target, scales = "free_y")
    
    # Add main geometry based on plot type
    if(input$plotType == "bar") {
      p <- p + stat_summary(aes(fill = Sample),
                            fun = mean,
                            geom = "bar",
                            position = position_dodge(0.9),
                            alpha = 0.7,
                            na.rm = TRUE)
    } else if(input$plotType == "box") {
      p <- p + geom_boxplot(aes(fill = Sample),
                            alpha = 0.7,
                            na.rm = TRUE)
    } else if(input$plotType == "violin") {
      p <- p + geom_violin(aes(fill = Sample),
                           alpha = 0.7,
                           na.rm = TRUE)
    }
    
    # Add individual points if requested
    if(input$showIndividualPoints) {
      p <- p + geom_jitter(width = 0.2,
                           alpha = 0.6,
                           size = 2,
                           color = "black",
                           na.rm = TRUE)
    }
    
    # Add error bars
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
    
    # Add significance if requested
    if(input$showSignificance) {
      p <- addSignificanceMarkers(p, plot_data, values, input)
    }
    
    # Apply theme and formatting
    p <- p +
      theme_prism(base_size = input$fontSize) +
      {if(input$rotateLabels) 
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))} +
      labs(title = input$plotTitle,  # Use custom title
           y = "Fold Change (2^-ddCt)",
           x = "Sample") +
      theme(
        legend.position = "right",
        strip.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.1))
      )
    
    # Apply color palette
    p <- addColorPalette(p, input$colorPalette)
    
    return(p)
  }
  
  output$foldChangePlot <- renderPlot({
    req(values$results)
    
    plot_data <- values$results %>%
      filter(!is.na(individual_fold_change), 
             is.finite(individual_fold_change))
    
    if(nrow(plot_data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "No valid data to plot") +
               theme_void())
    }
    
    createFoldChangePlot(plot_data, values, input)
  }, height = 600)
  
  # output for qcplot
  output$qcPlot <- renderPlot({
    req(values$data)
    plots <- createQCPlot(values$data, values$excluded_points)
    gridExtra::grid.arrange(plots$p1, plots$p2, ncol = 1, heights = c(2, 2))
  }, height = 800)
  
  # results table
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
      mutate(across(where(is.numeric), round, 3))
    
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
  
  # Statistical analysis output
  output$statsOutput <- renderText({
    req(values$stats_results)
    
    output_text <- character()
    
    for(target in names(values$stats_results)) {
      result <- values$stats_results[[target]]
      
      output_text <- c(output_text,
                       paste("\n=== Statistical Analysis for", target, "===\n"),
                       paste("Test type:", result$test))
      
      if(result$test == "ANOVA") {
        output_text <- c(output_text,
                         "\nANOVA Summary:",
                         capture.output(print(result$summary)),
                         "\nPost-hoc Analysis (vs. Control):",
                         capture.output(print(result$control_comparisons)))
      } else if(result$test == "Kruskal-Wallis") {
        output_text <- c(output_text,
                         "\nKruskal-Wallis Test:",
                         paste("chi-squared =", round(result$summary$statistic, 2)),
                         paste("p-value =", format.pval(result$summary$p.value)),
                         "\nPost-hoc Analysis (vs. Control):",
                         capture.output(print(result$control_comparisons)))
      }
    }
    
    paste(output_text, collapse = "\n")
  })
  
  # Server Part 4: Download Handlers and Utility Functions
  output$downloadAllResults <- downloadHandler(
    filename = function() {
      req(values$analysis_run)
      paste0("qPCR_complete_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(values$analysis_run, values$data, values$results, values$stats_results)
      
      wb <- createWorkbook()
      
      # 1. Raw Data Sheet
      addWorksheet(wb, "1_Raw_Data")
      writeData(wb, "1_Raw_Data", values$data %>% 
                  select(-row_id) %>%
                  arrange(Sample, Target))
      
      # 2. Housekeeping Gene Data
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
      
      # 3. Analysis Results
      addWorksheet(wb, "3_Analysis_Results")
      writeData(wb, "3_Analysis_Results", values$results %>%
                  select(-row_id) %>%
                  arrange(Target, Sample))
      
      # 4. Summary Results
      addWorksheet(wb, "4_Summary_Results")
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
      writeData(wb, "4_Summary_Results", summary_results)
      
      # 5. Statistical Results
      addWorksheet(wb, "5_Statistical_Results")
      stat_results <- capture.output(print(values$stats_results))
      writeData(wb, "5_Statistical_Results", data.frame(Results = stat_results))
      
      # 6. Modification History
      addWorksheet(wb, "6_Modification_History")
      if(length(values$edit_history) > 0) {
        mod_history <- do.call(rbind, lapply(values$edit_history, function(x) {
          data.frame(
            Timestamp = format(x$timestamp, "%Y-%m-%d %H:%M:%S"),
            Type = x$type,
            Details = if(x$type == "edit") {
              sprintf("Edited %s in Well %s (%s, %s) from %s to %s",
                      x$column, x$well, x$target, x$sample, x$old_value, x$new_value)
            } else if(x$type == "exclusion") {
              sprintf("Excluded point - Well %s (%s, %s, Cq=%.2f)",
                      x$well, x$target, x$sample, x$cq)
            } else {
              sprintf("Cleared %d exclusions", x$count)
            }
          )
        }))
        writeData(wb, "6_Modification_History", mod_history)
      } else {
        writeData(wb, "6_Modification_History", "No modifications recorded")
      }
      
      # 7. Analysis Parameters
      addWorksheet(wb, "7_Analysis_Parameters")
      analysis_params <- data.frame(
        Parameter = c(
          "Analysis Date",
          "Control Sample",
          "Housekeeping Genes",
          "Geometric Mean for HK",
          "Excluded Points",
          "Technical Replicate Handling",
          "Statistical Test",
          "Significance Threshold"
        ),
        Value = c(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          input$controlSample,
          paste(values$housekeeping_genes, collapse = ", "),
          ifelse(input$useGeometricMean, "Yes", "No"),
          length(values$excluded_points),
          input$replicateHandling,
          input$statsTest,
          input$pThreshold
        )
      )
      writeData(wb, "7_Analysis_Parameters", analysis_params)
      
      # Apply styling to all sheets
      for(sheet in names(wb)) {
        addStyle(wb, sheet, createStyle(textDecoration = "bold"), rows = 1, cols = 1:50)
        setColWidths(wb, sheet, cols = 1:50, widths = "auto")
      }
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      req(values$analysis_run)
      paste0("qPCR_analysis_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$plotFormat)
    },
    content = function(file) {
      req(values$analysis_run, values$results)
      
      if(input$plotFormat == "pdf") {
        pdf(file, width = input$plotWidth, height = input$plotHeight)
      } else {
        png(file, width = input$plotWidth, height = input$plotHeight, units = "in", res = 300)
      }
      
      plot_data <- values$results %>%
        filter(!is.na(individual_fold_change), 
               is.finite(individual_fold_change))
      
      print(createFoldChangePlot(plot_data, values, input))
      
      dev.off()
    }
  )
  
  # Utility function for error handling
  handleError <- function(expr, error_message) {
    tryCatch(
      expr,
      error = function(e) {
        showNotification(
          paste(error_message, e$message),
          type = "error"
        )
        return(NULL)
      },
      warning = function(w) {
        showNotification(
          paste("Warning:", w$message),
          type = "warning"
        )
      }
    )
  }
  
  # Enhanced data validation utility
  validateDataCompleteness <- function(data) {
    validation_messages <- character()
    
    # Check for missing values
    missing_values <- colSums(is.na(data))
    if(any(missing_values > 0)) {
      validation_messages <- c(
        validation_messages,
        paste("Missing values found in columns:",
              paste(names(missing_values[missing_values > 0]), collapse = ", "))
      )
    }
    
    # Check replicate numbers
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
    
    # Check Cq range
    if(any(data$Cq < 0 | data$Cq > 40)) {
      validation_messages <- c(
        validation_messages,
        "Warning: Cq values outside expected range (0-40) detected"
      )
    }
    
    # Check for duplicate wells
    duplicate_wells <- data %>%
      group_by(Well) %>%
      filter(n() > 1) %>%
      pull(Well) %>%
      unique()
    
    if(length(duplicate_wells) > 0) {
      validation_messages <- c(
        validation_messages,
        paste("Duplicate well assignments found for wells:",
              paste(duplicate_wells, collapse = ", "))
      )
    }
    
    return(validation_messages)
  }
  
  addSignificanceMarkers <- function(p, plot_data, values, input) {
    for(target in names(values$stats_results)) {
      target_data <- plot_data %>% filter(Target == target)
      
      if(nrow(target_data) > 0) {
        result <- values$stats_results[[target]]
        max_y <- max(target_data$individual_fold_change, na.rm = TRUE)
        y_position <- max_y * 1.1
        
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
            y_positions <- seq(y_position, 
                               y_position + (nrow(control_comparisons) - 1) * max_y * 0.1, 
                               by = max_y * 0.1)
            
            for(i in 1:nrow(control_comparisons)) {
              if(control_comparisons$significant[i] || input$showNonSignificant) {
                p <- p + geom_signif(
                  data = target_data,
                  comparisons = list(c(control_comparisons$group1[i], 
                                       control_comparisons$group2[i])),
                  annotations = control_comparisons$annotation[i],
                  y_position = y_positions[i],
                  tip_length = 0.01,
                  vjust = 0.5
                )
              }
            }
          }
        }
      }
    }
    return(p)
  }
  
  # Utility function for formatting p-values
  formatPValue <- function(p_value) {
    case_when(
      p_value < 0.001 ~ "< 0.001",
      p_value < 0.01 ~ sprintf("%.3f", p_value),
      p_value < 0.05 ~ sprintf("%.3f", p_value),
      TRUE ~ sprintf("%.3f", p_value)
    )
  }
  
  # Add color palette utility
  addColorPalette <- function(p, palette) {
    if(palette == "viridis") {
      p + scale_fill_viridis_d()
    } else if(palette == "npg") {
      p + scale_fill_manual(values = ggsci::pal_npg()(10))
    } else if(palette == "lancet") {
      p + scale_fill_manual(values = ggsci::pal_lancet()(10))
    } else {
      p + scale_fill_brewer(palette = "Set3")
    }
  }
  
  # Session cleanup
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