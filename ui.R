# UI definition
library(shiny)
library(shinythemes)
library(shinyjs)
library(DT)
library(sortable)

shinyUI(fluidPage(
  useShinyjs(),
  navbarPage(
    title = textOutput("app_title", inline = TRUE),
    theme = shinytheme("yeti"),
    
    # Language selector in navbar
    windowTitle = "qPCR Analysis",
  header = div(
    style = "position: absolute; top: 10px; right: 20px; z-index: 1000;",
    selectInput("language", 
                label = NULL,
                choices = list("English" = "en", "中文" = "zh"),
                selected = "en",
                width = "120px")
  ),
  
  # Analysis Tab
  tabPanel(textOutput("analysis_tab", inline = TRUE),
           tags$head(
             tags$style(HTML("
      .well { padding: 10px; margin-bottom: 15px; }
      .control-label { font-weight: bold; }
      .plot-container { border: 1px solid #ddd; padding: 10px; border-radius: 5px; }
      .error-text { color: red; font-weight: bold; }
      .info-text { color: blue; font-style: italic; }
      .analysis-section { margin-top: 20px; }
      .section-divider { 
        border-top: 2px solid #e0e0e0;
        margin: 15px 0;
        padding-top: 15px;
      }
      .housekeeping-section { 
        background-color: #f8f9fa;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 10px;
      }
      .outlier-row { background-color: #ffebee !important; }
      .selected-outlier { background-color: #fff3e0 !important; }
      .qc-plot { height: 800px !important; }
      .disabled {
      cursor: not-allowed !important;
    }
    [title] {
      position: relative;
      cursor: help;
    }
    "))
           ),
           
           sidebarLayout(
             sidebarPanel(
               width = 3,
               
               # Step 1: File upload section
               wellPanel(
                 h4(textOutput("step1_title", inline = TRUE), style = "color: #2c3e50; margin-top: 0;"),
                 fileInput("file", textOutput("upload_file", inline = TRUE),
                           accept = c("text/csv", ".csv", ".xlsx", ".xls"),
                           multiple = FALSE),
                 
                 tags$small(class = "info-text",
                            textOutput("upload_help", inline = TRUE)),
                 
                 conditionalPanel(
                   condition = "typeof input.file !== 'undefined'",
                   tags$div(
                     uiOutput("sheetSelection"),
                     uiOutput("validationMessages")
                   )
                 )
               ),
               
               # Step 2: Essential Settings Panel
               conditionalPanel(
                 condition = "typeof input.file !== 'undefined'",
                 wellPanel(
                   h4(textOutput("step2_title", inline = TRUE), style = "color: #2c3e50;"),
                   
                   selectInput("controlSample", textOutput("control_sample", inline = TRUE),
                               choices = NULL),
                   
                   # Simplified housekeeping gene section
                   checkboxInput("autoDetectHK", 
                                 textOutput("auto_detect_hk", inline = TRUE), 
                                 value = TRUE),
                   conditionalPanel(
                     condition = "input.autoDetectHK == false",
                     selectInput("manualHKGenes",
                                 textOutput("manual_hk_genes", inline = TRUE),
                                 choices = NULL,
                                 multiple = TRUE)
                   ),
                   
                   # Advanced housekeeping settings in collapsible
                   tags$details(
                     tags$summary(textOutput("advanced_hk_settings", inline = TRUE), 
                                  style = "cursor: pointer; color: #3498db;"),
                     div(style = "margin-top: 10px;",
                         conditionalPanel(
                           condition = "input.autoDetectHK == true",
                           textInput("hkPattern",
                                     textOutput("detection_pattern", inline = TRUE),
                                     value = "RNA|GAPDH|ACTB|18S")
                         ),
                         checkboxInput("useGeometricMean",
                                       textOutput("geometric_mean", inline = TRUE),
                                       value = TRUE)
                     )
                   ),

                   # PCR Efficiency Settings (MIQE 2.0 compliance)
                   tags$details(
                     tags$summary(textOutput("efficiency_settings", inline = TRUE),
                                  style = "cursor: pointer; color: #3498db;"),
                     div(style = "margin-top: 10px;",
                         checkboxInput("useEfficiencyCorrection",
                                       textOutput("use_efficiency_correction", inline = TRUE),
                                       value = FALSE),
                         conditionalPanel(
                           condition = "input.useEfficiencyCorrection == true",
                          uiOutput("efficiencyInputMethodUI"),
                           conditionalPanel(
                             condition = "input.efficiencyInputMethod == 'manual'",
                             uiOutput("efficiencyInputsUI"),
                             tags$small(class = "info-text",
                                        textOutput("efficiency_help", inline = TRUE))
                           ),
                           conditionalPanel(
                             condition = "input.efficiencyInputMethod == 'standard_curve'",
                             fileInput("stdCurveFile",
                                       textOutput("upload_std_curve", inline = TRUE),
                                       accept = c("text/csv", ".csv", ".xlsx", ".xls")),
                            tags$small(class = "info-text",
                                       textOutput("std_curve_help", inline = TRUE)),
                             uiOutput("stdCurveSheetSelection"),
                             actionButton("calculateEfficiency",
                                          textOutput("calculate_efficiency", inline = TRUE),
                                          class = "btn-info btn-sm",
                                          style = "margin-bottom: 10px;"),
                             uiOutput("calculatedEfficiencyUI")
                           )
                         ),
                         uiOutput("efficiencyWarningsUI")
                     )
                   ),

                   # Run Analysis and Download All Results
                   hr(style = "margin: 20px 0 15px 0;"),
                   actionButton("runAnalysis", textOutput("run_analysis", inline = TRUE), 
                                class = "btn-primary",
                                style = "width: 100%; margin-bottom: 10px;"),
                   uiOutput("downloadAllResultsButton")
                 )
               ),
               
               # Statistical Settings Panel
               conditionalPanel(
                 condition = "typeof input.file !== 'undefined'",
                 wellPanel(
                   h4(textOutput("statistical_settings", inline = TRUE), style = "color: #2c3e50;"),
                   
                   selectInput("statsTest", textOutput("statistical_test", inline = TRUE),
                               choices = list(
                                 "one_way_anova" = "anova",
                                 "kruskal_wallis" = "kruskal"
                               )),
                   
                   # Dynamic choices based on efficiency correction
                   uiOutput("statsDataTypeUI"),
                   
                   # Advanced statistics in collapsible
                   tags$details(
                     tags$summary(textOutput("advanced_statistics", inline = TRUE), 
                                  style = "cursor: pointer; color: #3498db;"),
                     div(style = "margin-top: 10px;",
                         selectInput("pAdjustMethod", 
                                     textOutput("p_adjust_method", inline = TRUE),
                                     choices = list(
                                       "none" = "none",
                                       "bonferroni" = "bonferroni",
                                       "holm" = "holm",
                                       "hochberg" = "hochberg",
                                       "hommel" = "hommel",
                                       "fdr_bh" = "BH",
                                       "benjamini_yekutieli" = "BY"
                                     ),
                                     selected = "BH"),
                         
                         numericInput("pThreshold", 
                                      textOutput("significance_threshold", inline = TRUE), 
                                      value = 0.05, 
                                      min = 0.001, 
                                      max = 0.1, 
                                      step = 0.001)
                     )
                   )
                 )
               ),
               
               # Plot Customization Panel
               conditionalPanel(
                 condition = "typeof input.file !== 'undefined'",
                 wellPanel(
                   h4(textOutput("plot_customization", inline = TRUE), style = "color: #2c3e50;"),
                   
                   # Essential plot settings
                   textInput("plotTitle", 
                             textOutput("plot_title", inline = TRUE), 
                             value = ""),
                   
                   uiOutput("dataDisplayTypeUI"),
                   
                   selectInput("plotType", textOutput("plot_type", inline = TRUE),
                               choices = list(
                                 "bar_plot" = "bar",
                                 "box_plot" = "box",
                                 "violin_plot" = "violin",
                                 "beeswarm_plot" = "beeswarm"
                               )),
                   
                   conditionalPanel(
                     condition = "input.plotType == 'bar'",
                     selectInput("errorBar", textOutput("error_bar_type", inline = TRUE),
                                 choices = list(
                                   "standard_error" = "se",
                                   "standard_deviation" = "sd",
                                   "confidence_interval" = "ci"
                                 ))
                   ),
                   
                   # Update plot button
                   actionButton("updatePlot", textOutput("update_plot", inline = TRUE), 
                                class = "btn-success",
                                style = "width: 100%; margin-top: 15px; margin-bottom: 15px;"),
                   
                   # Advanced plot settings in collapsible
                   tags$details(
                     tags$summary(textOutput("advanced_plot_settings", inline = TRUE), 
                                  style = "cursor: pointer; color: #3498db;"),
                     div(style = "margin-top: 10px;",
                         
                         # Sample Order
                         h5(textOutput("sample_order", inline = TRUE), style = "margin-top: 10px;"),
                         uiOutput("sampleOrderUI"),
                         
                         # Visual Options
                         checkboxInput("showIndividualPoints", 
                                       textOutput("show_individual_points", inline = TRUE), 
                                       value = TRUE),

                        checkboxInput("showBorderLines",
                                      textOutput("show_border_lines", inline = TRUE),
                                      value = TRUE),
                         
                         checkboxInput("showSignificance", 
                                       textOutput("show_significance", inline = TRUE), 
                                       value = TRUE),
                         
                         conditionalPanel(
                           condition = "input.showSignificance == true",
                           checkboxInput("showNonSignificant",
                                         textOutput("show_non_significant", inline = TRUE),
                                         value = FALSE),
                           selectInput("significanceType", 
                                       textOutput("significance_display", inline = TRUE),
                                       choices = list(
                                         "stars" = "stars",
                                         "p_values" = "p.value",
                                         "both" = "both"
                                       ))
                         ),
                         
                         # Color Palette
                         selectInput("colorPalette", textOutput("color_palette", inline = TRUE),
                                     choices = list(
                                       "classic_default" = "classic",
                                       "colorblind_friendly" = "viridis",
                                       "black_white" = "bw",
                                       "grayscale" = "grey",
                                                 "Nature (NPG)" = "npg",
                                                 "Science (AAAS)" = "aaas",
                                                 "NEJM" = "nejm",
                                                 "Lancet" = "lancet",
                                                 "JAMA" = "jama",
                                                 "Set1 (Bright)" = "set1",
                                                 "Set2 (Pastel)" = "set2",
                                                 "Dark2" = "dark2",
                                                 "Paired" = "paired"),
                                     selected = "classic"),
                         
                         # Size adjustments
                         h5("Size Adjustments", style = "margin-top: 15px;"),
                         numericInput("plotHeight", "Plot Height (inches)", 
                                      value = 8, min = 2, max = 20),
                         numericInput("plotWidth", "Plot Width (inches)", 
                                      value = 10, min = 2, max = 20),
                         sliderInput("fontSize", textOutput("font_size", inline = TRUE), 
                                     min = 8, max = 20, value = 12),
                         sliderInput("dotSize", textOutput("point_size", inline = TRUE), 
                                     min = 0.5, max = 5, value = 2, step = 0.5),
                         numericInput("facetCols", textOutput("facet_cols", inline = TRUE), 
                                      min = 1, max = 6, value = 3, step = 1)
                     )
                   )
                 )
               )
             ),
             
             # Main Panel
             mainPanel(
               width = 9,
               tabsetPanel(
                 # Data Preview Tab
                 tabPanel(textOutput("data_preview", inline = TRUE),
                          fluidRow(
                            column(12,
                                   tabsetPanel(
                                     # Raw Data tab
                                     tabPanel(textOutput("raw_data", inline = TRUE),
                                              fluidRow(
                                                column(12,
                                                       div(style = "margin: 10px 0;",
                                                           actionButton("excludeSelected", 
                                                                        textOutput("exclude_selected", inline = TRUE),
                                                                        class = "btn-info"),
                                                           actionButton("clearExclusions", 
                                                                        textOutput("clear_exclusions", inline = TRUE),
                                                                        class = "btn-warning")
                                                       )
                                                )
                                              ),
                                              div(style = "margin-top: 10px;",
                                                  tags$p(class = "info-text", 
                                                         textOutput("data_preview_help", inline = TRUE)),
                                                  DT::dataTableOutput("rawDataPreview"),
                                                  verbatimTextOutput("excludedPointsSummary"))
                                     ),
                                     
                                     # QC Plot tab
                                     tabPanel(textOutput("quality_control", inline = TRUE),
                                              h4(textOutput("qc_plots", inline = TRUE)),
                                              plotOutput("qcPlot", height = "auto"))
                                   ))
                          )),
                 
                 # Analysis Results Tab
                 tabPanel(textOutput("analysis_results", inline = TRUE),
                          uiOutput("analysisMessage"),  # Add this line
                          fluidRow(
                            column(12,
                                   div(class = "plot-container",
                                       plotOutput("foldChangePlot", height = "auto"),
                                       div(style = "margin-top: 10px; text-align: left;",
                                           selectInput("plotFormat", textOutput("plot_format", inline = TRUE),
                                                       choices = list(
                                                         "pdf_format" = "pdf",
                                                         "png_format" = "png"
                                                       ),
                                                       selected = "pdf",
                                                       width = "100px"
                                           ),
                                           downloadButton("downloadPlot", textOutput("download_plot", inline = TRUE),
                                                          class = "btn-info")
                                       )
                                   )
                            )
                          ),
                          br(),
                          fluidRow(
                            column(12,
                                   DT::dataTableOutput("resultsTable")
                            )
                          )
                 ),
                 
                 # Statistical Analysis Tab
                 tabPanel(textOutput("statistical_analysis", inline = TRUE),
                          fluidRow(
                            column(12,
                                   verbatimTextOutput("statsOutput"))
                          ))
               ))
  )),
  
  # Help Tab
  tabPanel(textOutput("help_tab", inline = TRUE),
           fluidRow(
             column(12,
                    # Add CSS for custom styling
                    tags$head(
                      tags$style(HTML("
          .help-section {
            margin-bottom: 30px;
            padding: 20px;
            border-radius: 5px;
            background-color: #fff;
            box-shadow: 0 1px 3px rgba(0,0,0,0.12);
          }
          .help-title {
            color: #2c3e50;
            border-bottom: 2px solid #3498db;
            padding-bottom: 10px;
            margin-bottom: 20px;
          }
          .help-subtitle {
            color: #34495e;
            margin: 15px 0;
          }
          .step-number {
            display: inline-block;
            width: 24px;
            height: 24px;
            background-color: #3498db;
            color: white;
            border-radius: 50%;
            text-align: center;
            margin-right: 10px;
          }
          .note-box {
            background-color: #f8f9fa;
            border-left: 4px solid #17a2b8;
            padding: 15px;
            margin: 10px 0;
          }
          .warning-box {
            background-color: #fff3cd;
            border-left: 4px solid #ffc107;
            padding: 15px;
            margin: 10px 0;
          }
          .help-image {
            max-width: 100%;
            height: auto;
            border: 1px solid #ddd;
            border-radius: 4px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          }
        "))
                    ),
                    
                    # Main help content
                    div(class = "help-section",
                        h2(class = "help-title", textOutput("help_quick_start_guide", inline = TRUE)),
                        p(textOutput("help_intro_text", inline = TRUE)),
                        
                        tags$ol(
                          tags$li(strong(textOutput("help_step1_data_upload", inline = TRUE)), 
                                  p(textOutput("help_step1_description", inline = TRUE)),
                                  # File upload interface screenshot
                                  div(style = "text-align: center; margin: 15px 0;",
                                      tags$img(src = "screenshots/file_upload.png", 
                                              alt = textOutput("help_file_upload_alt", inline = TRUE),
                                              class = "help-image")
                                  ),
                                  div(class = "note-box",
                                      textOutput("help_required_columns", inline = TRUE),
                                      tags$ul(
                                        tags$li(textOutput("help_column_sample", inline = TRUE)),
                                        tags$li(textOutput("help_column_target", inline = TRUE)),
                                        tags$li(textOutput("help_column_cq", inline = TRUE)),
                                        tags$li(textOutput("help_column_well", inline = TRUE))
                                      )
                                  ),
                                  # Example of properly formatted data screenshot
                                  div(style = "text-align: center; margin: 15px 0;",
                                      tags$img(src = "screenshots/sample_data_format.png", 
                                              alt = textOutput("help_sample_data_alt", inline = TRUE),
                                              class = "help-image")
                                  )
                          ),
                          
                          tags$li(strong(textOutput("help_step2_settings", inline = TRUE)), 
                                  tags$ul(
                                    tags$li(textOutput("help_step2_select_control", inline = TRUE)),
                                    tags$li(textOutput("help_step2_configure_hk", inline = TRUE),
                                            tags$ul(
                                              tags$li(textOutput("help_step2_auto_detect", inline = TRUE)),
                                              tags$li(textOutput("help_step2_manual_select", inline = TRUE))
                                            )
                                    ),
                                    tags$li(textOutput("help_step2_advanced", inline = TRUE))
                                  ),
                                  # Essential settings panel screenshot
                                  div(style = "text-align: center; margin: 15px 0;",
                                      tags$img(src = "screenshots/essential_settings.png", 
                                              alt = textOutput("help_essential_settings_alt", inline = TRUE),
                                              class = "help-image")
                                  )
                          ),
                          
                          tags$li(strong(textOutput("help_step3_run_analysis", inline = TRUE)), 
                                  p(textOutput("help_step3_click_run", inline = TRUE)),
                                  p(textOutput("help_step3_results", inline = TRUE)),
                                  # Analysis results screenshot
                                  div(style = "text-align: center; margin: 15px 0;",
                                      tags$img(src = "screenshots/plot_results.png", 
                                              alt = textOutput("help_analysis_results_alt", inline = TRUE),
                                              class = "help-image")
                                  )
                          )
                        )
                    ),
                    
                    div(class = "help-section",
                        h2(class = "help-title", textOutput("help_data_analysis_process", inline = TRUE)),
                        
                        h3(class = "help-subtitle", textOutput("help_qc_title", inline = TRUE)),
                        tags$ul(
                          tags$li(textOutput("help_qc_review_plots", inline = TRUE)),
                          tags$li(textOutput("help_qc_check_distribution", inline = TRUE)),
                          tags$li(textOutput("help_qc_monitor_cv", inline = TRUE)),
                          tags$li(strong(textOutput("help_qc_handling_outliers", inline = TRUE)),
                                  tags$ul(
                                    tags$li(textOutput("help_qc_select_problematic", inline = TRUE)),
                                    tags$li(textOutput("help_qc_exclude_rows", inline = TRUE)),
                                    tags$li(textOutput("help_qc_track_modifications", inline = TRUE))
                                  )
                          )
                        ),
                        # Quality control plots screenshot
                        div(style = "text-align: center; margin: 15px 0;",
                            tags$img(src = "screenshots/stats_results.png", 
                                    alt = textOutput("help_qc_plots_alt", inline = TRUE),
                                    class = "help-image")
                        ),
                        
                        h3(class = "help-subtitle", textOutput("help_analysis_settings", inline = TRUE)),
                        tags$ul(
                          tags$li(strong(textOutput("help_hk_genes_title", inline = TRUE)),
                                  tags$ul(
                                    tags$li(textOutput("help_hk_auto_detect_desc", inline = TRUE)),
                                    tags$li(textOutput("help_hk_manual_selection", inline = TRUE)),
                                    tags$li(textOutput("help_hk_geometric_mean", inline = TRUE)),
                                    tags$li(textOutput("help_hk_validation", inline = TRUE))
                                  )
                          ),
                          tags$li(strong(textOutput("help_statistical_analysis_title", inline = TRUE)),
                                  tags$ul(
                                    tags$li(textOutput("help_stats_test_on", inline = TRUE)),
                                    tags$li(textOutput("help_stats_anova", inline = TRUE)),
                                    tags$li(textOutput("help_stats_kruskal", inline = TRUE)),
                                    tags$li(textOutput("help_stats_correction", inline = TRUE),
                                            tags$ul(
                                              tags$li(textOutput("help_stats_correction_methods", inline = TRUE)),
                                              tags$li(textOutput("help_stats_correction_methods2", inline = TRUE))
                                            )
                                    ),
                                    tags$li(textOutput("help_stats_threshold", inline = TRUE))
                                  )
                          )
                        ),
                        
                        h3(class = "help-subtitle", textOutput("help_visualization_options", inline = TRUE)),
                        tags$ul(
                          tags$li(strong(textOutput("help_data_display_types", inline = TRUE)),
                                  tags$ul(
                                    tags$li(textOutput("help_display_fold_change", inline = TRUE)),
                                    tags$li(textOutput("help_display_ddct", inline = TRUE)),
                                    tags$li(textOutput("help_display_neg_ddct", inline = TRUE)),
                                    tags$li(textOutput("help_display_dct", inline = TRUE)),
                                    tags$li(textOutput("help_display_neg_dct", inline = TRUE))
                                  )
                          ),
                          tags$li(strong(textOutput("help_plot_types", inline = TRUE)),
                                  tags$ul(
                                    tags$li(textOutput("help_plot_bar", inline = TRUE)),
                                    tags$li(textOutput("help_plot_box", inline = TRUE)),
                                    tags$li(textOutput("help_plot_violin", inline = TRUE)),
                                    tags$li(textOutput("help_plot_beeswarm", inline = TRUE))
                                  )
                          ),
                          # Different plot types screenshot
                          div(style = "text-align: center; margin: 15px 0;",
                              tags$img(src = "screenshots/plot_settings.png", 
                                      alt = textOutput("help_plot_customization_alt", inline = TRUE),
                                      class = "help-image")
                          ),
                          tags$li(strong(textOutput("help_customization", inline = TRUE)),
                                  tags$ul(
                                    tags$li(textOutput("help_custom_error_bars", inline = TRUE)),
                                    tags$li(textOutput("help_custom_colors", inline = TRUE)),
                                    tags$li(textOutput("help_custom_sizes", inline = TRUE)),
                                    tags$li(textOutput("help_custom_sample_order", inline = TRUE)),
                                    tags$li(textOutput("help_custom_significance", inline = TRUE)),
                                    tags$li(textOutput("help_custom_export", inline = TRUE))
                                  )
                          )
                        ),
                        # Plot customization panel screenshot
                        div(style = "text-align: center; margin: 15px 0;",
                            tags$img(src = "screenshots/stats_settings.png", 
                                    alt = textOutput("help_stats_settings_alt", inline = TRUE),
                                    class = "help-image")
                        )
                    ),
                    
                    div(class = "help-section",
                        h2(class = "help-title", textOutput("help_results_downloads", inline = TRUE)),
                        
                        h3(class = "help-subtitle", textOutput("help_available_downloads", inline = TRUE)),
                        tags$ul(
                          tags$li(strong(textOutput("help_download_all_results", inline = TRUE)),
                                  tags$ul(
                                    tags$li(textOutput("help_download_raw_data", inline = TRUE)),
                                    tags$li(textOutput("help_download_hk_analysis", inline = TRUE)),
                                    tags$li(textOutput("help_download_complete_analysis", inline = TRUE)),
                                    tags$li(textOutput("help_download_statistical_analysis", inline = TRUE)),
                                    tags$li(textOutput("help_download_modification_history", inline = TRUE)),
                                    tags$li(textOutput("help_download_analysis_parameters", inline = TRUE))
                                  ),
                                  div(class = "note-box",
                                      textOutput("help_download_note", inline = TRUE)
                                  )
                          ),
                          tags$li(strong(textOutput("help_individual_tables", inline = TRUE)),
                                  tags$ul(
                                    tags$li(textOutput("help_raw_data_export", inline = TRUE)),
                                    tags$li(textOutput("help_analysis_results_export", inline = TRUE))
                                  )
                          ),
                          tags$li(strong(textOutput("help_plots_export", inline = TRUE)),
                                  tags$ul(
                                    tags$li(textOutput("help_pdf_publication", inline = TRUE)),
                                    tags$li(textOutput("help_png_presentation", inline = TRUE))
                                  )
                          )
                        ),
                        # Data results table screenshot
                        div(style = "text-align: center; margin: 15px 0;",
                            tags$img(src = "screenshots/table_results.png", 
                                    alt = textOutput("help_table_results_alt", inline = TRUE),
                                    class = "help-image")
                        )
                    ),
                    
                    div(class = "help-section",
                        h2(class = "help-title", textOutput("data_privacy_title", inline = TRUE)),
                        
                        div(class = "note-box",
                            h4(textOutput("data_secure_title", inline = TRUE)),
                            tags$ul(
                              tags$li(textOutput("no_server_storage", inline = TRUE)),
                              tags$li(textOutput("session_only_processing", inline = TRUE)),
                              tags$li(textOutput("local_processing", inline = TRUE)),
                              tags$li(textOutput("no_data_transmission", inline = TRUE))
                            )
                        ),
                        
                        div(class = "warning-box",
                            h4(textOutput("session_management_title", inline = TRUE)),
                            tags$ul(
                              tags$li(textOutput("starting_fresh", inline = TRUE)),
                              tags$li(textOutput("save_your_work", inline = TRUE)),
                              tags$li(textOutput("session_duration", inline = TRUE)),
                              tags$li(textOutput("memory_cleanup", inline = TRUE))
                            )
                        )
                    ),
                    
                    div(class = "help-section",
                        h2(class = "help-title", textOutput("help_tips_troubleshooting", inline = TRUE)),
                        
                        div(class = "warning-box",
                            h4(textOutput("help_common_issues", inline = TRUE)),
                            tags$ul(
                              tags$li(textOutput("help_issue_missing_columns", inline = TRUE)),
                              tags$li(textOutput("help_issue_no_hk_genes", inline = TRUE)),
                              tags$li(textOutput("help_issue_invalid_cq", inline = TRUE)),
                              tags$li(textOutput("help_issue_insufficient_replicates", inline = TRUE)),
                              tags$li(textOutput("help_issue_high_cv", inline = TRUE)),
                              tags$li(textOutput("help_issue_excel_sheets", inline = TRUE))
                            )
                        ),
                        
                        h3(class = "help-subtitle", textOutput("help_best_practices", inline = TRUE)),
                        tags$ul(
                          tags$li(textOutput("help_practice_review_qc", inline = TRUE)),
                          tags$li(textOutput("help_practice_document_exclusions", inline = TRUE)),
                          tags$li(textOutput("help_practice_geometric_mean", inline = TRUE)),
                          tags$li(textOutput("help_practice_ddct_statistics", inline = TRUE)),
                          tags$li(textOutput("help_practice_multiple_testing", inline = TRUE)),
                          tags$li(textOutput("help_practice_test_assumptions", inline = TRUE)),
                          tags$li(textOutput("help_practice_export_complete", inline = TRUE))
                        )
                    )
             )
           )
  ),
  
  # Footer with attribution
  tags$footer(
    style = "text-align: center; padding: 20px; margin-top: 30px; border-top: 1px solid #e0e0e0; color: #666; font-size: 12px;",
    textOutput("footer_attribution", inline = TRUE),
    tags$a(
      href = "https://github.com/mianaz/qpcr_tool",
      target = "_blank",
      style = "color: #337ab7; text-decoration: none; margin: 0 2px;",
      textOutput("footer_github_text", inline = TRUE)
    ),
    textOutput("footer_license_support", inline = TRUE)
  )
  )
))
