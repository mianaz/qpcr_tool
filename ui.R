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
                   
                   # Update choices dynamically in server
                   
                   selectInput("statsDataType", textOutput("test_on", inline = TRUE),
                               choices = list(
                                 "ddct_values" = "ddct",
                                 "dct_values" = "dct"
                               ),
                               selected = "ddct"),
                   
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
                   
                   selectInput("dataDisplayType",
                               textOutput("data_display_type", inline = TRUE),
                               choices = list(
                                 "fold_change" = "fold_change",
                                 "ddct_values" = "ddct",
                                 "neg_ddct_values" = "neg_ddct",
                                 "dct_values" = "dct",
                                 "neg_dct_values" = "neg_dct"
                               ),
                               selected = "fold_change"),
                   
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
                        h2(class = "help-title", "Quick Start Guide"),
                        p("This application performs qPCR data analysis using the ΔΔCt method with MIQE-compliant statistical analysis."),
                        
                        tags$ol(
                          tags$li(strong("Step 1: Data Upload"), 
                                  p("Upload your qPCR data file (CSV or Excel format)."),
                                  # File upload interface screenshot
                                  div(style = "text-align: center; margin: 15px 0;",
                                      tags$img(src = "screenshots/file_upload.png", 
                                              alt = "File upload interface",
                                              class = "help-image")
                                  ),
                                  div(class = "note-box",
                                      "Required columns:",
                                      tags$ul(
                                        tags$li("Sample: Sample identifiers"),
                                        tags$li("Target: Gene names"),
                                        tags$li("Cq: Quantification cycle values (0-40)"),
                                        tags$li("Well: PCR well locations (optional but recommended)")
                                      )
                                  ),
                                  # Example of properly formatted data screenshot
                                  div(style = "text-align: center; margin: 15px 0;",
                                      tags$img(src = "screenshots/sample_data_format.png", 
                                              alt = "Example of properly formatted qPCR data",
                                              class = "help-image")
                                  )
                          ),
                          
                          tags$li(strong("Step 2: Configure Essential Settings"), 
                                  tags$ul(
                                    tags$li("Select your control/reference sample"),
                                    tags$li("Configure housekeeping genes:",
                                            tags$ul(
                                              tags$li("Auto-detect: Uses pattern matching (e.g., GAPDH, ACTB, 18S)"),
                                              tags$li("Manual: Select specific genes from dropdown")
                                            )
                                    ),
                                    tags$li("Advanced settings available in collapsible sections")
                                  ),
                                  # Essential settings panel screenshot
                                  div(style = "text-align: center; margin: 15px 0;",
                                      tags$img(src = "screenshots/essential_settings.png", 
                                              alt = "Essential settings panel",
                                              class = "help-image")
                                  )
                          ),
                          
                          tags$li(strong("Step 3: Run Analysis"), 
                                  p("Click 'Run Analysis' to process your data."),
                                  p("Results include ΔCt, ΔΔCt, fold changes, and statistical comparisons."),
                                  # Analysis results screenshot
                                  div(style = "text-align: center; margin: 15px 0;",
                                      tags$img(src = "screenshots/plot_results.png", 
                                              alt = "Analysis results with fold change plot",
                                              class = "help-image")
                                  )
                          )
                        )
                    ),
                    
                    div(class = "help-section",
                        h2(class = "help-title", "Data Analysis Process"),
                        
                        h3(class = "help-subtitle", "1. Data Quality Control"),
                        tags$ul(
                          tags$li("Review QC plots in 'Quality Control' tab"),
                          tags$li("Check Cq distribution across samples"),
                          tags$li("Monitor CV values (red line indicates 5% threshold)"),
                          tags$li(strong("Handling outliers:"),
                                  tags$ul(
                                    tags$li("Select problematic data points in 'Raw Data' tab"),
                                    tags$li("Use 'Exclude Selected Rows' to remove outliers"),
                                    tags$li("All modifications are tracked and included in final report")
                                  )
                          )
                        ),
                        # Quality control plots screenshot
                        div(style = "text-align: center; margin: 15px 0;",
                            tags$img(src = "screenshots/stats_results.png", 
                                    alt = "Quality control plots",
                                    class = "help-image")
                        ),
                        
                        h3(class = "help-subtitle", "2. Analysis Settings"),
                        tags$ul(
                          tags$li(strong("Housekeeping Genes:"),
                                  tags$ul(
                                    tags$li("Auto-detect: Uses pattern matching (customizable)"),
                                    tags$li("Manual selection: Choose specific genes from your data"),
                                    tags$li("Geometric mean: MIQE-recommended for multiple housekeeping genes"),
                                    tags$li("Automatically validates housekeeping gene stability")
                                  )
                          ),
                          tags$li(strong("Statistical Analysis:"),
                                  tags$ul(
                                    tags$li("Tests performed on ΔΔCt or ΔCt values (selectable)"),
                                    tags$li("One-way ANOVA: For normally distributed data"),
                                    tags$li("Kruskal-Wallis: Non-parametric alternative"),
                                    tags$li("Multiple testing correction methods available:",
                                            tags$ul(
                                              tags$li("Bonferroni, Holm, FDR (Benjamini-Hochberg)"),
                                              tags$li("Hochberg, Hommel, Benjamini-Yekutieli")
                                            )
                                    ),
                                    tags$li("Customizable significance threshold")
                                  )
                          )
                        ),
                        
                        h3(class = "help-subtitle", "3. Visualization Options"),
                        tags$ul(
                          tags$li(strong("Data Display Types:"),
                                  tags$ul(
                                    tags$li("Fold Change (2^-ΔΔCt): Traditional relative expression"),
                                    tags$li("ΔΔCt Values: Log-scale normalized expression differences"),
                                    tags$li("-ΔΔCt Values: Intuitive direction (higher = more expression)"),
                                    tags$li("ΔCt Values: Ct normalized to housekeeping genes"),
                                    tags$li("-ΔCt Values: Intuitive ΔCt (higher = more expression)")
                                  )
                          ),
                          tags$li(strong("Plot Types:"),
                                  tags$ul(
                                    tags$li("Bar Plot: Shows mean with optional error bars and points"),
                                    tags$li("Box Plot: Displays quartiles and outliers"),
                                    tags$li("Violin Plot: Shows data density distribution"),
                                    tags$li("Beeswarm Plot: Individual points with mean indicator")
                                  )
                          ),
                          # Different plot types screenshot
                          div(style = "text-align: center; margin: 15px 0;",
                              tags$img(src = "screenshots/plot_settings.png", 
                                      alt = "Plot customization options",
                                      class = "help-image")
                          ),
                          tags$li(strong("Customization:"),
                                  tags$ul(
                                    tags$li("Error bars: SE, SD, or 95% CI"),
                                    tags$li("Color palettes: 13+ options including colorblind-friendly"),
                                    tags$li("Adjustable font and point sizes"),
                                    tags$li("Sample order: Drag-and-drop reordering"),
                                    tags$li("Statistical significance display: Stars, p-values, or both"),
                                    tags$li("Export formats: PDF (publication) or PNG (300 dpi)")
                                  )
                          )
                        ),
                        # Plot customization panel screenshot
                        div(style = "text-align: center; margin: 15px 0;",
                            tags$img(src = "screenshots/stats_settings.png", 
                                    alt = "Statistical settings panel",
                                    class = "help-image")
                        )
                    ),
                    
                    div(class = "help-section",
                        h2(class = "help-title", "Results and Downloads"),
                        
                        h3(class = "help-subtitle", "Available Downloads"),
                        tags$ul(
                          tags$li(strong("Download All Results:"),
                                  tags$ul(
                                    tags$li("Raw Data"),
                                    tags$li("Housekeeping Gene Analysis"),
                                    tags$li("Complete Analysis Results"),
                                    tags$li("Statistical Analysis"),
                                    tags$li("Modification History"),
                                    tags$li("Analysis Parameters")
                                  ),
                                  div(class = "note-box",
                                      "Note: 'Download All Results' is only available after running analysis"
                                  )
                          ),
                          tags$li(strong("Individual Tables:"),
                                  tags$ul(
                                    tags$li("Raw data table: Copy, CSV, or Excel export options"),
                                    tags$li("Analysis results: Copy, CSV, or Excel export options")
                                  )
                          ),
                          tags$li(strong("Plots:"),
                                  tags$ul(
                                    tags$li("PDF: Best for publication"),
                                    tags$li("PNG: High resolution (300 dpi) for presentations")
                                  )
                          )
                        ),
                        # Data results table screenshot
                        div(style = "text-align: center; margin: 15px 0;",
                            tags$img(src = "screenshots/table_results.png", 
                                    alt = "Data results table with export options",
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
                        h2(class = "help-title", "Tips and Troubleshooting"),
                        
                        div(class = "warning-box",
                            h4("Common Issues:"),
                            tags$ul(
                              tags$li("Missing or incorrectly named columns in data file"),
                              tags$li("No housekeeping genes detected/selected"),
                              tags$li("Invalid Cq values (should be between 0-40)"),
                              tags$li("Insufficient replicates (minimum 2 recommended)"),
                              tags$li("High CV values (>5%) indicating poor replicate consistency"),
                              tags$li("Excel files: Ensure data is in the first sheet or select correct sheet")
                            )
                        ),
                        
                        h3(class = "help-subtitle", "Best Practices"),
                        tags$ul(
                          tags$li("Always review QC plots before proceeding with analysis"),
                          tags$li("Document reasons for excluding data points"),
                          tags$li("Use geometric mean of multiple housekeeping genes (MIQE guideline)"),
                          tags$li("Perform statistics on ΔΔCt values for proper normalization"),
                          tags$li("Apply multiple testing correction for many comparisons"),
                          tags$li("Consider test assumptions: ANOVA for normal data, Kruskal-Wallis otherwise"),
                          tags$li("Export complete results including all parameters and modifications")
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
