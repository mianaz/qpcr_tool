# ui.R
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
library(shinythemes)
library(sortable)

shinyUI(navbarPage(
  title = "qPCR Analysis - ddCt Method",
  theme = shinytheme("flatly"),
  
  # Analysis Tab
  tabPanel("Analysis",
           useShinyjs(),
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
               
               # File upload and Run Analysis section
               wellPanel(
                 fileInput("file", "Upload CSV or Excel File",
                           accept = c("text/csv", ".csv", ".xlsx", ".xls"),
                           multiple = FALSE),
                 
                 tags$small(class = "info-text",
                            "Required columns: Sample, Target, Cq. Supports CSV and Excel files."),
                 
                 conditionalPanel(
                   condition = "typeof input.file !== 'undefined'",
                   tags$div(
                     uiOutput("sheetSelection"),
                     uiOutput("validationMessages")
                   )
                 ),
                 
                 # Run Analysis and Download All Results
                 conditionalPanel(
                   condition = "typeof input.file !== 'undefined'",
                   actionButton("runAnalysis", "Run Analysis", 
                                class = "btn-primary",
                                style = "width: 100%; margin-top: 10px; margin-bottom: 10px;"),
                   uiOutput("downloadAllResultsButton")
                 )),
               
               # Reference/Control Selection Panel
               conditionalPanel(
                 condition = "typeof input.file !== 'undefined'",
                 wellPanel(
                   tags$div(class = "section-divider"),
                   h4("Analysis Settings", style = "color: #2c3e50;"),
                   
                   selectInput("controlSample", "Control Sample",
                               choices = NULL),
                   
                   # Housekeeping gene section
                   div(class = "housekeeping-section",
                       h4("Housekeeping Gene Settings"),
                       checkboxInput("autoDetectHK", 
                                     "Auto-detect housekeeping genes", 
                                     value = TRUE),
                       conditionalPanel(
                         condition = "input.autoDetectHK == true",
                         textInput("hkPattern",
                                   "Detection pattern",
                                   value = "RNA|GAPDH|ACTB|18S")
                       ),
                       conditionalPanel(
                         condition = "input.autoDetectHK == false",
                         selectInput("manualHKGenes",
                                     "Select housekeeping gene(s)",
                                     choices = NULL,
                                     multiple = TRUE)
                       ),
                       checkboxInput("useGeometricMean",
                                     "Use geometric mean of multiple housekeeping genes",
                                     value = TRUE)
                   )
                )
               ),
               
               # Statistical Settings Panel
               conditionalPanel(
                 condition = "typeof input.file !== 'undefined'",
                 wellPanel(
                   tags$div(class = "section-divider"),
                   h4("Statistical Settings", style = "color: #2c3e50;"),
                   
                   selectInput("replicateHandling", 
                               "Technical Replicate Handling",
                               choices = c("Mean" = "mean",
                                           "Median" = "median",
                                           "Geometric Mean" = "geomean",
                                           "Keep All" = "keep")),
                   
                   selectInput("statsTest", "Statistical Test",
                               choices = c("One-way ANOVA" = "anova",
                                           "Kruskal-Wallis" = "kruskal")),
                   
                   numericInput("pThreshold", 
                                "Significance Threshold", 
                                value = 0.05, 
                                min = 0.001, 
                                max = 0.1, 
                                step = 0.001)
                 )
               ),
               
               # Plot Settings Panel
               conditionalPanel(
                 condition = "typeof input.file !== 'undefined'",
                 wellPanel(
                   tags$div(class = "section-divider"),
                   h4("Plot Settings", style = "color: #2c3e50;"),
                   
                   # Sample Order moved here
                   h5("Sample Order", style = "margin-top: 15px;"),
                   uiOutput("sampleOrderUI"),
                   actionButton("updateOrder", "Update Plot Order", 
                                class = "btn-info",
                                style = "width: 100%; margin-top: 10px;"),
                   
                   # Plot Title moved here
                   textInput("plotTitle", 
                             "Plot Title", 
                             value = "Relative Expression Analysis"),
                   
                   # Existing plot settings
                   checkboxInput("showIndividualPoints", 
                                 "Show Individual Points", 
                                 value = TRUE),
                   
                   checkboxInput("showSignificance", 
                                 "Show Statistical Significance", 
                                 value = TRUE),
                   
                   conditionalPanel(
                     condition = "input.showSignificance == true",
                     checkboxInput("showNonSignificant",
                                   "Show Non-significant Comparisons",
                                   value = FALSE),
                     selectInput("significanceType", 
                                 "Significance Display",
                                 choices = c("Stars" = "stars",
                                             "P-values" = "p.value",
                                             "Both" = "both"))
                   ),
                   
                   selectInput("plotType", "Plot Type",
                               choices = c("Bar + Points" = "bar",
                                           "Box Plot" = "box",
                                           "Violin Plot" = "violin")),
                   
                   selectInput("errorBar", "Error Bar Type",
                               choices = c("Standard Error" = "se",
                                           "Standard Deviation" = "sd",
                                           "95% Confidence Interval" = "ci")),
                   
                   selectInput("colorPalette", "Color Palette",
                               choices = c("Classic (Default)" = "classic",
                                           "Black & White" = "bw",
                                           "Grayscale" = "grey",
                                           "Nature (NPG)" = "npg",
                                           "Science (AAAS)" = "aaas",
                                           "NEJM" = "nejm",
                                           "Lancet" = "lancet",
                                           "JAMA" = "jama",
                                           "JCO" = "jco",
                                           "UCSCGB" = "ucscgb",
                                           "D3" = "d3",
                                           "IGV" = "igv",
                                           "Material" = "material",
                                           "Economist" = "economist",
                                           "FiveThirtyEight" = "fivethirtyeight",
                                           "Tableau" = "tableau",
                                           "Stata" = "stata",
                                           "Excel" = "excel",
                                           "Wall Street Journal" = "wsj",
                                           "Calc" = "calc",
                                           "Highcharts" = "hc",
                                           "Pander" = "pander",
                                           "Colorblind Friendly" = "viridis",
                                           "Set1 (Bright)" = "set1",
                                           "Set2 (Pastel)" = "set2",
                                           "Set3 (Light)" = "set3",
                                           "Dark2" = "dark2",
                                           "Paired" = "paired"),
                               selected = "classic"),
                   
                   numericInput("plotHeight", "Plot Height (inches)", 
                                value = 8, min = 2, max = 20),
                   numericInput("plotWidth", "Plot Width (inches)", 
                                value = 10, min = 2, max = 20),
                   sliderInput("fontSize", "Font Size", 
                               min = 8, max = 20, value = 12),
                   numericInput("facetCols", "Number of Columns", 
                               min = 1, max = 6, value = 3, 
                               step = 1)
                 )
               )
             ),
             
             # Main Panel modifications
             mainPanel(
               width = 9,
               tabsetPanel(
                 # Data Preview Tab
                 tabPanel("Data Preview",
                          fluidRow(
                            column(12,
                                   tabsetPanel(
                                     # Raw Data tab
                                     tabPanel("Raw Data",
                                              fluidRow(
                                                column(12,
                                                       div(style = "margin: 10px 0;",
                                                           actionButton("excludeSelected", 
                                                                        "Exclude Selected Rows",
                                                                        class = "btn-info"),
                                                           actionButton("clearExclusions", 
                                                                        "Clear All Exclusions",
                                                                        class = "btn-warning")
                                                       )
                                                )
                                              ),
                                              div(style = "margin-top: 10px;",
                                                  tags$p(class = "info-text", 
                                                         "Double-click cells to edit values. Technical replicates are grouped together."),
                                                  DT::dataTableOutput("rawDataPreview"),
                                                  verbatimTextOutput("excludedPointsSummary"))
                                     ),
                                     
                                     # QC Plot tab
                                     tabPanel("Quality Control",
                                              h4("Quality Control Plots"),
                                              plotOutput("qcPlot", height = "800px"))
                                   ))
                          )),
                 
                 # Analysis Results Tab
                 tabPanel("Analysis Results",
                          uiOutput("analysisMessage"),  # Add this line
                          fluidRow(
                            column(12,
                                   div(class = "plot-container",
                                       plotOutput("foldChangePlot", height = "600px"),
                                       div(style = "margin-top: 10px; text-align: left;",
                                           selectInput("plotFormat", "Plot Format",
                                                       choices = c("PDF" = "pdf", "PNG" = "png"),
                                                       selected = "pdf",
                                                       width = "100px"
                                           ),
                                           downloadButton("downloadPlot", "Download Plot",
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
                 tabPanel("Statistical Analysis",
                          fluidRow(
                            column(12,
                                   verbatimTextOutput("statsOutput"),
                                   plotOutput("statsPlot"))
                          ))
               ))
  )),
  
  # Help Tab
  tabPanel("Help",
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
        "))
                    ),
                    
                    # Main help content
                    div(class = "help-section",
                        h2(class = "help-title", "Quick Start Guide"),
                        p("This application performs qPCR data analysis using the ddCt method. Follow these steps to analyze your data:"),
                        
                        tags$ol(
                          tags$li(strong("Upload Data"), 
                                  p("Click 'Upload CSV File' and select your qPCR data file."),
                                  div(class = "note-box",
                                      "Required columns:",
                                      tags$ul(
                                        tags$li("Sample: Sample identifiers"),
                                        tags$li("Target: Gene names"),
                                        tags$li("Cq: Quantification cycle values"),
                                        tags$li("Well: PCR well locations (optional but recommended)")
                                      )
                                  )
                          ),
                          
                          tags$li(strong("Run Analysis"), 
                                  p("After uploading data:"),
                                  tags$ul(
                                    tags$li("Select your control sample"),
                                    tags$li("Configure housekeeping genes (auto-detect or manual selection)"),
                                    tags$li("Click 'Run Analysis' button"),
                                    tags$li("Analysis must be run before results can be downloaded")
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
                        
                        h3(class = "help-subtitle", "2. Analysis Settings"),
                        tags$ul(
                          tags$li(strong("Housekeeping Genes:"),
                                  tags$ul(
                                    tags$li("Auto-detect option uses common gene patterns (RNA18S, GAPDH, etc.)"),
                                    tags$li("Manual selection allows specific gene choices"),
                                    tags$li("Multiple housekeeping genes can be used with geometric mean option")
                                  )
                          ),
                          tags$li(strong("Statistical Analysis:"),
                                  tags$ul(
                                    tags$li("One-way ANOVA: For normally distributed data"),
                                    tags$li("Kruskal-Wallis: Non-parametric alternative"),
                                    tags$li("Set significance threshold (default: 0.05)")
                                  )
                          )
                        ),
                        
                        h3(class = "help-subtitle", "3. Visualization Options"),
                        tags$ul(
                          tags$li(strong("Plot Types:"),
                                  tags$ul(
                                    tags$li("Bar + Points: Shows mean with individual values"),
                                    tags$li("Box Plot: Displays distribution statistics"),
                                    tags$li("Violin Plot: Shows data density distribution")
                                  )
                          ),
                          tags$li(strong("Customization:"),
                                  tags$ul(
                                    tags$li("Error bars: SE, SD, or 95% CI"),
                                    tags$li("Color schemes: Default, Colorblind-friendly, Publication-ready"),
                                    tags$li("Sample order can be customized via drag-and-drop"),
                                    tags$li("Export formats: PDF or PNG (300 dpi)")
                                  )
                          )
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
                        )
                    ),
                    
                    div(class = "help-section",
                        h2(class = "help-title", "Tips and Troubleshooting"),
                        
                        div(class = "warning-box",
                            h4("Common Issues:"),
                            tags$ul(
                              tags$li("Missing or incorrectly named columns in CSV file"),
                              tags$li("No housekeeping genes detected/selected"),
                              tags$li("Invalid Cq values (should be between 0-40)"),
                              tags$li("Insufficient replicates (minimum 2 recommended)"),
                              tags$li("High CV values (>5%) indicating poor replicate consistency")
                            )
                        ),
                        
                        h3(class = "help-subtitle", "Best Practices"),
                        tags$ul(
                          tags$li("Always review QC plots before proceeding with analysis"),
                          tags$li("Document reasons for excluding data points"),
                          tags$li("Use multiple housekeeping genes when possible"),
                          tags$li("Consider statistical test assumptions when choosing between ANOVA and Kruskal-Wallis"),
                          tags$li("Save comprehensive results for documentation")
                        )
                    )
             )
           )
  )
))