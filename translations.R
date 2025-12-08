# Translation system
tr <- function(key, lang = "en") {
  translations <- list(
    "app_title" = list(
      "en" = "qPCR Analysis - ΔΔCt Method",
      "zh" = "qPCR分析 - ΔΔCt方法"
    ),
    "analysis_tab" = list(
      "en" = "Analysis",
      "zh" = "分析"
    ),
    "help_tab" = list(
      "en" = "Help",
      "zh" = "帮助"
    ),
    "language_label" = list(
      "en" = "Language",
      "zh" = "语言"
    ),
    
    "step1_title" = list(
      "en" = "Step 1: Data Upload",
      "zh" = "步骤1：数据上传"
    ),
    "upload_file" = list(
      "en" = "Upload CSV or Excel File",
      "zh" = "上传CSV或Excel文件"
    ),
    "upload_help" = list(
      "en" = "Required columns: Sample, Target, Cq. Supports CSV and Excel files.",
      "zh" = "必须包含列：Sample（样本名称）、Target（目标基因）、Cq（定量循环数）。支持CSV和Excel格式。"
    ),
    "select_sheet" = list(
      "en" = "Select Sheet:",
      "zh" = "选择工作表："
    ),
    
    "step2_title" = list(
      "en" = "Step 2: Essential Settings",
      "zh" = "步骤2：基本设置"
    ),
    "control_sample" = list(
      "en" = "Control Sample",
      "zh" = "对照样本"
    ),
    "auto_detect_hk" = list(
      "en" = "Auto-detect housekeeping genes",
      "zh" = "自动检测管家基因"
    ),
    "manual_hk_genes" = list(
      "en" = "Select housekeeping gene(s)",
      "zh" = "选择管家基因"
    ),
    "advanced_hk_settings" = list(
      "en" = "Advanced housekeeping settings",
      "zh" = "高级管家基因设置"
    ),
    "detection_pattern" = list(
      "en" = "Detection pattern",
      "zh" = "检测规则"
    ),
    "geometric_mean" = list(
      "en" = "Use geometric mean of housekeeping gene values",
      "zh" = "使用管家基因值的几何平均值"
    ),

    # PCR Efficiency settings (MIQE 2.0)
    "efficiency_settings" = list(
      "en" = "PCR Efficiency Settings (MIQE 2.0)",
      "zh" = "PCR效率设置（MIQE 2.0）"
    ),
    "use_efficiency_correction" = list(
      "en" = "Use efficiency-corrected calculations (Pfaffl method)",
      "zh" = "使用效率校正计算（Pfaffl方法）"
    ),
    "efficiency_input_method" = list(
      "en" = "Efficiency Input Method:",
      "zh" = "效率输入方式："
    ),
    "manual_entry" = list(
      "en" = "Manual Entry",
      "zh" = "手动输入"
    ),
    "standard_curve" = list(
      "en" = "Calculate from Standard Curve",
      "zh" = "从标准曲线计算"
    ),
    "efficiency_help" = list(
      "en" = "Enter efficiency as percentage (90-110%). Values outside this range will show a warning.",
      "zh" = "输入效率百分比（90-110%）。超出此范围将显示警告。"
    ),
    "upload_std_curve" = list(
      "en" = "Upload Standard Curve Data",
      "zh" = "上传标准曲线数据"
    ),
    "calculate_efficiency" = list(
      "en" = "Calculate Efficiency",
      "zh" = "计算效率"
    ),
    "efficiency_for_target" = list(
      "en" = "Efficiency for",
      "zh" = "效率："
    ),
    "efficiency_warning_low" = list(
      "en" = "Warning: Efficiency below 90% may indicate primer problems",
      "zh" = "警告：效率低于90%可能表示引物问题"
    ),
    "efficiency_warning_high" = list(
      "en" = "Warning: Efficiency above 110% may indicate contamination or pipetting errors",
      "zh" = "警告：效率高于110%可能表示污染或移液误差"
    ),
    "calculated_efficiency" = list(
      "en" = "Calculated Efficiency",
      "zh" = "计算的效率"
    ),
    "std_curve_slope" = list(
      "en" = "Slope",
      "zh" = "斜率"
    ),
    "std_curve_r2" = list(
      "en" = "R²",
      "zh" = "R²"
    ),

    "run_analysis" = list(
      "en" = "Run Analysis",
      "zh" = "运行分析"
    ),
    "download_all_results" = list(
      "en" = "Download All Results",
      "zh" = "下载所有结果"
    ),
    
    "statistical_settings" = list(
      "en" = "Statistical Settings",
      "zh" = "统计设置"
    ),
    "statistical_test" = list(
      "en" = "Statistical Test",
      "zh" = "统计检验"
    ),
    "test_on" = list(
      "en" = "Test on:",
      "zh" = "检验对象："
    ),
    "advanced_statistics" = list(
      "en" = "Advanced statistics",
      "zh" = "高级统计"
    ),
    "p_adjust_method" = list(
      "en" = "P-value Adjustment Method",
      "zh" = "P值调整方法"
    ),
    "significance_threshold" = list(
      "en" = "Significance Threshold",
      "zh" = "显著性阈值"
    ),
    
    "one_way_anova" = list(
      "en" = "One-way ANOVA",
      "zh" = "单因素方差分析"
    ),
    "kruskal_wallis" = list(
      "en" = "Kruskal-Wallis",
      "zh" = "Kruskal-Wallis检验"
    ),
    "ddct_values" = list(
      "en" = "ΔΔCt Values",
      "zh" = "ΔΔCt值"
    ),
    "dct_values" = list(
      "en" = "ΔCt Values",
      "zh" = "ΔCt值"
    ),
    
    "plot_customization" = list(
      "en" = "Plot Customization",
      "zh" = "图表自定义"
    ),
    "plot_title" = list(
      "en" = "Plot Title",
      "zh" = "图表标题"
    ),
    "plot_title_default" = list(
      "en" = "Relative Expression Analysis",
      "zh" = "相对表达量分析"
    ),
    "data_display_type" = list(
      "en" = "Data Display Type",
      "zh" = "数据展示类型"
    ),
    "plot_type" = list(
      "en" = "Plot Type",
      "zh" = "图表类型"
    ),
    "error_bar_type" = list(
      "en" = "Error Bar Type",
      "zh" = "误差条类型"
    ),
    "advanced_plot_settings" = list(
      "en" = "Advanced plot settings",
      "zh" = "高级图表设置"
    ),
    "sample_order" = list(
      "en" = "Sample Order",
      "zh" = "样本顺序"
    ),
    "update_plot" = list(
      "en" = "Update Plot",
      "zh" = "更新图表"
    ),
    "show_individual_points" = list(
      "en" = "Show Individual Points",
      "zh" = "显示单个数据点"
    ),
    "show_significance" = list(
      "en" = "Show Statistical Significance",
      "zh" = "显示统计显著性"
    ),
    "show_non_significant" = list(
      "en" = "Show Non-significant Comparisons",
      "zh" = "显示非显著差异"
    ),
    "significance_display" = list(
      "en" = "Significance Display",
      "zh" = "显著性标注"
    ),
    "color_palette" = list(
      "en" = "Color Palette",
      "zh" = "颜色方案"
    ),
    "size_adjustments" = list(
      "en" = "Size Adjustments",
      "zh" = "大小调整"
    ),
    "plot_height" = list(
      "en" = "Plot Height (inches)",
      "zh" = "图表高度（英寸）"
    ),
    "plot_width" = list(
      "en" = "Plot Width (inches)",
      "zh" = "图表宽度（英寸）"
    ),
    "font_size" = list(
      "en" = "Font Size",
      "zh" = "字体大小"
    ),
    "point_size" = list(
      "en" = "Point Size",
      "zh" = "点大小"
    ),
    "facet_cols" = list(
      "en" = "Number of Columns",
      "zh" = "图表列数"
    ),
    
    "fold_change" = list(
      "en" = "Fold Change (2^-ΔΔCt)",
      "zh" = "表达倍数 (2^-ΔΔCt)"
    ),
    "fold_change_corrected" = list(
      "en" = "Fold Change - Efficiency Corrected (E^-ΔΔCt)",
      "zh" = "表达倍数 - 效率校正 (E^-ΔΔCt)"
    ),
    "relative_quantity" = list(
      "en" = "Relative Quantity (2^-ΔCt)",
      "zh" = "相对表达量 (2^-ΔCt)"
    ),
    "relative_quantity_corrected" = list(
      "en" = "Relative Quantity - Efficiency Corrected (E^-ΔCt)",
      "zh" = "相对表达量 - 效率校正 (E^-ΔCt)"
    ),
    "neg_ddct_values" = list(
      "en" = "-ΔΔCt Values",
      "zh" = "-ΔΔCt值"
    ),
    "neg_dct_values" = list(
      "en" = "-ΔCt Values",
      "zh" = "-ΔCt值"
    ),
    
    "bar_plot" = list(
      "en" = "Bar Plot",
      "zh" = "柱状图"
    ),
    "box_plot" = list(
      "en" = "Box Plot",
      "zh" = "箱线图"
    ),
    "violin_plot" = list(
      "en" = "Violin Plot",
      "zh" = "小提琴密度图"
    ),
    "beeswarm_plot" = list(
      "en" = "Beeswarm Plot",
      "zh" = "蜂群散点图"
    ),
    
    "standard_error" = list(
      "en" = "Standard Error",
      "zh" = "标准误差"
    ),
    "standard_deviation" = list(
      "en" = "Standard Deviation",
      "zh" = "标准差"
    ),
    "confidence_interval" = list(
      "en" = "95% Confidence Interval",
      "zh" = "95%置信区间"
    ),
    
    "classic_default" = list(
      "en" = "Classic (Default)",
      "zh" = "经典（默认）"
    ),
    "colorblind_friendly" = list(
      "en" = "Colorblind Friendly",
      "zh" = "色盲友好"
    ),
    "black_white" = list(
      "en" = "Black & White",
      "zh" = "黑白"
    ),
    "grayscale" = list(
      "en" = "Grayscale",
      "zh" = "灰度"
    ),
    "nature_npg" = list(
      "en" = "Nature (NPG)",
      "zh" = "Nature期刊"
    ),
    "science_aaas" = list(
      "en" = "Science (AAAS)",
      "zh" = "Science期刊"
    ),
    "nejm" = list(
      "en" = "NEJM",
      "zh" = "新英格兰医学"
    ),
    "lancet" = list(
      "en" = "Lancet",
      "zh" = "柳叶刀"
    ),
    "jama" = list(
      "en" = "JAMA",
      "zh" = "美国医学会杂志"
    ),
    "set1_bright" = list(
      "en" = "Set1 (Bright)",
      "zh" = "Set1（明亮色）"
    ),
    "set2_pastel" = list(
      "en" = "Set2 (Pastel)",
      "zh" = "Set2（柔和色）"
    ),
    "dark2" = list(
      "en" = "Dark2",
      "zh" = "深色系"
    ),
    "paired" = list(
      "en" = "Paired",
      "zh" = "配对色"
    ),
    
    "data_preview" = list(
      "en" = "Data Preview",
      "zh" = "数据预览"
    ),
    "raw_data" = list(
      "en" = "Raw Data",
      "zh" = "原始数据"
    ),
    "quality_control" = list(
      "en" = "Quality Control",
      "zh" = "质量控制"
    ),
    "analysis_results" = list(
      "en" = "Analysis Results",
      "zh" = "分析结果"
    ),
    "statistical_analysis" = list(
      "en" = "Statistical Analysis",
      "zh" = "统计分析"
    ),
    
    "exclude_selected" = list(
      "en" = "Exclude Selected Rows",
      "zh" = "排除所选数据"
    ),
    "clear_exclusions" = list(
      "en" = "Clear All Exclusions",
      "zh" = "清除所有排除"
    ),
    "data_preview_help" = list(
      "en" = "Double-click cells to edit values. Technical replicates are grouped together.",
      "zh" = "双击单元格编辑数值。技术重复组会自动显示在相邻行。"
    ),
    "qc_plots" = list(
      "en" = "Quality Control Plots",
      "zh" = "质量控制图"
    ),
    
    "plot_format" = list(
      "en" = "Plot Format",
      "zh" = "图表格式"
    ),
    "download_plot" = list(
      "en" = "Download Plot",
      "zh" = "下载图表"
    ),
    "pdf_format" = list(
      "en" = "PDF",
      "zh" = "PDF"
    ),
    "png_format" = list(
      "en" = "PNG", 
      "zh" = "PNG"
    ),
    
    "none" = list(
      "en" = "None",
      "zh" = "无"
    ),
    "bonferroni" = list(
      "en" = "Bonferroni",
      "zh" = "Bonferroni"
    ),
    "holm" = list(
      "en" = "Holm",
      "zh" = "Holm"
    ),
    "hochberg" = list(
      "en" = "Hochberg",
      "zh" = "Hochberg"
    ),
    "hommel" = list(
      "en" = "Hommel",
      "zh" = "Hommel"
    ),
    "fdr_bh" = list(
      "en" = "FDR (Benjamini-Hochberg)",
      "zh" = "FDR（Benjamini-Hochberg）"
    ),
    "benjamini_yekutieli" = list(
      "en" = "Benjamini-Yekutieli",
      "zh" = "Benjamini-Yekutieli"
    ),
    
    "stars" = list(
      "en" = "Stars",
      "zh" = "星号"
    ),
    "p_values" = list(
      "en" = "P-values",
      "zh" = "P值"
    ),
    "both" = list(
      "en" = "Both",
      "zh" = "星号+P值"
    ),
    
    "analysis_not_run" = list(
      "en" = "Analysis Not Run",
      "zh" = "分析未运行"
    ),
    "run_analysis_first" = list(
      "en" = "Please run the analysis first using the 'Run Analysis' button in the sidebar.",
      "zh" = "请先使用侧边栏中的'运行分析'按钮运行分析。"
    ),
    "run_analysis_tooltip" = list(
      "en" = "Please run analysis first",
      "zh" = "请先运行分析"
    ),
    "drag_reorder_samples" = list(
      "en" = "Drag to reorder samples",
      "zh" = "拖动重新排列样本"
    ),
    "footer_attribution" = list(
      "en" = "Developed by",
      "zh" = "由"
    ),
    "footer_github_text" = list(
      "en" = "mianaz/qpcr_tool",
      "zh" = "mianaz/qpcr_tool"
    ),
    "footer_license_support" = list(
      "en" = " | MIT License | Support: support@bioinfospace.com",
      "zh" = " | MIT 许可证 | 技术支持：support@bioinfospace.com"
    ),
    "data_privacy_title" = list(
      "en" = "Data Privacy & Security",
      "zh" = "数据隐私与安全"
    ),
    "data_secure_title" = list(
      "en" = "🔒 Your Data is Secure:",
      "zh" = "🔒 您的数据是安全的："
    ),
    "session_management_title" = list(
      "en" = "⚠️ Session Management:",
      "zh" = "⚠️ 会话管理："
    ),
    "no_server_storage" = list(
      "en" = "No Permanent Storage: Your data is processed temporarily on the server and never permanently saved or stored",
      "zh" = "无永久存储：您的数据在服务器上临时处理，从不永久保存或存储"
    ),
    "session_only_processing" = list(
      "en" = "Session-Only Processing: All files and results are automatically deleted when your session ends",
      "zh" = "仅会话处理：会话结束时自动删除所有文件和结果"
    ),
    "local_processing" = list(
      "en" = "Server Processing: Calculations occur on the cloud server in an isolated R session",
      "zh" = "服务器处理：计算在云服务器的独立R会话中进行"
    ),
    "no_data_transmission" = list(
      "en" = "No Data Transmission: Your research data remains within the secure processing environment",
      "zh" = "无数据传输：您的研究数据保留在安全的处理环境内"
    ),
    "starting_fresh" = list(
      "en" = "Starting Fresh: To begin a new analysis, refresh your browser page (previous results will be lost)",
      "zh" = "重新开始：要开始新的分析，请刷新浏览器页面（先前的结果将丢失）"
    ),
    "save_your_work" = list(
      "en" = "Save Your Work: Use 'Download All Results' to preserve your complete analysis before closing",
      "zh" = "保存工作：关闭前使用'下载所有结果'来保存完整分析"
    ),
    "session_duration" = list(
      "en" = "Session Duration: Your results remain available during your current browser session only",
      "zh" = "会话持续时间：您的结果仅在当前浏览器会话期间可用"
    ),
    "memory_cleanup" = list(
      "en" = "Memory Cleanup: Data is automatically cleared from memory when sessions end",
      "zh" = "内存清理：会话结束时数据自动从内存中清除"
    ),
    
    # Help section translations
    "help_quick_start_guide" = list(
      "en" = "Quick Start Guide",
      "zh" = "快速入门指南"
    ),
    "help_intro_text" = list(
      "en" = "This application performs qPCR data analysis using the ΔΔCt method with MIQE-compliant statistical analysis.",
      "zh" = "本应用使用ΔΔCt方法进行qPCR数据分析，提供符合MIQE标准的统计分析。"
    ),
    "help_step1_data_upload" = list(
      "en" = "Step 1: Data Upload",
      "zh" = "步骤1：数据上传"
    ),
    "help_step1_description" = list(
      "en" = "Upload your qPCR data file (CSV or Excel format).",
      "zh" = "上传您的qPCR数据文件（CSV或Excel格式）。"
    ),
    "help_file_upload_alt" = list(
      "en" = "File upload interface",
      "zh" = "文件上传界面"
    ),
    "help_required_columns" = list(
      "en" = "Required columns:",
      "zh" = "必需列："
    ),
    "help_column_sample" = list(
      "en" = "Sample: Sample identifiers",
      "zh" = "Sample：样本标识符"
    ),
    "help_column_target" = list(
      "en" = "Target: Gene names",
      "zh" = "Target：基因名称"
    ),
    "help_column_cq" = list(
      "en" = "Cq: Quantification cycle values (0-40)",
      "zh" = "Cq：定量循环值（0-40）"
    ),
    "help_column_well" = list(
      "en" = "Well: PCR well locations (optional but recommended)",
      "zh" = "Well：PCR孔位置（可选但建议）"
    ),
    "help_sample_data_alt" = list(
      "en" = "Example of properly formatted qPCR data",
      "zh" = "正确格式的qPCR数据示例"
    ),
    "help_step2_settings" = list(
      "en" = "Step 2: Configure Essential Settings",
      "zh" = "步骤2：配置基本设置"
    ),
    "help_step2_select_control" = list(
      "en" = "Select your control/reference sample",
      "zh" = "选择您的对照/参考样本"
    ),
    "help_step2_configure_hk" = list(
      "en" = "Configure housekeeping genes:",
      "zh" = "配置管家基因："
    ),
    "help_step2_auto_detect" = list(
      "en" = "Auto-detect: Uses pattern matching (e.g., GAPDH, ACTB, 18S)",
      "zh" = "自动检测：使用模式匹配（如GAPDH、ACTB、18S）"
    ),
    "help_step2_manual_select" = list(
      "en" = "Manual: Select specific genes from dropdown",
      "zh" = "手动：从下拉菜单选择特定基因"
    ),
    "help_step2_advanced" = list(
      "en" = "Advanced settings available in collapsible sections",
      "zh" = "折叠部分提供高级设置"
    ),
    "help_essential_settings_alt" = list(
      "en" = "Essential settings panel",
      "zh" = "基本设置面板"
    ),
    "help_step3_run_analysis" = list(
      "en" = "Step 3: Run Analysis",
      "zh" = "步骤3：运行分析"
    ),
    "help_step3_click_run" = list(
      "en" = "Click 'Run Analysis' to process your data.",
      "zh" = "点击'运行分析'处理您的数据。"
    ),
    "help_step3_results" = list(
      "en" = "Results include ΔCt, ΔΔCt, fold changes, and statistical comparisons.",
      "zh" = "结果包括ΔCt、ΔΔCt、表达倍数和统计比较。"
    ),
    "help_analysis_results_alt" = list(
      "en" = "Analysis results with fold change plot",
      "zh" = "包含表达倍数图的分析结果"
    ),
    "help_data_analysis_process" = list(
      "en" = "Data Analysis Process",
      "zh" = "数据分析流程"
    ),
    "help_qc_title" = list(
      "en" = "1. Data Quality Control",
      "zh" = "1. 数据质量控制"
    ),
    "help_qc_review_plots" = list(
      "en" = "Review QC plots in 'Quality Control' tab",
      "zh" = "在'质量控制'选项卡中查看QC图"
    ),
    "help_qc_check_distribution" = list(
      "en" = "Check Cq distribution across samples",
      "zh" = "检查样本间Cq分布"
    ),
    "help_qc_monitor_cv" = list(
      "en" = "Monitor CV values (red line indicates 5% threshold)",
      "zh" = "监控CV值（红线表示5%阈值）"
    ),
    "help_qc_handling_outliers" = list(
      "en" = "Handling outliers:",
      "zh" = "处理异常值："
    ),
    "help_qc_select_problematic" = list(
      "en" = "Select problematic data points in 'Raw Data' tab",
      "zh" = "在'原始数据'选项卡中选择异常数据点"
    ),
    "help_qc_exclude_rows" = list(
      "en" = "Use 'Exclude Selected Rows' to remove outliers",
      "zh" = "使用'排除所选数据'移除异常值"
    ),
    "help_qc_track_modifications" = list(
      "en" = "All modifications are tracked and included in final report",
      "zh" = "所有修改都被跟踪并包含在最终报告中"
    ),
    "help_qc_plots_alt" = list(
      "en" = "Quality control plots",
      "zh" = "质量控制图"
    ),
    "help_analysis_settings" = list(
      "en" = "2. Analysis Settings",
      "zh" = "2. 分析设置"
    ),
    "help_hk_genes_title" = list(
      "en" = "Housekeeping Genes:",
      "zh" = "管家基因："
    ),
    "help_hk_auto_detect_desc" = list(
      "en" = "Auto-detect: Uses pattern matching (customizable)",
      "zh" = "自动检测：使用模式匹配（可定制）"
    ),
    "help_hk_manual_selection" = list(
      "en" = "Manual selection: Choose specific genes from your data",
      "zh" = "手动选择：从数据中选择特定基因"
    ),
    "help_hk_geometric_mean" = list(
      "en" = "Geometric mean: MIQE-recommended for multiple housekeeping genes",
      "zh" = "几何平均：MIQE推荐用于多个管家基因"
    ),
    "help_hk_validation" = list(
      "en" = "Automatically validates housekeeping gene stability",
      "zh" = "自动验证管家基因稳定性"
    ),
    "help_statistical_analysis_title" = list(
      "en" = "Statistical Analysis:",
      "zh" = "统计分析："
    ),
    "help_stats_test_on" = list(
      "en" = "Tests performed on ΔΔCt or ΔCt values (selectable)",
      "zh" = "对ΔΔCt或ΔCt值进行检验（可选择）"
    ),
    "help_stats_anova" = list(
      "en" = "One-way ANOVA: For normally distributed data",
      "zh" = "单因素方差分析：用于正态分布数据"
    ),
    "help_stats_kruskal" = list(
      "en" = "Kruskal-Wallis: Non-parametric alternative",
      "zh" = "Kruskal-Wallis：非参数检验替代"
    ),
    "help_stats_correction" = list(
      "en" = "Multiple testing correction methods available:",
      "zh" = "可用的多重检验校正方法："
    ),
    "help_stats_correction_methods" = list(
      "en" = "Bonferroni, Holm, FDR (Benjamini-Hochberg)",
      "zh" = "Bonferroni、Holm、FDR（Benjamini-Hochberg）"
    ),
    "help_stats_correction_methods2" = list(
      "en" = "Hochberg, Hommel, Benjamini-Yekutieli",
      "zh" = "Hochberg、Hommel、Benjamini-Yekutieli"
    ),
    "help_stats_threshold" = list(
      "en" = "Customizable significance threshold",
      "zh" = "可定制显著性阈值"
    ),
    "help_visualization_options" = list(
      "en" = "3. Visualization Options",
      "zh" = "3. 可视化选项"
    ),
    "help_data_display_types" = list(
      "en" = "Data Display Types:",
      "zh" = "数据显示类型："
    ),
    "help_display_fold_change" = list(
      "en" = "Fold Change (2^-ΔΔCt): Traditional relative expression",
      "zh" = "表达倍数（2^-ΔΔCt）：传统相对表达量"
    ),
    "help_display_ddct" = list(
      "en" = "ΔΔCt Values: Log-scale normalized expression differences",
      "zh" = "ΔΔCt值：对数尺度归一化表达差异"
    ),
    "help_display_neg_ddct" = list(
      "en" = "-ΔΔCt Values: Intuitive direction (higher = more expression)",
      "zh" = "-ΔΔCt值：直观方向（数值越高=表达量越多）"
    ),
    "help_display_dct" = list(
      "en" = "ΔCt Values: Ct normalized to housekeeping genes",
      "zh" = "ΔCt值：归一化到管家基因的Ct值"
    ),
    "help_display_neg_dct" = list(
      "en" = "-ΔCt Values: Intuitive ΔCt (higher = more expression)",
      "zh" = "-ΔCt值：直观ΔCt（数值越高=表达量越多）"
    ),
    "help_plot_types" = list(
      "en" = "Plot Types:",
      "zh" = "图表类型："
    ),
    "help_plot_bar" = list(
      "en" = "Bar Plot: Shows mean with optional error bars and points",
      "zh" = "柱状图：显示均值，可选误差条和数据点"
    ),
    "help_plot_box" = list(
      "en" = "Box Plot: Displays quartiles and outliers",
      "zh" = "箱线图：显示四分位数和异常值"
    ),
    "help_plot_violin" = list(
      "en" = "Violin Plot: Shows data density distribution",
      "zh" = "小提琴图：显示数据密度分布"
    ),
    "help_plot_beeswarm" = list(
      "en" = "Beeswarm Plot: Individual points with mean indicator",
      "zh" = "蜂群图：单个数据点带均值指示器"
    ),
    "help_plot_customization_alt" = list(
      "en" = "Plot customization options",
      "zh" = "图表自定义选项"
    ),
    "help_customization" = list(
      "en" = "Customization:",
      "zh" = "自定义："
    ),
    "help_custom_error_bars" = list(
      "en" = "Error bars: SE, SD, or 95% CI",
      "zh" = "误差条：SE、SD或95% CI"
    ),
    "help_custom_colors" = list(
      "en" = "Color palettes: 13+ options including colorblind-friendly",
      "zh" = "调色板：13+种选项包括色盲友好"
    ),
    "help_custom_sizes" = list(
      "en" = "Adjustable font and point sizes",
      "zh" = "可调整字体和点大小"
    ),
    "help_custom_sample_order" = list(
      "en" = "Sample order: Drag-and-drop reordering",
      "zh" = "样本顺序：拖放重排"
    ),
    "help_custom_significance" = list(
      "en" = "Statistical significance display: Stars, p-values, or both",
      "zh" = "统计显著性显示：星号、p值或两者"
    ),
    "help_custom_export" = list(
      "en" = "Export formats: PDF (publication) or PNG (300 dpi)",
      "zh" = "导出格式：PDF（发表用）或PNG（300 dpi）"
    ),
    "help_stats_settings_alt" = list(
      "en" = "Statistical settings panel",
      "zh" = "统计设置面板"
    ),
    "help_results_downloads" = list(
      "en" = "Results and Downloads",
      "zh" = "结果与下载"
    ),
    "help_available_downloads" = list(
      "en" = "Available Downloads",
      "zh" = "可用下载"
    ),
    "help_download_all_results" = list(
      "en" = "Download All Results:",
      "zh" = "下载所有结果："
    ),
    "help_download_raw_data" = list(
      "en" = "Raw Data",
      "zh" = "原始数据"
    ),
    "help_download_hk_analysis" = list(
      "en" = "Housekeeping Gene Analysis",
      "zh" = "管家基因分析"
    ),
    "help_download_complete_analysis" = list(
      "en" = "Complete Analysis Results",
      "zh" = "完整分析结果"
    ),
    "help_download_statistical_analysis" = list(
      "en" = "Statistical Analysis",
      "zh" = "统计分析"
    ),
    "help_download_modification_history" = list(
      "en" = "Modification History",
      "zh" = "修改历史"
    ),
    "help_download_analysis_parameters" = list(
      "en" = "Analysis Parameters",
      "zh" = "分析参数"
    ),
    "help_download_note" = list(
      "en" = "Note: 'Download All Results' is only available after running analysis",
      "zh" = "注意：'下载所有结果'仅在运行分析后可用"
    ),
    "help_individual_tables" = list(
      "en" = "Individual Tables:",
      "zh" = "单独表格："
    ),
    "help_raw_data_export" = list(
      "en" = "Raw data table: Copy, CSV, or Excel export options",
      "zh" = "原始数据表：复制、CSV或Excel导出选项"
    ),
    "help_analysis_results_export" = list(
      "en" = "Analysis results: Copy, CSV, or Excel export options",
      "zh" = "分析结果：复制、CSV或Excel导出选项"
    ),
    "help_plots_export" = list(
      "en" = "Plots:",
      "zh" = "图表："
    ),
    "help_pdf_publication" = list(
      "en" = "PDF: Best for publication",
      "zh" = "PDF：最适合发表"
    ),
    "help_png_presentation" = list(
      "en" = "PNG: High resolution (300 dpi) for presentations",
      "zh" = "PNG：高分辨率（300 dpi）适合演示"
    ),
    "help_table_results_alt" = list(
      "en" = "Data results table with export options",
      "zh" = "带导出选项的数据结果表"
    ),
    "help_tips_troubleshooting" = list(
      "en" = "Tips and Troubleshooting",
      "zh" = "提示与故障排除"
    ),
    "help_common_issues" = list(
      "en" = "Common Issues:",
      "zh" = "常见问题："
    ),
    "help_issue_missing_columns" = list(
      "en" = "Missing or incorrectly named columns in data file",
      "zh" = "数据文件中缺少或错误命名的列"
    ),
    "help_issue_no_hk_genes" = list(
      "en" = "No housekeeping genes detected/selected",
      "zh" = "未检测到/选择管家基因"
    ),
    "help_issue_invalid_cq" = list(
      "en" = "Invalid Cq values (should be between 0-40)",
      "zh" = "无效Cq值（应在0-40之间）"
    ),
    "help_issue_insufficient_replicates" = list(
      "en" = "Insufficient replicates (minimum 2 recommended)",
      "zh" = "重复不足（建议最少2个）"
    ),
    "help_issue_high_cv" = list(
      "en" = "High CV values (>5%) indicating poor replicate consistency",
      "zh" = "高CV值（>5%）表示重复一致性差"
    ),
    "help_issue_excel_sheets" = list(
      "en" = "Excel files: Ensure data is in the first sheet or select correct sheet",
      "zh" = "Excel文件：确保数据在第一个工作表或选择正确工作表"
    ),
    "help_best_practices" = list(
      "en" = "Best Practices",
      "zh" = "最佳实践"
    ),
    "help_practice_review_qc" = list(
      "en" = "Always review QC plots before proceeding with analysis",
      "zh" = "分析前始终检查QC图"
    ),
    "help_practice_document_exclusions" = list(
      "en" = "Document reasons for excluding data points",
      "zh" = "记录排除数据点的原因"
    ),
    "help_practice_geometric_mean" = list(
      "en" = "Use geometric mean of multiple housekeeping genes (MIQE guideline)",
      "zh" = "使用多个管家基因的几何平均（MIQE指南）"
    ),
    "help_practice_ddct_statistics" = list(
      "en" = "Perform statistics on ΔΔCt values for proper normalization",
      "zh" = "对ΔΔCt值进行统计以正确归一化"
    ),
    "help_practice_multiple_testing" = list(
      "en" = "Apply multiple testing correction for many comparisons",
      "zh" = "对多重比较应用多重检验校正"
    ),
    "help_practice_test_assumptions" = list(
      "en" = "Consider test assumptions: ANOVA for normal data, Kruskal-Wallis otherwise",
      "zh" = "考虑检验假设：正态数据用ANOVA，否则用Kruskal-Wallis"
    ),
    "help_practice_export_complete" = list(
      "en" = "Export complete results including all parameters and modifications",
      "zh" = "导出包含所有参数和修改的完整结果"
    ),

    # Plot default title (for encoding safety with Chinese)
    "default_plot_title" = list(
      "en" = "Relative Expression Analysis",
      "zh" = "Relative Expression Analysis"
    ),

    # Border line option
    "show_border_lines" = list(
      "en" = "Show border lines on bars/boxes",
      "zh" = "显示条形图/箱线图边框"
    ),

    # MIQE 2.0 efficiency input method labels
    "efficiency_method_manual" = list(
      "en" = "Manual Entry",
      "zh" = "手动输入"
    ),
    "efficiency_method_standard_curve" = list(
      "en" = "Standard Curve",
      "zh" = "标准曲线"
    ),
    "std_curve_help" = list(
      "en" = "Required columns: Target, Concentration, Cq",
      "zh" = "必须包含列：Target（目标基因）、Concentration（浓度）、Cq"
    ),
    "efficiency_defaults_used" = list(
      "en" = "Note: Using default 100% efficiency (E=2.0) for targets without specified values",
      "zh" = "注意：未指定值的目标使用默认100%效率（E=2.0）"
    )
  )
  
  if (key %in% names(translations)) {
    translation <- translations[[key]][[lang]]
    if (is.null(translation)) {
      translation <- translations[[key]][["en"]]
    }
    return(translation)
  } else {
    return(key)
  }
}

language_options <- list(
  "English" = "en",
  "中文" = "zh"
)