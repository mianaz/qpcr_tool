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
      "en" = "Use geometric mean of multiple housekeeping genes (MIQE recommended)",
      "zh" = "使用多个管家基因的几何平均值（MIQE推荐）"
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