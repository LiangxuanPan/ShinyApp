################################## Shiny 数据分析软件 源代码文件 ##################################
# ui 文件
# server 文件

######################################## 加载库及全局设置 #########################################
library(shiny)
library(shinydashboard)
library(shinythemes)
library(vroom)
library(openxlsx)
library(DT)
library(missForest)
library(DMwR2)
library(raster) # cv函数
library(car)
library(colourpicker)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(pheatmap)
library(Rmisc) # ggplot 排版布局
########################################### ui 文件模块 ###########################################
# 1. 使用介绍模块

########################################## 2. 数据处理模块 ######################################## 
## 2.1、上传原始数据
ui.page.02 <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # 加载数据文件
      fileInput(
        # 文件路径变量名称
        inputId = "file_02",
        # 选择框名称
        label = "选择数据文件 (仅支持xlsx、csv和txt文件)",
        # 接受的文件格式
        accept = c('.xlsx', '.csv', '.txt'),
        # 是否允许一次选择上传多个文件
        multiple = TRUE,
        # 输入框宽度
        width = NULL,
        # 按钮标签
        buttonLabel = "Upload...",
        # 占位文本
        placeholder = "No file selected",
        # 捕获图像、音频或视频数据的来源
        capture = NULL),
      column(
        width = 6,
        textInput("primary_key_02", "Primary_Key", "Genes")),
      column(
        width = 6,
        selectInput('transpose_02', '是否转置',
                    choices = c(FALSE, TRUE),
                    selected = FALSE)),
      textInput("row_del_02", "删除行名(名称以英文逗号切分)", ""),
      textInput("col_del_02", "删除列名(名称以英文逗号切分)", ""),
      textInput("nominal_02", "名义变量(名称以英文逗号切分)", "Samples, Label"),
      numericInput("significance_02", "数值变量(有效数字个数)", 4, min = 1, max = 27),
      checkboxInput("scientific_02", "科学计数法", FALSE),
      fluidRow(
        column(
          width = 6,
          numericInput("missingscale_x_02", "行缺失比例 (%)", 100, min = 0, max = 100)),
        column(
          width = 6,
          numericInput("missingscale_y_02", "列缺失比例 (%)", 100, min = 0, max = 100)),
      ),
      fluidRow(
        column(
          width = 9,
          selectInput('scalemethod_02', '数据变换方法',
                      choices = c('NULL', 'Min-Max', 'Z-Score', 'Robust', 'Log', 'LogScale'),
                      selected = 'NULL')),
        column(
          width = 3,
          numericInput('k_log_02', 'k_log', 2, min = 1, max = 100)
        )
      ),
      fluidRow(
        column(
          width = 9,
          selectInput('imputation_02', '缺失值处理',
                      choices = c('NULL', 'Delete', 'Min', 'Mean', 'RandomForest', 'KNN'),
                      selected = 'NULL')),
        column(
          width = 3,
          numericInput('k_ntree_02', 'k/ntree', 5, min = 1, max = 1000)
        )
      ),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Raw Data Table',
                 dataTableOutput('table_0201', width = "100%")
        ),
        tabPanel('Standard  Data Table',
                 shinycssloaders::withSpinner(dataTableOutput('table_0202', width = "100%")),
                 downloadButton("download_0201", class = "btn-block")
        )
      )
    )
  )
)

########################################## 3. 假设检验模块 ######################################## 
## 3.1、上传假设检验数据
ui.page.03 <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # 加载数据文件
      fileInput(
        # 文件路径变量名称
        inputId = "file_03",
        # 选择框名称
        label = "选择数据文件 (仅支持xlsx、csv和txt文件)",
        # 接受的文件格式
        accept = c('.xlsx', '.csv', '.txt'),
        # 是否允许一次选择上传多个文件
        multiple = TRUE,
        # 输入框宽度
        width = NULL,
        # 按钮标签
        buttonLabel = "Upload...",
        # 占位文本
        placeholder = "No file selected",
        # 捕获图像、音频或视频数据的来源
        capture = NULL),
      column(
        width = 6,
        textInput("primary_key_03", "Primary_Key", "Samples")),
      column(
        width = 6,
        selectInput('transpose_03', '是否转置',
                    choices = c(FALSE, TRUE),
                    selected = FALSE)),
      textInput("row_del_03", "删除行名(名称以英文逗号切分)", ""),
      textInput("col_del_03", "删除列名(名称以英文逗号切分)", ""),
      textInput("nominal_03", "名义变量(名称以英文逗号切分)", "Samples, Label"),
      numericInput("significance_03", "数值变量(有效数字个数)", 4, min = 1, max = 27),
      checkboxInput("scientific_03", "科学计数法", FALSE),
      fluidRow(
        column(
          width = 6,
          numericInput("missingscale_x_03", "行缺失比例 (%)", 100, min = 0, max = 100)),
        column(
          width = 6,
          numericInput("missingscale_y_03", "列缺失比例 (%)", 100, min = 0, max = 100)),
      ),
      fluidRow(
        column(
          width = 12,
          textInput("label_03", "样本标签(列名)", "Label")),
      ),
      fluidRow(
        column(
          width = 6,
          textInput('control_label_03', 'Control组标签(名称以英文逗号切分)', 'H, B')),
        column(
          width = 6,
          textInput('case_label_03', 'Case组标签(名称以英文逗号切分)', 'C')),
      ),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Raw Data Table',
                 dataTableOutput('table_0301', width = "100%")
        ),
        tabPanel('Standard  Data Table',
                 shinycssloaders::withSpinner(dataTableOutput('table_0302', width = "100%"))
        ),
        tabPanel('Testing Result Table',
                 shinycssloaders::withSpinner(dataTableOutput('table_0303', width = "100%")),
                 downloadButton("download_0301", class = "btn-block")
        ),
      )
    )
  )
)


########################################## 4. 火山图模块 ###########################################
## 4.1、上传假设检验数据
ui.page.04 <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # 加载数据文件
      fileInput(
        # 文件路径变量名称
        inputId = "file_04",
        # 选择框名称
        label = "选择数据文件 (仅支持xlsx、csv和txt文件)",
        # 接受的文件格式
        accept = c('.xlsx', '.csv', '.txt'),
        # 是否允许一次选择上传多个文件
        multiple = TRUE,
        # 输入框宽度
        width = NULL,
        # 按钮标签
        buttonLabel = "Upload...",
        # 占位文本
        placeholder = "No file selected",
        # 捕获图像、音频或视频数据的来源
        capture = NULL),
      fluidRow(
        column(
          width = 4,
          textInput("col_gene_04", "Gene列名", "Samples")),
        column(
          width = 4,
          textInput("col_fc_04", "FC列名", "Ratio")),
        column(
          width = 4,
          textInput("col_pval_04", "Pvalue列名", "pValue"))
        ),
      numericInput("significance_04", "数值变量(有效数字个数)", 4, min = 1, max = 27),
      fluidRow(
        column(
          width = 12,
          selectInput('pval_method_04', 'P值修正方法',
                      choices = p.adjust.methods,
                      selected = 'none'))
        ),
      fluidRow(
        column(
          width = 6,
          numericInput("fc_threshold_04", "FC Threshold", 1.5, min = 0)),
        column(
          width = 6,
          numericInput("pval_threshold_04", "Pvalue Threshold", 0.05, min = 0, max = 1)),
        ),
      fluidRow(
        column(
          width = 12,
          textInput("mark_gene_04", "标记基因名(名称以英文逗号切分)", "")),
        ),
      fluidRow(
        column(
          width = 4,
          textInput('title_04', '标题名称', 'Volcano')),
        column(
          width = 4,
          numericInput("xlim_min_04", "X轴范围 (min)", -5, max = 0)),
        column(
          width = 4,
          numericInput("xlim_max_04", "X轴范围 (max)", 5, min = 0)),
        ),
      fluidRow(
        column(width = 4,                                                 
               colourInput("col_up_04", "Color (Up)", "firebrick")),
        column(width = 4,                                                 
               colourInput("col_down_04", "Color (Down)", "blue")),
        column(width = 4,
               colourInput("col_unchanged_04", "Color (Unchanged)", "gray"))
      ),
      fluidRow(
        column(width = 4,                                                 
               sliderInput('point_size_04', 'Scatter size', value = 6.5,
                           min = 1, max = 15, step = 0.5)),
        column(width = 4,                                                 
               sliderInput('text_size_04', 'Text size', value = 8.5,
                           min = 1, max = 15, step = 0.5)),
        column(width = 4,
               sliderInput('alpha_04', 'Alpha', value = 0.7,
                           min = 0, max = 1, step = 0.1))
      ),
      width = 4
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel('Raw Data Table',
                 dataTableOutput('table_0401', width = "100%")
                 ),
        tabPanel('Standard  Data Table',
                 shinycssloaders::withSpinner(dataTableOutput('table_0402', width = "100%")),
                 downloadButton("download_0401", class = "btn-block")
                 ),
        tabPanel('Volcano Plot',
                 shinycssloaders::withSpinner(plotOutput('plot_0403', width = "100%", height = "750px")),
                 downloadButton("download_0402", class = "btn-block")
                 )
        )
      )
    )
  )

########################################## 5. 热图模块 ###########################################
## 5.1、上传原始表达量数据
ui.page.05 <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # 加载数据文件
      fileInput(
        # 文件路径变量名称
        inputId = "file_05",
        # 选择框名称
        label = "选择数据文件 (仅支持xlsx、csv和txt文件)",
        # 接受的文件格式
        accept = c('.xlsx', '.csv', '.txt'),
        # 是否允许一次选择上传多个文件
        multiple = TRUE,
        # 输入框宽度
        width = NULL,
        # 按钮标签
        buttonLabel = "Upload...",
        # 占位文本
        placeholder = "No file selected",
        # 捕获图像、音频或视频数据的来源
        capture = NULL),
      textInput("primary_key_05", "Primary_Key", "Samples"),
      textInput("row_del_05", "删除行名(名称以英文逗号切分)", ""),
      textInput("col_del_05", "删除列名(名称以英文逗号切分)", ""),
      textInput("nominal_05", "名义变量(名称以英文逗号切分)", "Samples, Label"),
      numericInput("significance_05", "数值变量(有效数字个数)", 4, min = 1, max = 27),
      checkboxInput("scientific_05", "科学计数法", FALSE),
      fluidRow(
        column(
          width = 12,
          textInput("sample_05", "样本名称(列名)", "Samples")),
        column(
          width = 12,
          textInput("label_05", "样本标签(列名)", "Label")),
        column(
          width = 12,
          textInput('label_group_05', '组别标签(名称以英文逗号切分)', 'H, C')),
      ),
      fluidRow(
        column(width = 4,                                                 
               colourInput("col_high_05", "Color (High)", "firebrick")),
        column(width = 4,                                                 
               colourInput("col_medium_05", "Color (Medium)", NULL)),
        column(width = 4,
               colourInput("col_low_05", "Color (Low)", "blue"))
      ),
      sliderInput('col_num_05', '颜色等级', value = 80, min = 5, max = 105),
      width = 4
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Raw Data Table',
                 dataTableOutput('table_0501', width = "100%")
        ),
        tabPanel('Standard  Data Table',
                 shinycssloaders::withSpinner(dataTableOutput('table_0502', width = "100%")),
                 downloadButton("download_0501", class = "btn-block")
        ),
        tabPanel('HeatMap Plot',
                 shinycssloaders::withSpinner(plotOutput('plot_0503', width = "100%", height = "800px")),
                 downloadButton("download_0502", class = "btn-block")
        )
      )
    )
  )
)


########################################## 6. 箱线图模块 ###########################################
## 6.1、上传原始表达量数据
ui.page.06 <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # 加载数据文件
      fileInput(
        # 文件路径变量名称
        inputId = "file_06",
        # 选择框名称
        label = "选择数据文件 (仅支持xlsx、csv和txt文件)",
        # 接受的文件格式
        accept = c('.xlsx', '.csv', '.txt'),
        # 是否允许一次选择上传多个文件
        multiple = TRUE,
        # 输入框宽度
        width = NULL,
        # 按钮标签
        buttonLabel = "Upload...",
        # 占位文本
        placeholder = "No file selected",
        # 捕获图像、音频或视频数据的来源
        capture = NULL),
      fluidRow(
        column(
          width = 12,
          textInput("sample_06", "样本名称(列名)", "Samples")),
        column(
          width = 12,
          textInput("label_06", "样本标签(列名)", "Label")),
        column(
          width = 12,
          textInput('label_group_06', '组别标签(名称以英文逗号切分)', 'H, C')),
        column(
          width = 12,
          textInput('name_vars_06', '特征变量(名称以英文逗号切分)', 'Gene1, Gene2')),
        ),
      numericInput("significance_06", "数值变量(有效数字个数)", 4, min = 1, max = 27),
      checkboxInput("scientific_06", "科学计数法", FALSE),
      fluidRow(
        column(width = 6,                                                 
               numericInput("nrow_06", "行数(图形布局)", 1, min = 1)),
        column(width = 6,                                                 
               numericInput("ncol_06", "列数(图形布局)", 1, min = 1))
        ),
      fluidRow(
        column(width = 6,                                                 
               textInput("title_06", "标题名称", "")),
        column(width = 6,                                                 
               textInput("ytitle_06", "Y轴名称", ""))
        ),
      fluidRow(
        column(width = 6,                                                 
               selectInput("pvalue_06", "组间差异",
                           choices = c(FALSE, TRUE),
                           selected = FALSE)),
        column(width = 6,
               selectInput("method_06", "检验方法",
                           choices = c("t.test", "wilcox.test"),
                           selected = "t.test"))
        ),
      fluidRow(
        column(width = 6,                                                 
               sliderInput('title_size_06', 'Title size', value = 16,
                           min = 10, max = 20, step = 0.5)),
        column(width = 6,                                                 
               sliderInput('text_size_06', 'Text size', value = 4.5,
                           min = 1, max = 8, step = 0.5)),
      ),
      checkboxInput("samplename_06", "是否显示样本名称", FALSE),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Raw Data Table',
                 dataTableOutput('table_0601', width = "100%")
        ),
        tabPanel('Standard  Data Table',
                 shinycssloaders::withSpinner(dataTableOutput('table_0602', width = "100%"))
        ),
        tabPanel('Box Plot',
                 shinycssloaders::withSpinner(plotOutput('plot_0603', width = "100%", height = "800px")),
                 downloadButton("download_0601", class = "btn-block")
        )
      )
    )
  )
)

########################################## 7. PCA模块 #########################################
## 7.1、上传原始表达量数据
ui.page.07 <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # 加载数据文件
      fileInput(
        # 文件路径变量名称
        inputId = "file_07",
        # 选择框名称
        label = "选择数据文件 (仅支持xlsx、csv和txt文件)",
        # 接受的文件格式
        accept = c('.xlsx', '.csv', '.txt'),
        # 是否允许一次选择上传多个文件
        multiple = TRUE,
        # 输入框宽度
        width = NULL,
        # 按钮标签
        buttonLabel = "Upload...",
        # 占位文本
        placeholder = "No file selected",
        # 捕获图像、音频或视频数据的来源
        capture = NULL),
      textInput("primary_key_07", "Primary_Key", "Samples"),
      textInput("row_del_07", "删除行名(名称以英文逗号切分)", ""),
      textInput("col_del_07", "删除列名(名称以英文逗号切分)", ""),
      textInput("nominal_07", "名义变量(名称以英文逗号切分)", "Samples, Label"),
      numericInput("significance_07", "数值变量(有效数字个数)", 4, min = 1, max = 27),
      checkboxInput("scientific_07", "科学计数法", FALSE),
      fluidRow(
        column(
          width = 12,
          textInput("sample_07", "样本名称(列名)", "Samples")),
        column(
          width = 12,
          textInput("label_07", "样本标签(列名)", "Label")),
        column(
          width = 12,
          textInput('label_group_07', '组别标签(名称以英文逗号切分)', 'H, C')),
      ),
      textInput("title_07", "标题名称", ""),
      fluidRow(
        column(width = 6,                                                 
               sliderInput('title_size_07', 'Title size', value = 26,
                           min = 20, max = 30, step = 0.5)),
        column(width = 6,                                                 
               sliderInput('text_size_07', 'Text size', value = 4.5,
                           min = 1, max = 8, step = 0.5)),
      ),
      checkboxInput("samplename_07", "是否显示样本名称", FALSE),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Raw Data Table',
                 dataTableOutput('table_0701', width = "100%")
        ),
        tabPanel('Standard  Data Table',
                 shinycssloaders::withSpinner(dataTableOutput('table_0702', width = "100%")),
                 downloadButton("download_0701", class = "btn-block")
        ),
        tabPanel('PCA Plot',
                 shinycssloaders::withSpinner(plotOutput('plot_0703', width = "100%", height = "800px")),
                 downloadButton("download_0702", class = "btn-block")
        )
      )
    )
  )
)
########################################## 8. 数据建模模块 ######################################### 
## 8.1


############################################# UI文件整合 ########################################### 
ui <- dashboardPage(
  dashboardHeader(title = "数据分析网站"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("使用说明", tabName = "Introduce", icon = icon("dashboard")),
      menuItem("数据处理", tabName = "Processing", icon = icon("dashboard")),
      menuItem("差异分析", tabName = "Hypothesis",icon = icon("th"), badgeColor = "green"),
      menuItem("火山图", tabName = "Volcano", icon = icon("volcano")),
      menuItem("热图", tabName = "Heatmap", icon = icon("volcano")),
      menuItem("箱线图", tabName = "Box", icon = icon("volcano")),
      menuItem("PCA图", tabName = "Dim", icon = icon("volcano")),
      menuItem("数据建模", tabName = "Model", icon = icon("volcano"))
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Introduce", verbatimTextOutput("Introduce")),
      tabItem(tabName = "Processing", ui.page.02),
      tabItem(tabName = "Hypothesis", ui.page.03),
      tabItem(tabName = "Volcano", ui.page.04),
      tabItem(tabName = "Heatmap", ui.page.05),
      tabItem(tabName = "Box", ui.page.06),
      tabItem(tabName = "Dim", ui.page.07),
      tabItem(tabName = "Model", plotOutput("Plot_04"))
      )
  )
)


########################################### server 文件模块 ###########################################
server <- function(input, output, session) {
  options(shiny.maxRequestSize=60*1024^2)
  ############################################## 全局函数 #############################################
  ## 去掉字符串前后空格
  Trimws_func = function(str){return(trimws(str, which = "both", whitespace = "[ \t\r\n]"))}
  ########################################## 2. 数据处理模块 ##########################################
  # Upload ---------------------------------------------------------------
  raw_02 <- reactive({
    # 如果遇到认为“缺失”或“错误”的参数, 它将停止。
    req(input$file_02)
    lst <- list()
    for(i in 1:length(input$file_02[, 1])){
      # 提取文件后缀名
      suffix <- tools::file_ext(input$file_02[[i, 'datapath']])
      # 根据文件后缀名选择不同方式读取文件
      if(suffix == 'xlsx'){
        lst[[i]] <- data.frame(read.xlsx(input$file_02[[i, 'datapath']]), check.names = FALSE)
      } else if(suffix == 'csv'){
        lst[[i]] <- data.frame(read.csv(input$file_02[[i, 'datapath']]), check.names = FALSE)
      } else{
        lst[[i]] <- data.frame(read.table(input$file_02[[i, 'datapath']]), check.names = FALSE)
      }
    }
    # 合并上传的所有文件
    Reduce(rbind,lst)
  })
  output$table_0201 <- renderDataTable(raw_02(), rownames = FALSE, options = list(scrollX = TRUE))
  
  # Clean ----------------------------------------------------------------
  tidied_02 <- reactive({
    out <- data.frame(raw_02(), check.names = FALSE)
    # 数据去重
    out <- unique(out)
    # 需要删除的行
    row_del <- sapply(strsplit(input$row_del_02, ',')[[1]], Trimws_func)
    # 需要删除的列
    col_del <- sapply(strsplit(input$col_del_02, ',')[[1]], Trimws_func)
    # 删除行列
    out <- out[!out[,input$primary_key_02] %in% row_del, !colnames(out) %in% col_del]
    # 切分非数值类型变量名
    nominal <- sapply(strsplit(input$nominal_02, ',')[[1]], Trimws_func)
    # 是否需要转置
    if(input$transpose_02){
      row_out <- out[, input$primary_key_02]
      out <- out[, !colnames(out) %in% input$primary_key_02]
      Samples <- colnames(out)
      out <- data.frame(t(out), check.names = FALSE)
      colnames(out) <- row_out
      out <- cbind(Samples, out)
    } else{
      names(out)[names(out) == input$primary_key_02] = 'Samples'
    }
    # 增加名义变量Samples
    nominal <- c(nominal, 'Samples')
    # 修改变量类型
    out[, !colnames(out) %in% nominal] <- data.frame(apply(out[, !colnames(out) %in% nominal], c(1,2), as.numeric), check.names = FALSE)
    # 删除缺失比例超过missingscales的变量
    out <- out[which(rowMeans(!is.na(out)) > 1-input$missingscale_x_02/100), which(colMeans(!is.na(out)) > 1-input$missingscale_y_02/100)]
    # 数据变换
    if(input$scalemethod_02 == 'Min-Max'){
      Min_Max_Scale = function(vec){
        return((max(vec, na.rm = TRUE)-vec)/(max(vec, na.rm = TRUE)-min(vec, na.rm = TRUE)))
      }
      out[, !colnames(out) %in% nominal] <- sapply(out[, !colnames(out) %in% nominal], Min_Max_Scale)
    } else if(input$scalemethod_02 == 'Z-Score'){
      Z_Score_Scale = function(vec){
        return((vec-mean(vec, na.rm = TRUE))/sd(vec, na.rm = TRUE))
      }
      out[, !colnames(out) %in% nominal] <- sapply(out[, !colnames(out) %in% nominal], Z_Score_Scale)
      
    } else if(input$scalemethod_02 == 'Robust'){
      Robust_Scale = function(vec){
        return((vec-median(vec, na.rm = TRUE))/as.numeric(quantile(vec, 0.75, na.rm = TRUE)-quantile(vec, 0.25, na.rm = TRUE)))
      }
      out[, !colnames(out) %in% nominal] <- sapply(out[, !colnames(out) %in% nominal], Robust_Scale)
    } else if(input$scalemethod_02 == 'Log'){
      out[, !colnames(out) %in% nominal] = log(out[, !colnames(out) %in% nominal], input$k_log_02)
    } else if(input$scalemethod_02 == 'LogScale'){
      LogScale_Scale = function(vec){
        return(log(vec,input$k_log_02)/log(max(vec, na.rm = TRUE),input$k_log_02))
      }
      out[, !colnames(out) %in% nominal] <- sapply(out[, !colnames(out) %in% nominal], LogScale_Scale)
    } else{
      out <- out
    }
    # 缺失值处理
    if(input$imputation_02 == 'Delete'){
      out <- na.omit(out)
    } else if(input$imputation_02 == 'Min'){
      Min_imputation = function(vec){
        vec = as.numeric(vec)
        vec[is.na(vec)] = min(vec, na.rm = TRUE)
        return(vec)
      }
      out[, !colnames(out) %in% nominal] <- sapply(out[, !colnames(out) %in% nominal], Min_imputation)
    } else if(input$imputation_02 == 'Mean'){
      Mean_imputation = function(vec){
        vec = as.numeric(vec)
        vec[is.na(vec)] = mean(vec, na.rm = TRUE)
        return(vec)
      }
      out[, !colnames(out) %in% nominal] <- sapply(out[, !colnames(out) %in% nominal], Mean_imputation)
    } else if(input$imputation_02 == 'RandomForest'){
      set.seed(1234)
      out[, !colnames(out) %in% nominal] <- missForest(out[, !colnames(out) %in% nominal], ntree = input$k_ntree_02)$ximp
    } else if(input$imputation_02 == 'KNN'){
      set.seed(1234)
      out[, !colnames(out) %in% nominal] <- DMwR2::knnImputation(out[, !colnames(out) %in% nominal], k = input$k_ntree_02)
    } else{
      out <- out
    }
    
    # 修改数值变量有效数字及表现形式
    formatfuc <- function(num){if(is.na(num)) NA else format(signif(num, input$significance_02), scientific = input$scientific_02)}
    out[, !colnames(out) %in% nominal] <- data.frame(apply(out[, !colnames(out) %in% nominal], c(1,2), formatfuc), check.names = FALSE)
    
    out
  })
  output$table_0202 <- renderDataTable(tidied_02(), rownames = FALSE, options = list(scrollX = TRUE))
  
  # Download -------------------------------------------------------------
  output$download_0201 <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file_02$name), "(Processed_Data).xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(tidied_02(), file)
    }
  )
  
  ########################################## 3. 假设检验模块 ##########################################
  # Upload ---------------------------------------------------------------
  raw_03 <- reactive({
    # 如果遇到认为“缺失”或“错误”的参数, 它将停止。
    req(input$file_03)
    lst <- list()
    for(i in 1:length(input$file_03[, 1])){
      # 提取文件后缀名
      suffix <- tools::file_ext(input$file_03[[i, 'datapath']])
      # 根据文件后缀名选择不同方式读取文件
      if(suffix == 'xlsx'){
        lst[[i]] <- data.frame(read.xlsx(input$file_03[[i, 'datapath']]), check.names = FALSE)
      } else if(suffix == 'csv'){
        lst[[i]] <- data.frame(read.csv(input$file_03[[i, 'datapath']]), check.names = FALSE)
      } else{
        lst[[i]] <- data.frame(read.table(input$file_03[[i, 'datapath']]), check.names = FALSE)
      }
    }
    # 合并上传的所有文件
    Reduce(rbind,lst)
  })
  output$table_0301 <- renderDataTable(raw_03(), rownames = FALSE, options = list(scrollX = TRUE))
  
  # Clean ----------------------------------------------------------------
  tidied_03 <- reactive({
    out <- data.frame(raw_03(), check.names = FALSE)
    # 数据去重
    out <- unique(out)
    # 需要删除的行
    row_del <- sapply(strsplit(input$row_del_03, ',')[[1]], Trimws_func)
    # 需要删除的列
    col_del <- sapply(strsplit(input$col_del_03, ',')[[1]], Trimws_func)
    # 删除行列
    out <- out[!out[,input$primary_key_03] %in% row_del, !colnames(out) %in% col_del]
    # 切分非数值类型变量名
    nominal <- sapply(strsplit(input$nominal_03, ',')[[1]], Trimws_func)
    # 是否需要转置
    if(input$transpose_03){
      row_out <- out[, input$primary_key_03]
      out <- out[, !colnames(out) %in% input$primary_key_03]
      Samples <- colnames(out)
      out <- data.frame(t(out), check.names = FALSE)
      colnames(out) <- row_out
      out <- cbind(Samples, out)
    } else{
      names(out)[names(out) == input$primary_key_03] = 'Samples'
    }
    # 增加名义变量 Samples
    nominal <- c(nominal, 'Samples')
    # 修改变量类型
    out[, !colnames(out) %in% nominal] <- data.frame(apply(out[, !colnames(out) %in% nominal], c(1,2), as.numeric), check.names = FALSE)
    # 删除缺失比例超过missingscales的变量
    out <- out[which(rowMeans(!is.na(out)) > 1-input$missingscale_x_03/100), which(colMeans(!is.na(out)) > 1-input$missingscale_y_03/100)]
    # 修改数值变量有效数字及表现形式
    formatfuc <- function(num){if(is.na(num)) NA else format(signif(num, input$significance_03), scientific = input$scientific_03)}
    out[, !colnames(out) %in% nominal] <- data.frame(apply(out[, !colnames(out) %in% nominal], c(1,2), formatfuc), check.names = FALSE)
    out
  })
  output$table_0302 <- renderDataTable(tidied_03(), rownames = FALSE, options = list(scrollX = TRUE))
  
  # Hypothesis -----------------------------------------------------------
  hypothesis_03 <- reactive({
    # 加载标准格式的假设检验数据
    data_testing <- data.frame(tidied_03(), check.names = FALSE)
    # 修改样本标签列名称为Label
    colnames(data_testing)[colnames(data_testing) == input$label_03] <- 'Label'
    # 获取所有标签名称
    name_labels <- levels(factor(data_testing[, 'Label']))
    # 切分非数值类型变量名
    nominal <- sapply(strsplit(input$nominal_03, ',')[[1]], Trimws_func)
    nominal <- c(nominal, 'Samples')
    # 获取蛋白/代谢名称
    Samples <- colnames(data_testing)[!colnames(data_testing) %in% nominal]
    # 修改数据类型
    data_testing[, colnames(data_testing) %in% Samples] <- data.frame(apply(data_testing[, colnames(data_testing) %in% Samples], c(1,2), as.numeric), check.names = FALSE)
    # 创建空的数据框存储各标签的均值和变异系数
    mean_list = data.frame(matrix(ncol = (length(name_labels)+1), nrow = length(Samples)), check.names = FALSE)
    cv_list = data.frame(matrix(ncol = (length(name_labels)+1), nrow = length(Samples)), check.names = FALSE)
    # 添加mean_list 和 cv.list 的列名
    colnames(mean_list) = c('Samples', paste0("Mean_",name_labels))
    colnames(cv_list) = c('Samples', paste0("CV_",name_labels))
    mean_list[, 1] = Samples
    cv_list[, 1] = Samples
    for(j in c(1:length(name_labels))){
      for(i in c(1:length(Samples))){
        mean_list[i,j+1] = mean(data_testing[data_testing$Label == name_labels[j], Samples[i]], na.rm = TRUE)
        cv_list[i,j+1] = cv(data_testing[data_testing$Label == name_labels[j], Samples[i]], na.rm = TRUE)
      }
    }
    # 切分Control组和Case组标签
    label_control <- sapply(strsplit(input$control_label_03, ',')[[1]], Trimws_func)
    label_case <- sapply(strsplit(input$case_label_03, ',')[[1]], Trimws_func)
    # 修改样本标签
    data_testing[, 'Label'] <- factor(ifelse(data_testing[, 'Label'] %in% label_control, 0,
                                             ifelse(data_testing[, 'Label'] %in% label_case, 1, 2)),
                                      levels = c(0,1,2))
    # 只分析两组变量
    data_testing <- data_testing[data_testing[, "Label"] %in% c(0,1), ]
    # 设置因子变量
    data_testing[, "Label"] <- factor(data_testing[, "Label"], levels = c(0,1))
    # 计算Ratio
    # 正态分布检验(Shapiro-Wilk's Test) 和 方差齐性检验(Levene's Test)
    # 参数检验(T 检验)和非参数检验(U 检验)
    # 创建空的数据框存储各种检验结果
    test.result = data.frame(matrix(ncol = 7), check.names = FALSE)
    colnames(test.result) = c("Samples", "Ratio", "ShapiroWilk_Test", "Levene_Test", "T_Test", "U_Test", "pValue")
    for(k in c(1:length(Samples))){
      test.result[k, 1] = Samples[k]
      test.result[k, 2] = mean(data_testing[data_testing[,"Label"] == 1, Samples[k]], na.rm = TRUE) / mean(data_testing[data_testing[,"Label"] == 0, Samples[k]], na.rm = TRUE)
      if((sum(!is.na(data_testing[data_testing[, 'Label']==0, Samples[k]])) > 3 & sum(!is.na(data_testing[data_testing[, 'Label']==1, Samples[k]])) > 3)){
        test.result[k, 3] = shapiro.test(data_testing[, Samples[k]])$p.value
        test.result[k, 4] = leveneTest(data_testing[, Samples[k]] ~ data_testing[,"Label"])$`Pr(>F)`[1]
        test.result[k, 5] = t.test(data_testing[, Samples[k]] ~ data_testing[,"Label"], var.equal = TRUE)$p.value
        test.result[k, 6] = wilcox.test(data_testing[, Samples[k]] ~ data_testing[,"Label"], exact = FALSE)$p.value
      }else{
        test.result[k, 3] = NA
        test.result[k, 4] = NA
        test.result[k, 5] = NA
        test.result[k, 6] = NA
      }
      # 根据正态分布检验和方差齐性检验确定pValue
      if(is.na(test.result[k, 3]) & is.na(test.result[k, 4])){
        test.result[k, 7] = NA
      } else if(test.result[k, 3] >= 0.05 & test.result[k, 4] >= 0.05){
        test.result[k, 7] = test.result[k, 5]
      }else{
        test.result[k, 7] = test.result[k, 6]
      }
    }
    # 创建数据框存储所有结果
    test.result <- cbind(mean_list, cv_list[,-1], test.result[,-1])
    # 修改数值变量有效数字及表现形式
    formatfuc <- function(num){if(is.na(num)) NA else format(signif(num, input$significance_03), scientific = input$scientific_03)}
    test.result[, -1] <- data.frame(apply(test.result[, -1], c(1,2), formatfuc), check.names = FALSE)
    test.result
   
  })
  output$table_0303 <- renderDataTable(hypothesis_03(), rownames = FALSE, options = list(scrollX = TRUE))
  
  # Download -------------------------------------------------------------
  output$download_0301 <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file_03$name), "(Testing_Result).xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(hypothesis_03(), file)
    }
  )
  
  ########################################## 4. 火山图模块 ##########################################
  # Upload ---------------------------------------------------------------
  raw_04 <- reactive({
    # 如果遇到认为“缺失”或“错误”的参数, 它将停止。
    req(input$file_04)
    lst <- list()
    for(i in 1:length(input$file_04[, 1])){
      # 提取文件后缀名
      suffix <- tools::file_ext(input$file_04[[i, 'datapath']])
      # 根据文件后缀名选择不同方式读取文件
      if(suffix == 'xlsx'){
        lst[[i]] <- data.frame(read.xlsx(input$file_04[[i, 'datapath']]), check.names = FALSE)
      } else if(suffix == 'csv'){
        lst[[i]] <- data.frame(read.csv(input$file_04[[i, 'datapath']]), check.names = FALSE)
      } else{
        lst[[i]] <- data.frame(read.table(input$file_04[[i, 'datapath']]), check.names = FALSE)
      }
    }
    # 合并上传的所有文件
    Reduce(rbind,lst)
  })
  output$table_0401 <- renderDataTable(raw_04(), rownames = FALSE, options = list(scrollX = TRUE))
  
  # Clean ----------------------------------------------------------------
  tidied_04 <- reactive({
    out <- data.frame(raw_04(), check.names = FALSE)
    # 数据去重
    out <- unique(out)
    # 只提取需要的列
    cols_name <- c(input$col_gene_04, input$col_fc_04, input$col_pval_04)
    out <- out[, colnames(out) %in% cols_name]
    # 修改列名
    colnames(out) <- c('Samples', 'FoldChange', 'Pvalue')
    # 修改数据类型
    out[, c('FoldChange','Pvalue')] <- data.frame(apply(out[, c('FoldChange','Pvalue')], c(1,2), as.numeric), check.names = FALSE)
    # 修正P值
    out[, 'Padj'] <- p.adjust(out[, 'Pvalue'], method = input$pval_method_04)
    # 计算Log2(FoldChange)、Log10(Pvalue)、Log10(Padj)
    out[, 'LogFC'] <- log2(out[, 'FoldChange'])
    out[, 'LogPval'] <- -log10(out[, 'Pvalue'])
    out[, 'LogPadj'] <- -log10(out[, 'Padj'])
    nums_name <- c('FoldChange', 'Pvalue', 'Padj', 'LogFC', 'LogPval', 'LogPadj')
    # 修改数值变量有效数字及表现形式
    formatfuc <- function(num){if(is.na(num)) NA else format(signif(num, input$significance_04))}
    out[, colnames(out) %in% nums_name] <- data.frame(apply(out[, colnames(out) %in% nums_name], c(1,2), formatfuc), check.names = FALSE)
    out[, colnames(out) %in% nums_name] <- data.frame(apply(out[, colnames(out) %in% nums_name], c(1,2), as.numeric), check.names = FALSE)
    # 设置上调下调蛋白
    out$Group <- ifelse(out$FoldChange >= input$fc_threshold_04 & out$Padj < input$pval_threshold_04, 'Up',
                        ifelse(out$FoldChange <= (1/input$fc_threshold_04) & out$Padj < input$pval_threshold_04, "Down", "Unchanged"))
    out
  })
  output$table_0402 <- renderDataTable(tidied_04(), rownames = FALSE, options = list(scrollX = TRUE))
  
  volcanoplot_04 <- reactive({
    # 加载火山图数据
    data_volcano <- data.frame(tidied_04(), check.names = FALSE)
    # 修改数据类型
    nums_name <- c('FoldChange', 'Pvalue', 'Padj', 'LogFC', 'LogPval', 'LogPadj')
    data_volcano[, colnames(data_volcano) %in% nums_name] <- data.frame(apply(data_volcano[, colnames(data_volcano) %in% nums_name], c(1,2), as.numeric), check.names = FALSE)
    # 设计标记蛋白
    markgene <- as.character(sapply(strsplit(input$mark_gene_04, ',')[[1]], Trimws_func))
    if(length(markgene)){
      markdata <- data_volcano[data_volcano$Samples %in% markgene,]
    } else{
      markdata <- data_volcano[data_volcano$Group %in% c("Up", "Down"),]
    }
    # 设置图例颜色
    cols <- c("Up" = input$col_up_04, "Unchanged" = input$col_unchanged_04, "Down" = input$col_down_04)
    volcano_plot <- ggplot(data_volcano, aes(x=LogFC, y=LogPadj, colour = Group)) + 
      # 设置一般散点大小，形状
      geom_point(data = data_volcano, aes(colour = Group), shape=1, size=input$point_size_04) + 
      # 设置标记蛋白散点的大小，形状和颜色
      geom_point(data = markdata, aes(colour = Group), 
                 shape=16, size=input$point_size_04, alpha=input$alpha_04, show.legend=FALSE) +                                                                                                                                                                                                                                                                                                             
      # 设置颜色映射
      scale_color_manual(values = cols) +
      # 添加标记蛋白文本名称、字体大小和颜色
      geom_text_repel(data = markdata, aes(label = Samples), 
                      size=input$text_size_04, color = "black", show.legend=FALSE) +
      # 辅助线设置（hline 表示 平行于x轴，vline表示平行于y轴）
      geom_hline(yintercept= -log10(input$pval_threshold_04), linetype="longdash") + 
      geom_vline(xintercept= c(-log2(input$fc_threshold_04),log2(input$fc_threshold_04)), linetype="longdash") +
      theme_bw() +
      # 标签设置
      labs(
        title = input$title_04,
        y = ifelse(input$pval_method_04=="none", expression("-Log"[10]* " P Value"), expression("-Log"[10]* " P.adj Value")),
        x = expression("Log"[2]*" Fold Change"), 
      ) + 
      # 坐标范围设置
      xlim(input$xlim_min_04, input$xlim_max_04) + 
      theme(
        # 文本字体设置（"sans" 表示 "TT Arial", "serif" 表示 "TT Times New Roman"）
        text = element_text(family = "sans"),
        plot.title = element_text(hjust = 0.5, vjust = 2.5, size = 30),
        legend.text = element_text(size = 20),
        # 图例位置（横纵坐标）
        # legend.position = c(0.82, 0.85), 
        legend.position = 'top',
        # 图例名称，设置不显示
        # legend.title = element_text(size=22, colour = "black"),
        legend.title = element_blank(),
        # 坐标轴标签（刻度）
        axis.text = element_text(size=22, colour = "black"),
        # 坐标轴名称
        axis.title.x = element_text(size=28, vjust = -0.5),
        axis.title.y = element_text(size=28, vjust = 1.5),
        # 边框间距(上、右、下、左)
        plot.margin = unit(c(1,5,1,1),'lines'),
        # 背景设置
        # 网格设置（去除网格）
        panel.grid.major=element_line(colour=NA),
        # 背景设置（透明色）
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank()
      )
    volcano_plot
  })
  output$plot_0403 <- renderPlot(volcanoplot_04())
  
  # Download -------------------------------------------------------------
  output$download_0401 <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file_04$name), "(volcano_data).xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(tidied_04(), file)
    }
  )
  output$download_0402 <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file_04$name), "(volcano_plot).pdf")
    },
    content = function(file) {
      ggsave(file, volcanoplot_04(), width = 8, height = 8)
    }
  )
  
  ########################################## 5. 热图模块 ##########################################
  # Upload ---------------------------------------------------------------
  raw_05 <- reactive({
    # 如果遇到认为“缺失”或“错误”的参数, 它将停止。
    req(input$file_05)
    lst <- list()
    for(i in 1:length(input$file_05[, 1])){
      # 提取文件后缀名
      suffix <- tools::file_ext(input$file_05[[i, 'datapath']])
      # 根据文件后缀名选择不同方式读取文件
      if(suffix == 'xlsx'){
        lst[[i]] <- data.frame(read.xlsx(input$file_05[[i, 'datapath']]), check.names = FALSE)
      } else if(suffix == 'csv'){
        lst[[i]] <- data.frame(read.csv(input$file_05[[i, 'datapath']]), check.names = FALSE)
      } else{
        lst[[i]] <- data.frame(read.table(input$file_05[[i, 'datapath']]), check.names = FALSE)
      }
    }
    # 合并上传的所有文件
    Reduce(rbind,lst)
  })
  output$table_0501 <- renderDataTable(raw_05(), rownames = FALSE, options = list(scrollX = TRUE))
  
  # Clean ----------------------------------------------------------------
  tidied_05 <- reactive({
    out <- data.frame(raw_05(), check.names = FALSE)
    # 数据去重
    out <- unique(out)
    # 需要删除的行
    row_del <- sapply(strsplit(input$row_del_05, ',')[[1]], Trimws_func)
    # 需要删除的列
    col_del <- sapply(strsplit(input$col_del_05, ',')[[1]], Trimws_func)
    # 删除行列
    out <- out[!out[,input$primary_key_05] %in% row_del, !colnames(out) %in% col_del]
    # 切分非数值类型变量名
    nominal <- sapply(strsplit(input$nominal_05, ',')[[1]], Trimws_func)
    # 增加名义变量 Samples
    nominal <- c(nominal, 'Samples')
    # 修改变量类型
    out[, !colnames(out) %in% nominal] <- data.frame(apply(out[, !colnames(out) %in% nominal], c(1,2), as.numeric), check.names = FALSE)
    # 修改数值变量有效数字及表现形式
    formatfuc <- function(num){if(is.na(num)) NA else format(signif(num, input$significance_05), scientific = input$scientific_05)}
    out[, !colnames(out) %in% nominal] <- data.frame(apply(out[, !colnames(out) %in% nominal], c(1,2), formatfuc), check.names = FALSE)
    # 切分组别标签
    label_group <- as.character(sapply(strsplit(input$label_group_05, ',')[[1]], Trimws_func))
    # 选择只包含组别标签的数据
    out <- out[which(out[, input$label_05] %in% label_group, arr.ind = TRUE), ]
    # 将组别变量因子化
    out[, input$label_05] <- factor(out[, input$label_05], levels = label_group)
    # 按照因子顺序排序
    out <- out[order(out[, input$label_05]), ]
    out
    })
  
  output$table_0502 <- renderDataTable(tidied_05(), rownames = FALSE, options = list(scrollX = TRUE))
  
  heatmapplot_05 <- reactive({
    # 加载热图数据
    data_heatmap <- data.frame(tidied_05(), check.names = FALSE)
    # 设置组别
    annotation_col <- data.frame(Group = data_heatmap[, input$label_05])
    # 切分非数值类型变量名
    nominal <- sapply(strsplit(input$nominal_05, ',')[[1]], Trimws_func)
    # 增加名义变量 Samples
    nominal <- c(nominal, 'Samples')
    # 数值变量名称
    vars <- colnames(data_heatmap)[which(!colnames(data_heatmap) %in% nominal)]
    # 样本名称
    samples <- data_heatmap[, input$sample_05]
    # 只保留热图数据，并转置
    data_heatmap <- data.frame(t(data_heatmap[, which(!colnames(data_heatmap) %in% nominal)]), check.names = FALSE)
    data_heatmap <- data.frame(apply(data_heatmap, c(1,2), as.numeric), check.names = FALSE)
    # 修改行列名称
    colnames(data_heatmap) <- samples
    rownames(data_heatmap) <- vars
    # 绘制热图
    rownames(annotation_col) <- samples
    heatmap_plot <- pheatmap(data_heatmap, 
                               color = colorRampPalette(c(input$col_high_05, input$col_medium_05, input$col_low_05))(input$col_num_05),
                               scale="row",      # 表示进行均一化的方向
                               #kmeans_k = 2,    # 默认为NA，即不会对行进行聚类
                               cluster_cols = F, # 表示仅对样本（列）聚类
                               cluster_rows = F, # 表示仅对变量（行）聚类
                               border = "white",   #设置边框为白色
                               annotation_row = NA,  # 表示是否对列进行注释
                               annotation_col = annotation_col, # 表示是否对列进行注释(BE、AC分组)
                               show_rownames = TRUE,  # 表示是否显示变量名
                               show_colnames = FALSE, # 表示是否显示样本名称
                               fontsize = 11,
                               # main="HeatMap"     #设置图形标题
                               )
    heatmap_plot
    })
  output$plot_0503 <- renderPlot(heatmapplot_05())
  
  # Download -------------------------------------------------------------
  output$download_0501 <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file_05$name), "(heatmap_data).xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(tidied_05(), file)
    }
  )
  output$download_0502 <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file_05$name), "(heatmap_plot).pdf")
    },
    content = function(file) {
      ggsave(file, heatmapplot_05(), width = 12)
    }
  )
  
  ########################################## 6. 箱线图模块 ##########################################
  # Upload ---------------------------------------------------------------
  raw_06 <- reactive({
    # 如果遇到认为“缺失”或“错误”的参数, 它将停止。
    req(input$file_06)
    lst <- list()
    for(i in 1:length(input$file_06[, 1])){
      # 提取文件后缀名
      suffix <- tools::file_ext(input$file_06[[i, 'datapath']])
      # 根据文件后缀名选择不同方式读取文件
      if(suffix == 'xlsx'){
        lst[[i]] <- data.frame(read.xlsx(input$file_06[[i, 'datapath']]), check.names = FALSE)
      } else if(suffix == 'csv'){
        lst[[i]] <- data.frame(read.csv(input$file_06[[i, 'datapath']]), check.names = FALSE)
      } else{
        lst[[i]] <- data.frame(read.table(input$file_06[[i, 'datapath']]), check.names = FALSE)
      }
    }
    # 合并上传的所有文件
    Reduce(rbind,lst)
  })
  output$table_0601 <- renderDataTable(raw_06(), rownames = FALSE, options = list(scrollX = TRUE))
  
  # Clean ----------------------------------------------------------------
  tidied_06 <- reactive({
    out <- data.frame(raw_06(), check.names = FALSE)
    # 数据去重
    out <- unique(out)
    # 切分特征变量名称
    vars <- as.vector(sapply(strsplit(input$name_vars_06, ',')[[1]], Trimws_func))
    # 选择样本名称、样本标签和特征变量列
    cols <- c(input$sample_06, input$label_06, vars)
    out <- out[, colnames(out) %in% cols]
    # 修改数字类型
    out[, colnames(out) %in% vars] <- data.frame(apply(data.frame(out[, colnames(out) %in% vars]), c(1,2), as.numeric), check.names = FALSE)
    # 切分组别标签
    label_group <- as.character(sapply(strsplit(input$label_group_06, ',')[[1]], Trimws_func))
    # 选择只包含组别标签的数据
    out <- out[which(out[, input$label_06] %in% label_group, arr.ind = TRUE), ]
    # 将组别变量因子化
    out[, input$label_06] <- factor(out[, input$label_06], levels = label_group)
    # 按照因子顺序排序
    out <- out[order(out[, input$label_06]), ]
    # 修改数值变量有效数字及表现形式
    formatfuc <- function(num){if(is.na(num)) NA else format(signif(num, input$significance_06), scientific = input$scientific_06)}
    out[, colnames(out) %in% vars] <- data.frame(apply(data.frame(out[, colnames(out) %in% vars]), c(1,2), formatfuc), check.names = FALSE)
    
    out
    
  })
  output$table_0602 <- renderDataTable(tidied_06(), rownames = FALSE, options = list(scrollX = TRUE))
  
  boxplot_06 <- reactive({
    # 加载箱线图数据
    data_box <- data.frame(tidied_06(), check.names = FALSE)
    # 修改变量类型
    # 切分特征变量名称
    vars <- as.vector(sapply(strsplit(input$name_vars_06, ',')[[1]], Trimws_func))
    # 选择样本名称、样本标签和特征变量列
    cols <- c(input$sample_06, input$label_06, vars)
    data_box <- data_box[, colnames(data_box) %in% cols]
    # 修改数字类型
    data_box[, colnames(data_box) %in% vars] <- data.frame(apply(data.frame(data_box[, colnames(data_box) %in% vars]), c(1,2), as.numeric), check.names = FALSE)
    # 绘制箱线图
    box_plot <- function(item){
      ggboxplot(data_box, x = input$label_06, y = item,
                color = input$label_06, palette = "jco",add = "jitter",
                title = item) + 
        {
          if(input$samplename_06){
            geom_text_repel(aes(label=data_box[, input$sample_06]), size = input$text_size_06)
          }
        } + 
        
        # 坐标轴名称内容设置
        theme(
          plot.title = element_text(hjust = 0.5, vjust = 5.0, size = input$title_size_06, colour = "black"),
          # 坐标轴标签大小
          axis.text.x = element_blank(),
          axis.text.y = element_text(size=20, colour = "black"),
          # axis.text = element_text(size=14, colour = "black"),
          # 坐标轴名称大小
          axis.title = element_blank(),  # 不显示坐标轴名称
          # 刻度设置
          axis.ticks.x = element_blank(),
          # 图例设置
          legend.position = "right",  # 不显示图例
          legend.title = element_blank(), # 不显示图例名称
          legend.text = element_text(size = 4+input$title_size_06),
          # 调整边距 （上、右、下、左）
          plot.margin=unit(c(2.0, 1.8, 1.5, 0.5),'lines')# ,
          # 边框设置
          # panel.border = element_rect(fill = NA, color = "black", size = 1.0)
        ) + 
        # t-test 结果
        if(input$pvalue_06){
          stat_compare_means(
            # 分组对比
            comparisons = split(combn(levels(data_box[, input$label_06]), 2), col(combn(levels(data_box[, input$label_06]), 2))), 
            # 检验方法 
            method = input$method_06,
            # 字体大小
            size = 1.5 + input$text_size_06
          )
        }
    }
    # 多张图合并
    boxs_plot <- lapply(vars, box_plot)
    boxs <- ggarrange(plotlist = boxs_plot[1:length(boxs_plot)], 
                      ncol = input$ncol_06, 
                      nrow = input$nrow_06, 
                      common.legend = TRUE,
                      legend = "top") + 
      theme(
        # 调整边距 （上、右、下、左）
        plot.margin=unit(c(1.0, 1.8, 1.0, 0.5),'lines'),
        
      )
    boxs <- annotate_figure(boxs, 
                            top = text_grob(input$title_06, size = 28),
                            left = text_grob(ifelse(input$ytitle_06=="", expression("log"[10]*" abundance"), input$ytitle_06), rot = 90, size = 26))
    boxs
  })
  output$plot_0603 <- renderPlot(boxplot_06())
  
  # Download -------------------------------------------------------------
  output$download_0601 <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file_06$name), "(box_plot).pdf")
    },
    content = function(file) {
      ggsave(file, boxplot_06(), width = 12, height = 6*input$nrow_06)
    }
  )
  
  ########################################## 7. PCA模块 ##########################################
  # Upload ---------------------------------------------------------------
  raw_07 <- reactive({
    # 如果遇到认为“缺失”或“错误”的参数, 它将停止。
    req(input$file_07)
    lst <- list()
    for(i in 1:length(input$file_07[, 1])){
      # 提取文件后缀名
      suffix <- tools::file_ext(input$file_07[[i, 'datapath']])
      # 根据文件后缀名选择不同方式读取文件
      if(suffix == 'xlsx'){
        lst[[i]] <- data.frame(read.xlsx(input$file_07[[i, 'datapath']]), check.names = FALSE)
      } else if(suffix == 'csv'){
        lst[[i]] <- data.frame(read.csv(input$file_07[[i, 'datapath']]), check.names = FALSE)
      } else{
        lst[[i]] <- data.frame(read.table(input$file_07[[i, 'datapath']]), check.names = FALSE)
      }
    }
    # 合并上传的所有文件
    Reduce(rbind,lst)
  })
  output$table_0701 <- renderDataTable(raw_07(), rownames = FALSE, options = list(scrollX = TRUE))
  
  # Clean ----------------------------------------------------------------
  tidied_07 <- reactive({
    out <- data.frame(raw_07(), check.names = FALSE)
    # 数据去重
    out <- unique(out)
    # 需要删除的行
    row_del <- sapply(strsplit(input$row_del_07, ',')[[1]], Trimws_func)
    # 需要删除的列
    col_del <- sapply(strsplit(input$col_del_07, ',')[[1]], Trimws_func)
    # 删除行列
    out <- out[!out[,input$primary_key_07] %in% row_del, !colnames(out) %in% col_del]
    # 切分非数值类型变量名
    nominal <- sapply(strsplit(input$nominal_07, ',')[[1]], Trimws_func)
    # 增加名义变量 Samples
    nominal <- c(nominal, 'Samples')
    # 修改变量类型
    out[, !colnames(out) %in% nominal] <- data.frame(apply(out[, !colnames(out) %in% nominal], c(1,2), as.numeric), check.names = FALSE)
    # 修改数值变量有效数字及表现形式
    formatfuc <- function(num){if(is.na(num)) NA else format(signif(num, input$significance_05), scientific = input$scientific_05)}
    out[, !colnames(out) %in% nominal] <- data.frame(apply(out[, !colnames(out) %in% nominal], c(1,2), formatfuc), check.names = FALSE)
    # 切分组别标签
    label_group <- as.character(sapply(strsplit(input$label_group_07, ',')[[1]], Trimws_func))
    # 选择只包含组别标签的数据
    out <- out[which(out[, input$label_07] %in% label_group, arr.ind = TRUE), ]
    # 将组别变量因子化
    out[, input$label_07] <- factor(out[, input$label_07], levels = label_group)
    # 按照因子顺序排序
    out <- out[order(out[, input$label_07]), ]
    out
  })
  
  output$table_0702 <- renderDataTable(tidied_07(), rownames = FALSE, options = list(scrollX = TRUE))
  
  pcaplot_07 <- reactive({
    # 加载热图数据
    data_pca <- data.frame(tidied_07(), check.names = FALSE)
    # 设置组别
    groups <- data_pca[, input$label_07]
    # 切分非数值类型变量名
    nominal <- sapply(strsplit(input$nominal_07, ',')[[1]], Trimws_func)
    # 增加名义变量 Samples
    nominal <- c(nominal, 'Samples')
    # 数值变量名称
    vars <- colnames(data_pca)[which(!colnames(data_pca) %in% nominal)]
    # 样本名称
    samples <- data_pca[, input$sample_07]
    # 只保留PCA数据
    data_pca <- data.frame(data_pca[, which(!colnames(data_pca) %in% nominal)], check.names = FALSE)
    data_pca <- data.frame(apply(data_pca, c(1,2), as.numeric), check.names = FALSE)
    # 修改行列名称
    colnames(data_pca) <- vars
    rownames(data_pca) <- samples
    # PCA处理（标准化数据）
    pca_result <- prcomp(data_pca, scale. = T, na.action = na.pass)
    # 查看主成分结果
    pca_scores <- as.data.frame(pca_result$x)
    # 切分组别标签
    label_group <- as.character(sapply(strsplit(input$label_group_07, ',')[[1]], Trimws_func))
    # 添加样本标签Label
    pca_scores$Label <- factor(groups, levels = label_group)
    # 坐标轴设置
    pc1 <- paste('PC1 (', round(summary(pca_result)[6]$importance[2,1]*100, 2), '%)')
    pc2 <- paste('PC2 (', round(summary(pca_result)[6]$importance[2,2]*100, 2), '%)')
    # 颜色设置
    color <- c('blue', 'firebrick', 'seagreen', 'pink3', 'skyblue3', 'seagreen3')
    # 绘制PCA
    pca_plot <- ggplot(data = pca_scores, aes(PC1, PC2, fill = Label, col = Label)) +
      geom_point(aes(color = Label, shape = Label), size = input$text_size_07) +  
      stat_ellipse(aes(fill =Label), geom = 'polygon', level = 0.95, alpha = 0.1, show.legend = FALSE) + 
      scale_color_manual(values =color[1:length(unique(pca_scores$Label))]) +
      scale_fill_manual(values = color[1:length(unique(pca_scores$Label))]) +
      {
        if(input$samplename_07){
          geom_text_repel(aes(label=samples), size = input$text_size_07)
        }
      } +  
      labs(x = pc1, y = pc2, title = input$title_07) +
      theme(
        # 文本字体设置（"sans" 表示 "TT Arial", "serif" 表示 "TT Times New Roman"）
        text = element_text(family = "sans"),
        plot.title = element_text(hjust = 0.5, vjust = 2.5, size = input$title_size_07),
        legend.text = element_text(size = input$title_size_07-2),
        # 图例位置（横纵坐标）
        legend.position = "top", 
        # 图例名称，设置不显示
        legend.title = element_blank(),
        # 坐标轴标签（刻度）
        axis.text = element_text(size = input$title_size_07-4, colour = "black"),
        # 坐标轴名称
        axis.title.x = element_text(size = input$title_size_07, vjust = -0.5),
        axis.title.y = element_text(size = input$title_size_07),
        # 边框间距(上、右、下、左)
        plot.margin = unit(c(1,5,1,1),'lines'),
        # 背景设置
        # 网格设置
        # panel.grid.major = element_line(color = 'gray', size = 0.2), 
        panel.background = element_rect(color = 'black', fill = 'transparent'),
      ) 
    pca_plot
  })
  output$plot_0703 <- renderPlot(pcaplot_07())
  
  # Download -------------------------------------------------------------
  output$download_0701 <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file_07$name), "(pca_data).xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(tidied_07(), file)
    }
  )
  output$download_0702 <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file_07$name), "(pca_plot).pdf")
    },
    content = function(file) {
      ggsave(file, pcaplot_07(), width = 8)
    }
  )
  
}




shinyApp(ui, server)

