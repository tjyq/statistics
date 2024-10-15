


##############进行数据分析以及数据保存#################
#这是进行数据的VPA分析的代码

library(vegan) # 加载包
library(readxl) # 加载读取Excel文件的包
library(writexl) # 加载写入Excel文件的包

# 读取数据
data <- read_excel("E:\\1ecohydrology\\SMobservation\\SMdata20142023\\Drain与ET耦合分析结果202408.xlsx", sheet = "Drain与ET综合关系")

# 提取响应变量和影响因素
response <- data$Coup_v1
soil <- data[, c('clay', 'sand', 'ks', 'bulk', 'soc', 'theta_s', 'theta_r')]
vegetation <- data[, 'NDVI', drop = FALSE]
meteorology <- data[, c('SM1', 'ST1', 'prec')]
topography <- data[, c('TWI', 'Slope', 'DEM', 'Aspect')]

# 进行VPA分析
vpa_result <- varpart(response, soil, vegetation, meteorology, topography)

# 绘制VPA结果图并保存
png("E:\\1ecohydrology\\SMobservation\\SMdata20142023\\VPA_plot.png", width = 800, height = 600)
plot(vpa_result, digits = 2, Xnames = c('Soil', 'Vegetation', 'Meteorology', 'Topography'), bg = c('blue', 'red', 'green', 'yellow'))

# 检查残差值是否为数值，如果是则进行四舍五入，否则显示"NA"
residuals_value <- vpa_result$part$indfract$Adj.R.squared[15]
residuals_text <- if(is.numeric(residuals_value)) {
  paste("Residuals =", round(residuals_value, 2))
} else {
  "Residuals = NA"
}
text(0.5, -0.1, cex = 0.8)

dev.off()

# 进行显著性检验并保存结果
significance_tests <- list(
  soil = anova.cca(rda(response, soil), permutations = 999),
  vegetation = anova.cca(rda(response, vegetation), permutations = 999),
  meteorology = anova.cca(rda(response, meteorology), permutations = 999),
  topography = anova.cca(rda(response, topography), permutations = 999),
  soil_vegetation = anova.cca(rda(response, cbind(soil, vegetation)), permutations = 999),
  soil_meteorology = anova.cca(rda(response, cbind(soil, meteorology)), permutations = 999),
  soil_topography = anova.cca(rda(response, cbind(soil, topography)), permutations = 999),
  vegetation_meteorology = anova.cca(rda(response, cbind(vegetation, meteorology)), permutations = 999),
  vegetation_topography = anova.cca(rda(response, cbind(vegetation, topography)), permutations = 999),
  meteorology_topography = anova.cca(rda(response, cbind(meteorology, topography)), permutations = 999)
)

# 直接从 vpa_result 中提取数据
vpa_data <- vpa_result$part$indfract

if (!is.null(vpa_data) && is.data.frame(vpa_data) && nrow(vpa_data) > 0) {
  cat("vpa_data 的结构：\n")
  print(str(vpa_data))
  
  cat("\nvpa_data 的内容：\n")
  print(vpa_data)
  
  if (!is.null(vpa_data$Adj.R.square)) {
    vpa_results <- data.frame(
      Component = rownames(vpa_data),
      Explained_Variance = vpa_data$Adj.R.square,
      stringsAsFactors = FALSE
    )
    
    cat("\n原始 vpa_results：\n")
    print(vpa_results)
    
    # 移除负值并重新计算百分比
    vpa_results$Explained_Variance[vpa_results$Explained_Variance < 0] <- 0
    total_variance <- sum(vpa_results$Explained_Variance)
    vpa_results$Percentage <- vpa_results$Explained_Variance / total_variance * 100
    
    cat("\n调整后的 vpa_results（移除负值并计算百分比）：\n")
    print(vpa_results)
    
    # 创建一个只包含正解释方差的数据框
    positive_results <- vpa_results[vpa_results$Explained_Variance > 0, ]
    cat("\n只包含正解释方差的结果：\n")
    print(positive_results)
  } else {
    cat("\n警告：vpa_data 中没有 Adj.R.square 列。\n")
  }
} else {
  cat("错误：无法从 vpa_result 中提取有效数据。请检查 varpart 函数的输出。\n")
  print(str(vpa_result))
}


significance_results <- do.call(rbind, lapply(names(significance_tests), function(name) {
  data.frame(
    Component = name,
    F_value = significance_tests[[name]]$F[1],
    P_value = significance_tests[[name]]$`Pr(>F)`[1]
  )
}))

# 将结果保存到Excel文件
output_path <- "E:\\1ecohydrology\\SMobservation\\SMdata20142023\\VPA_results.xlsx"
write_xlsx(list(VPA_Results = vpa_results, Significance_Tests = significance_results), output_path)

cat("VPA结果已保存到", output_path, "\n")
cat("VPA图已保存到 E:\\1ecohydrology\\SMobservation\\SMdata20142023\\VPA_plot.png\n")





#######################################################################
#这是进行循环分析多个解释变量


library(vegan) # 加载包
library(readxl) # 加载读取Excel文件的包
library(writexl) # 加载写入Excel文件的包

# 读取数据
data <- read_excel("E:\\1ecohydrology\\SMobservation\\SMdata20142023\\Drain与ET耦合分析结果202408.xlsx", sheet = "Drain与ET综合关系")

# 提取响应变量和影响因素
# response <- data$Coup_v1
soil <- data[, c('clay', 'sand', 'ks', 'bulk', 'soc', 'theta_s', 'theta_r')]
vegetation <- data[, 'NDVI', drop = FALSE]
meteorology <- data[, c('SM1', 'ST1', 'prec')]
topography <- data[, c('TWI', 'Slope', 'DEM', 'Aspect')]

# 定义所有因变量
response_variables <- c('Coup_v1', 'SR_D', 'PR_D', 'Drain', 'ET')

# 循环处理每个因变量
for (response_var in response_variables) {
  cat(paste("\n正在分析因变量:", response_var, "\n"))
  
  # 提取当前响应变量
  response <- data[[response_var]]
  
  # 进行VPA分析
  vpa_result <- varpart(response, soil, vegetation, meteorology, topography)
  
  # 绘制VPA结果图并保存
  png(paste0("E:\\1ecohydrology\\SMobservation\\SMdata20142023\\VPA_plot_", response_var, ".png"), width = 800, height = 600)
  plot(vpa_result, digits = 2, Xnames = c('Soil', 'Vegetation', 'Meteorology', 'Topography'), bg = c('blue', 'red', 'green', 'yellow'))
  
  # 检查残差值并添加到图中
  residuals_value <- vpa_result$part$indfract$Adj.R.squared[15]
  residuals_text <- if(is.numeric(residuals_value)) {
    paste("Residuals =", round(residuals_value, 2))
  } else {
    "Residuals = NA"
  }
  text(0.5, -0.1, residuals_text, cex = 0.8)
  
  dev.off()
  
  # 进行显著性检验
  significance_tests <- list(
    soil = anova.cca(rda(response, soil), permutations = 999),
    vegetation = anova.cca(rda(response, vegetation), permutations = 999),
    meteorology = anova.cca(rda(response, meteorology), permutations = 999),
    topography = anova.cca(rda(response, topography), permutations = 999),
    soil_vegetation = anova.cca(rda(response, cbind(soil, vegetation)), permutations = 999),
    soil_meteorology = anova.cca(rda(response, cbind(soil, meteorology)), permutations = 999),
    soil_topography = anova.cca(rda(response, cbind(soil, topography)), permutations = 999),
    vegetation_meteorology = anova.cca(rda(response, cbind(vegetation, meteorology)), permutations = 999),
    vegetation_topography = anova.cca(rda(response, cbind(vegetation, topography)), permutations = 999),
    meteorology_topography = anova.cca(rda(response, cbind(meteorology, topography)), permutations = 999)
  )
  
  # 处理VPA结果
  vpa_data <- vpa_result$part$indfract
  
  if (!is.null(vpa_data) && is.data.frame(vpa_data) && nrow(vpa_data) > 0) {
    if (!is.null(vpa_data$Adj.R.square)) {
      vpa_results <- data.frame(
        Component = rownames(vpa_data),
        Explained_Variance = vpa_data$Adj.R.square,
        stringsAsFactors = FALSE
      )
      
      # 移除负值并重新计算百分比
      vpa_results$Explained_Variance[vpa_results$Explained_Variance < 0] <- 0
      total_variance <- sum(vpa_results$Explained_Variance)
      vpa_results$Percentage <- vpa_results$Explained_Variance / total_variance * 100
      
      # 创建一个只包含正解释方差的数据框
      positive_results <- vpa_results[vpa_results$Explained_Variance > 0, ]
    } else {
      cat("\n警告：vpa_data 中没有 Adj.R.square 列。\n")
      next
    }
  } else {
    cat("错误：无法从 vpa_result 中提取有效数据。请检查 varpart 函数的输出。\n")
    print(str(vpa_result))
    next
  }
  
  # 处理显著性检验结果
  significance_results <- do.call(rbind, lapply(names(significance_tests), function(name) {
    data.frame(
      Component = name,
      F_value = significance_tests[[name]]$F[1],
      P_value = significance_tests[[name]]$`Pr(>F)`[1]
    )
  }))
  
  # 将结果保存到Excel文件
  output_path <- paste0("E:\\1ecohydrology\\SMobservation\\SMdata20142023\\VPA_results_", response_var, ".xlsx")
  write_xlsx(list(VPA_Results = vpa_results, Positive_Results = positive_results, Significance_Tests = significance_results), output_path)
  
  cat("VPA结果已保存到", output_path, "\n")
  cat("VPA图已保存到 E:\\1ecohydrology\\SMobservation\\SMdata20142023\\VPA_plot_", response_var, ".png\n")
}

# ... 现有代码 ...


