library(ggplot2)
library(ggpubr)

files <- list.files(pattern = "*.csv", recursive = TRUE)

lollipop <- function(file,filename,title) {
  data = read.csv(file,header = TRUE)
  if(nrow(data) <= 1) return()
  data = sapply(data, table)
  #得到计算Description频数的结果
  data_Description = data$Description
  #将结果转换为data.frame
  data_Description = as.data.frame(data_Description)
  #计算行数
  rows <- nrow(data_Description)
  #按频数排序
  data_Description = data_Description[order(data_Description$Freq,data_Description$Var1,decreasing = T),]
  
  # 创建一个名为'top'的新列
  data_Description$Top <- 0
  
  # 循环遍历数据帧的每一行
  for (i in 1:nrow(data_Description)) {
    # 计算当前行的top值
    top_value <- 10 * ceiling(i / 10)
    
    # 将当前行的top值赋给新列
    data_Description[i, "Top"] <- top_value
  }
  
  # 如果剩余的行数不足十行，则将实际的行数赋给top值
  if (nrow(data_Description) < 10) {
    data_Description$Top[nrow(data_Description)] <- nrow(data_Description)
  } else
    if (nrow(data_Description) %% 10 == 0) {
    # 如果数据帧的行数为整十的倍数，则将最后十行的 "Top" 值设为实际的行数
    data_Description$Top[(nrow(data_Description) - 9):nrow(data_Description)] <- nrow(data_Description)
  } else {
    # 如果剩余的行数超过十行，则将最后一段小于十行的部分的top值设为实际的行数
    data_Description$Top[(nrow(data_Description) - (nrow(data_Description) %% 10) + 1):nrow(data_Description)] <- nrow(data_Description)
  }
  
  data_Description <- data_Description[nrow(data_Description):1, ]
  data_Description$Description = as.character(data_Description$Var1)
  data_Description$Freq = as.numeric(data_Description$Freq)
  data_Description$Top = as.factor(data_Description$Top)
  
  freq_values <- data_Description$Freq
  types <- length(unique(freq_values))
  colors <- heat.colors(types)
  
  lollipop_pic <- ggdotchart(data_Description, x="Description", y="Freq",
                         col = colors[data_Description$Freq],
                         sorting = "descending", add = "segments", rotate = TRUE,
                         dot.size = 7,title=title,
                         label = round(data_Description$Freq), font.label = list(color="black", size=9, vjust=0.5), ggtheme = theme_pubr())
  filename = paste(dirname(file),"/",filename,".pdf",sep = "") 
  ggsave(lollipop_pic, filename =filename,  width = 18, height = 10)
}

for (file in files) {
  file <- file
  filenames = strsplit(file, "[/_.]")
  # 检查列表中是否存在 "GO" 元素
  if (any("GO" %in% filenames[[1]])) {
    filename <- paste0(filenames[[1]][length(filenames[[1]]) - 6], "_", filenames[[1]][length(filenames[[1]]) - 4], "_", filenames[[1]][length(filenames[[1]]) - 3], "_", filenames[[1]][length(filenames[[1]]) - 2], "_", filenames[[1]][length(filenames[[1]]) - 1])
  } else {
    filename <- paste0(filenames[[1]][length(filenames[[1]]) - 5], "_", filenames[[1]][length(filenames[[1]]) - 3], "_", filenames[[1]][length(filenames[[1]]) - 2], "_", filenames[[1]][length(filenames[[1]]) - 1])
  }
  title = paste(filename,"pathway",sep = "_")
  lollipop(file,filename,title)
}
