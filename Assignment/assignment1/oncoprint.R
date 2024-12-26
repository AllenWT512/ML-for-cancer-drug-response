library(openxlsx)
library(ComplexHeatmap)
library(circlize)
setwd("C:\\Users\\Administrator\\Melbourne University Assignment jupyter notebook\\assignment1")
tubian30<-read.csv("TopMutationGene30.csv", row.names = 1)
tubian30[tubian30 == 1] <- "mutation"##数据框1替换成mutution
tubian30[tubian30 == 0] <- "  "##0替换成空白
col = c("mutation" = "#8999ca")##颜色
##设置格子大小宽度
alter_fun = function(x, y, w, h, v) {
  n=sum(v)
  h=0.9*h
  grid.rect(x, y-unit(0.5, "mm"), w-unit(0.5, "mm"), h, gp = gpar(fill = "#CCCCCC", col = NA))
  if(v["mutation"])  grid.rect(x, y - h*0.5 + 0.95:n/n*h, w*0.95, 1/n*h,  gp = gpar(fill = col[names(which(v))], col = NA), just = "top")
  
}
#标题
column_title = "OncoPrint "
#设置标签等
heatmap_legend_param = list(title = "Alternations", at = c("mutation"), 
                            labels = c("mutation"))

##删除空行和列绘制瀑布图
oncoPrint(tubian30,
          alter_fun = alter_fun, col = col, 
          remove_empty_columns = TRUE, remove_empty_rows = TRUE,
          column_title = column_title, heatmap_legend_param = heatmap_legend_param)
