#' 批量读取csv文件方法
#'
#' @param col.names T: vector<charactor> 列名
#' @param data T: data.frame 数据框
#' @param probs T: vector<double> 分位数向量
#'
#' @returns T: factor 返回一个因子，表示每个数据点所属的分位数区间
#' @export
quantile_by <- function(colname, data, probs) {
  num <- c(1:(length(probs) - 1))
  labels <- num %>% map(\(x) paste("Q", x, sep = ""))
  temp <- quantile(data.final[[colname]], probs) %>%
    cut(data.final[[colname]], breaks = ., labels = labels)
  return(temp)
}
