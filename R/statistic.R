#' 计算均值的置信区间，并判断 0 是否在区间内
#'
#' @param data T: data.frame 必须包含 `estimate` 和 `std.error` 两列
#' @param confidence.level T: num 置信水平，默认 0.95
#'
#' @returns T: data.frame 包含 upper, lower, t.statistic, p.value
#' @export
#'
#' @examples
#' df <- data.frame(estimate = c(1.2, -0.5, 0.8), std.error = c(0.3, 0.2, 0.4))
#' stat.confidence.interval(df, 0.95)
stat.confidence.interval <- function(data, confidence.level = 0.95) {
  if (!all(c("estimate", "std.error") %in% colnames(data))) {
    stop("数据框必须包含 'estimate' 和 'std.error' 列")
  }

  alpha <- 1 - confidence.level # 计算显著性水平
  t_critical <- qt(1 - alpha / 2, df = Inf) # 计算标准正态分布的 t 值（近似 z 值）

  # 计算置信区间
  data$upper <- data$estimate + t_critical * data$std.error
  data$lower <- data$estimate - t_critical * data$std.error

  # 计算 t 统计量和 p 值
  data$t.statistic <- data$estimate / data$std.error
  data$p.value <- 2 * (1 - pnorm(abs(data$t.statistic))) # 计算双侧 p 值

  return(data)
}
