#' 数据拆分为X和Y
#'
#' @param set T: vector<double> ~> 输入的数据
#' @param look_back T: num ~> 回溯的数据步长：如果在时间序列中，当前值的预测值与前n个数有关，那么回溯的数据步长为n
#' @description
#' 通过一个向量生成一个训练窗口集合以及一个验证集
#'
#' @return T: list<array> ~>返回一个list，其中包含训练窗口数组（windows）和验证数据数组（confirms）
#' @export
lstm.create.dataset <- function(set, look_back) {
  # set表示数据集，look_back表示回溯多少步
  l <- length(set)

  dataX <- array(dim = c(l - look_back, look_back))
  for (i in 1:look_back) {
    dataX[, i] <- set[i:(l - look_back + i - 1)]
  }

  dataY <- array(
    data = set[(look_back + 1):l],
    dim = c(l - look_back, 1)
  ) # label只有1个

  return(list(windows = dataX, confirms = dataY))
}


#' 生成比例数据
#' @description
#' 生成的比例数据是原数据在传入序列范围内的百分比位置，越靠近低点值越小。
#'
#' @param data T: vector<double> ~> 输入的数据
#'
#' @return T: vector<double> ~> 比例类型的向量，值范围`[0,1]`
#' @export
lstm.convert.scaled.data <- function(data) {
  max_value <- max(data)
  min_value <- min(data)
  spread <- max_value - min_value
  atrain <- (data - min_value) / spread
  return(atrain)
}
