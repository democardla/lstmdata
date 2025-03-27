#' 批量读取Excel文件方法
#'
#' @param file.names T: vector<charactor> 文件名
#'
#' @returns T: data.frame[col.names <- c(...)] 合并文件的数据框
#' @export
read.patch.excel <- function(file.names) {
  map_dfr(file.names, ~ read_excel(.x, col_names = TRUE, na = ""))
}

#' 批量读取csv文件方法
#'
#' @param file.names T: vector<charactor> 文件名
#'
#' @returns T: data.frame[col.names <- c(...)] 合并文件的数据框
#' @export
read.patch.csv <- function(file.names) {
  map_dfr(file.names, ~ read.csv(.x, header = TRUE))
}

#' 批量读取xport文件方法
#'
#' @param file.names T: vector<charactor> 文件名，要求传入的文件拥有相同数量的变量和相同的变量名称
#'
#' @returns T: data.frame[col.names <- c(...)] 合并文件的数据框
#' @export
read.patch.xport <- function(file.names) {
  map_dfr(file.names, ~ read.xport(.x))
}
