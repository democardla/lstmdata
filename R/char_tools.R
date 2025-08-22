#' 将字符转换为标题格式（将 . 替换为空格，首字母大写）
#'
#' @description
#' 该函数将输入的字符串中的句点（`.`）替换为空格，并将每个单词的首字母转换为大写，
#' 以适应标题格式。适用于将列名或其他字符串转换为更具可读性的标题。
#'
#' @param char T: char 需要处理的字符，通常是带有 `.` 的列名
#'
#' @returns T: char 返回转换后的标题格式字符串，首字母大写，且句点被替换为空格
#' @export
#'
#' @examples
#' # 示例：将列名 "incidence.rate" 转换为 "Incidence Rate"
#' char.tools.uppercased("incidence.rate")
#'
#' # 示例：将列名 "total.sales.amount" 转换为 "Total Sales Amount"
#' char.tools.uppercased("total.sales.amount")
char.tools.uppercased <- function(char) {
  # 替换句点为空间
  char <- gsub("\\.", " ", char) # 替换 "." 为 " "

  # 将每个单词的首字母大写
  char <- toTitleCase(char) # 转换为标题格式（首字母大写）

  return(char)
}
