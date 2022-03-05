#' @title massstat_logo
#' @description massstat_logo
#' @author Xiaotao Shen
#' \email{shenxt@@stanford.edu}
#' @importFrom Biobase featureData
#' @importFrom crayon yellow red green bold bgRed
#' @import ggplot2
#' @importFrom pbapply pblapply pboptions
#' @importFrom stringr str_split str_replace_all str_trim str_detect str_extract
#' @importFrom dplyr filter select pull everything distinct one_of left_join mutate bind_cols arrange
#' @importFrom dplyr case_when
#' @importFrom tibble as_tibble enframe tibble rownames_to_column
#' @importFrom clisymbols symbol
#' @importFrom cli rule col_cyan tree
#' @importFrom utils packageVersion object.size write.csv tail
#' @importFrom purrr map map2
#' @importFrom plyr dlply .
#' @importFrom RColorBrewer brewer.pal
#' @importFrom readr read_csv cols
#' @importFrom readxl read_excel
#' @importFrom masstools get_os mz_rt_match
#' @importFrom BiocParallel MulticoreParam SnowParam bplapply
#' @importFrom magrittr %>%
#' @importFrom plotly ggplotly
#' @importFrom BiocGenerics basename
#' @importFrom patchwork plot_layout
#' @importFrom mixOmics plotIndiv plotVar pls
#' @import patchwork
#' @import snow
#' @importFrom stats coefficients lm loess median predict dist cor cor.test
#' @importFrom stats rgamma rt sd cor p.adjust prcomp t.test wilcox.test
#' @importFrom methods new is
#' @importFrom ggrepel geom_text_repel
#' @importClassesFrom massdataset mass_dataset tidymass_parameter
#' @import ggfortify
#' @import tidyr
#' @import graphics
#' @importFrom fastDummies dummy_cols
#' @importFrom tibble column_to_rownames
#' @export
#' @return logo
#' @examples
#' massstat_logo()

massstat_logo <- function() {
  cat(crayon::green("Thank you for using massstat_logo!\n"))
  message(crayon::green("Version", massstat_version, "(", update_date, ')\n'))
  cat(crayon::green("Bug fixing\n"))
  cat(crayon::green("More information: google tidymass massstat.\n"))
  cat(crayon::green(
    c(
      "                           _____ _        _   ",
      "                          / ____| |      | |  ",
      "  _ __ ___   __ _ ___ ___| (___ | |_ __ _| |_ ",
      " | '_ ` _ \\ / _` / __/ __|\\___ \\| __/ _` | __|",
      " | | | | | | (_| \\__ \\__ \\____) | || (_| | |_ ",
      " |_| |_| |_|\\__,_|___/___/_____/ \\__\\__,_|\\__|",
      "                                              ",
      "                                              "
    )
    
  ), sep = "\n")
}

massstat_version = "0.99.8"
update_date = as.character(Sys.time())

#' # library(cowsay)
#' #https://onlineasciitools.com/convert-text-to-ascii-art
#' # art <- readLines("logo.txt")
#' # dput(art)
