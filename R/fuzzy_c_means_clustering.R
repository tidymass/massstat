#' #' @title run_clustering
#' #' @description run_clustering
#' #' @author Xiaotao Shen
#' #' \email{shenxt1990@@163.com}
#' #' @param object tidymass-class object.
#' #' @return prcomp object.
#' #' @export
#' #' @examples
#' library(massdataset)
#' library(tidyverse)
#' data("liver_aging_pos")
#' 
#' qc_id <-
#'   liver_aging_pos %>%
#'   activate_mass_dataset(what = "sample_info") %>%
#'   dplyr::filter(group == "QC") %>%
#'   dplyr::pull(sample_id)
#' object <-
#'   mutate_rsd(liver_aging_pos, according_to_samples = qc_id)
#' 
#' ###only remain the features with rt > 100, mz > 150 and rsd < 30
#' object <-
#'   object %>%
#'   activate_mass_dataset(what = "variable_info") %>%
#'   dplyr::filter(rt > 100) %>%
#'   dplyr::filter(mz > 150) %>%
#'   dplyr::filter(rsd < 30)
#' 
#' ##only remain the week 24 samples
#' object <-
#'   object %>%
#'   activate_mass_dataset(what = "sample_info") %>%
#'   dplyr::filter(group == "24W")
#' 
#' dim(object)
#' 
#' object <-
#'   object %>%
#'   `+`(1) %>%
#'   log(10) %>%
#'   scale_data(method = "auto")
#' 
#' 
#' 
#' 
#' 
#' 
#' run_clustering <-
#'   function(object,
#'            path = ".") {
#'     dir.create(path, recursive = TRUE, showWarnings = FALSE)
#'     
#'     expression_data <-
#'       object@expression_data
#'     
#'     time <- 1:ncol(expression_data)
#'     
#'     expression_data <- rbind(time, expression_data)
#'     
#'     row.names(expression_data)[1] <- "time"
#'     
#'     #save it to a temp file
#'     dir.create("example")
#'     tmp <- tempfile(tmpdir = "example")
#'     
#'     write.table(
#'       expression_data,
#'       file = tmp,
#'       sep = '\t',
#'       quote = F,
#'       col.names = NA
#'     )
#'     
#'     #read it back in as an expression set
#'     data <- Mfuzz::table2eset(filename = tmp)
#'     m1 <- Mfuzz::mestimate(eset = data)
#'     
#'     Dmin(
#'       data,
#'       m = m1,
#'       crange = seq(2, 22, 1),
#'       repeats = 3,
#'       visu = TRUE
#'     )
#'     
#'     cluster_number <- 5
#'     c <- Mfuzz::mfuzz(eset = data, c = cluster_number, m = m1)
#'     
#'     centers <- c$centers
#'     names(c$cluster) == rownames(c$membership)
#'     
#'     cluster_info <-
#'       data.frame(
#'         variable_id = names(c$cluster),
#'         c$membership,
#'         cluster = c$cluster,
#'         stringsAsFactors = FALSE
#'       ) %>%
#'       dplyr::arrange(cluster)
#'     
#'     rownames(cluster_info) <- NULL
#'     colnames(cluster_info) <-
#'       colnames(cluster_info) %>%
#'       stringr::str_replace(pattern = "^X", "membership_")
#'   }
