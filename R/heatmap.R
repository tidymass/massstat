#####from the Heatmap
#' @method Heatmap mass_dataset
#' @docType methods
#' @importFrom ComplexHeatmap Heatmap
#' @export

# library(massdataset)
# library(tidyverse)
# data("liver_aging_pos")
#
# qc_id <-
#   liver_aging_pos %>%
#   activate_mass_dataset(what = "sample_info") %>%
#   dplyr::filter(group == "QC") %>%
#   dplyr::pull(sample_id)
# object <-
#   mutate_rsd(liver_aging_pos, according_to_samples = qc_id)
#
# ###only remain the features with rt > 100, mz > 150 and rsd < 30
# object <-
#   object %>%
#   activate_mass_dataset(what = "variable_info") %>%
#   dplyr::filter(rt > 100) %>%
#   dplyr::filter(mz > 150) %>%
#   dplyr::filter(rsd < 30)
#
#
# matrix <-
#   object %>%
#   `+`(1) %>%
#   log(10) %>%
#   scale_data()

Heatmap.mass_dataset <-
  function(matrix, ...) {
    expression_data <-
      matrix@expression_data
    ComplexHeatmap::Heatmap(as.numeric(expression_data),
                            ...)
  }

#' @importFrom ComplexHeatmap Heatmap
#' @export
ComplexHeatmap::Heatmap
