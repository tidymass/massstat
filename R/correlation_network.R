#' @title mutate_fc
#' @description Calculate fold change.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object tidymass-class object.
#' @param margin variable or sample
#' @param cor_method spearman, pearson or kendall
#' @param p_adjust_method see ?p.adjust
#' @param p_adjust_cutoff between 0 to 1
#' @param p_value_cutoff between 0 to 1
#' @param pos_cor_cutoff between 0 to 1
#' @param neg_cor_cutoff between -1 to 0
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom massdataset extract_variable_info
#' @return tbl_graph class object (from tidygraph)
#' @export
#' @examples
#' library(massdataset)
#' library(magrittr)
#' library(dplyr)
#'
#' data("liver_aging_pos")
#' liver_aging_pos
#'
#' qc_id <-
#'   liver_aging_pos %>%
#'   activate_mass_dataset(what = "sample_info") %>%
#'   dplyr::filter(group == "QC") %>%
#'   dplyr::pull(sample_id)
#'
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
#' graph_data <-
#'   convert_mass_dataset2graph(
#'     object = object,
#'     margin = "variable",
#'     cor_method = "spearman",
#'     p_adjust_cutoff = 1,
#'     p_value_cutoff = 0.0001,
#'     pos_cor_cutoff = 0.7,
#'     neg_cor_cutoff = -0.7
#'   )
#'
#' library(ggraph)
#' extrafont::loadfonts()
#' ggraph(graph = graph_data, layout = "kk") +
#'   geom_edge_fan(aes(width = -log(p_value, 10)),
#'                 show.legend = TRUE) +
#'   geom_node_point(aes(size = mz)) +
#'   theme_graph()
#'
#' ggraph(graph = graph_data, layout = "fr") +
#'   geom_edge_fan(aes(width = -log(p_value, 10)),
#'                 show.legend = TRUE) +
#'   geom_node_point(aes(size = mz)) +
#'   theme_graph()

convert_mass_dataset2graph <-
  function(object,
           margin = c("variable", "sample"),
           cor_method = c("spearman", "pearson", "kendall"),
           p_adjust_method = c(c(
             "BH",
             "holm",
             "hochberg",
             "hommel",
             "bonferroni",
             "BY",
             "fdr",
             "none"
           )),
           p_adjust_cutoff = 0.05,
           p_value_cutoff = 0.05,
           pos_cor_cutoff = 0,
           neg_cor_cutoff = 0) {
    
    if(!requireNamespace("tidygraph", quietly = TRUE)){
      stop("Please install tidygraph pacakge first.")
    }
    
    margin <-
      match.arg(margin)
    cor_method <-
      match.arg(cor_method)
    p_adjust_method <-
      match.arg(p_adjust_method)
    
    if (p_adjust_cutoff < 0) {
      stop("p_adjust_cutoff should be between 0 - 1")
    }
    
    if (p_value_cutoff < 0) {
      stop("p_value_cutoff should be between 0 - 1")
    }
    
    if (pos_cor_cutoff < 0 | pos_cor_cutoff > 1) {
      stop("p_cutoff should be between 0 - 1")
    }
    
    if (neg_cor_cutoff > 0 | neg_cor_cutoff < -1) {
      stop("p_cutoff should be between -1 to 0")
    }
    
    ###correlation
    cor_data <-
      cor_mass_dataset(
        x = object,
        margin = margin,
        method = cor_method,
        p_adjust_method = p_adjust_method,
        data_type = "longer"
      )
    
    cor_data <-
      cor_data %>%
      dplyr::filter(p_value < p_value_cutoff &
                      p_adjust < p_adjust_cutoff) %>%
      dplyr::filter((correlation > pos_cor_cutoff &
                       correlation > 0) |
                      (correlation < neg_cor_cutoff &
                         correlation < 0)
      )
    
    edge_data <-
      cor_data
    
    if (margin == "variable") {
      node_data <-
        massdataset::extract_variable_info(object) %>%
        # object@variable_info %>%
        dplyr::rename(node = variable_id) %>%
        dplyr::filter(node %in% unique(c(edge_data$from, edge_data$to)))
      
      graph_data <-
        tidygraph::tbl_graph(nodes = node_data,
                             edges = edge_data,
                             directed = FALSE)
    } else{
      node_data <-
        object@sample_info %>%
        dplyr::rename(node = sample_id) %>%
        dplyr::filter(node %in% unique(c(edge_data$from, edge_data$to)))
      
      graph_data <-
        tidygraph::tbl_graph(nodes = node_data,
                             edges = edge_data,
                             directed = FALSE)
    }
    return(graph_data)
  }
