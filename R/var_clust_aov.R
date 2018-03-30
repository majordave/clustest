#' Variable per clustering method ANOVA test
#'
#' Perform an ANOVA test to a chosen set of dependent variables with each of the given clusterings.
#' @param dataset Dataset from which to perform test.
#' @param methods Character vector with the names of the columns corresponding to the clustering labels.
#' @param dep_vars Chracter vector with the names of the dependent variables to test.
#' @return Returns a data.frame with the results of the ANOVA test.
#' @examples
#' a_o_v  <- var_clust_aov(dataset = data, methods = c("KMeans", "FCMeans", "HCWard", "HCSing", "HCComp"), 
#' dep_vars = c("WOMACPAIN", "BLKL", "TKREvent"))
#' @export

var_clust_aov <- function(dataset, methods, dep_vars){
  purrr::map_dfr(dep_vars,
                 ~purrr::map_dfr(methods, 
                     function(x) {
                       dplyr::bind_cols(
                         dep_var = .,
                         paste(.,"~", x) %>% 
                           as.formula() %>% 
                           aov(dataset) %>% 
                           broom::tidy() %>% 
                           .[1,-2])
                      }
                   )
                )
}