clusterise <- function(data, dists, max_k, min_k = 2){
  ward     <- hclust(dists, method = "ward.D")
  single   <- hclust(dists, method = "single")
  complete <- hclust(dists, method = "complete")
  
  purrr::map_dfr(min_k:max_k,
          ~dplyr::tibble(
            k = .,
            KMeans  = kmeans(data, .)$cluster,
            FCMeans = e1071::cmeans(data, .)$cluster,
            HCWard  = cutree(ward, .),
            HCSing  = cutree(single, .),
            HCComp  = cutree(complete, .)
          ))
}

get_dunns <- function(clusts, dists){
  clusts %>%
    dplyr::group_by(k) %>% 
    dplyr::summarise_all(dplyr::funs(clValid::dunn(dists, .)))
}

get_best_k <- function(dunns, ks, m_names) {
  dunns %>% 
    dplyr::slice(which.max(ks)) %>% 
    dplyr::select(k) %>% 
    dplyr::rename(!!m_names := k)
}

conf_int <- function(vec){
  CI <- tryCatch({
    tt <- t.test(vec)$conf.int
    tibble(
      ci_min = tt[1],
      ci_max = tt[2]
    )
  }, 
  error = function(x) {
    val <- vec[1]
    tibble(
      ci_min = val,
      ci_max = val
    )
  })
}

#' Find _k_ function
#'
#' A bootstrap method for calculating optimal _k_ s for each given clustering algorithm by:
#' 1. Sample of a given size from the dataset with replacement.
#' 2. Calculate Dunn index for said sample per clustering algorithm for _k_ s between and 2 and a given value.
#' 3. Find _k_ that maximizes the Dunn index per clustering algorithm.
#' 4. Repeat steps 1 to 3 _iters_ times.
#' 5. Get mean best _k_ per clustering algorithm.
#' @param dataset Dataset from which to generate bootstrap.
#' @param iters Number of iterations/samples the bootstrap will take.
#' @param max_k Maximum _k_ value to analize.
#' @return Returns a list containing:
#' \describe{
#'   \item{\code{all_best_ks}}{A data.frame containing the best _k_ found for each clustering per sample.}
#'   \item{\code{k_stats}}{A data.frame of the mean best _k_ and confidence intervals found for each clustering.}
#'   \item{\code{best_k}}{A data.frame of the best _k_ found for each clustering.}
#'   \item{\code{c_samples}}{A data.frame of the bootrapped data clustered with optimal _k_ s.}
#' }
#' @examples
#' data   <- load("my_data.RData")
#' res    <- bootstrap(dataset = data, iters = 30, max_k = 5)
#' all_ks <- res$_all_best_ks
#' best_k <- res$best_k
#' k_st   <- res$k_stats
#' c_samp <- res$c_samples
#' @export

find_k <- function(dataset, train_size, iters, max_k){
  
  boot <- 
    purrr::map_dfr(1:iters, ~dplyr::sample_frac(dataset, replace = TRUE), .id = "sample") %>% 
    tidyr::nest(-sample) %>% 
    dplyr::mutate(
      dists  = purrr::map(data, dist),
      clusts = purrr::map2(data, dists, ~clusterise(.x, .y, max_k)),
      dunns  = purrr::map2(clusts, dists, get_dunns)
    )
  
  all_best_ks <- purrr::map_df(boot$dunns, ~purrr::map2_dfc(.[-1], names(.[-1]), 
                                                            function(x, y) get_best_k(., x, y)))
  k_stats <- 
    all_best_ks %>%
    tidyr::gather(method, best_k) %>%
    dplyr::group_by(method) %>%
    dplyr::do(ci = conf_int(.$best_k), mean_best_k = mean(.$best_k)) %>% 
    tidyr::unnest()
  
  best_k <- 
    k_stats %>% 
    dplyr::select(method, mean_best_k) %>% 
    dplyr::mutate(mean_best_k = round(mean_best_k)) %>% 
    tidyr::spread(method, mean_best_k)
  
  clusts <- 
    boot %>% 
    dplyr::select(clusts) %>% 
    tidyr::unnest()
  
  clusts <-  
    best_k %>% 
    purrr::map2_dfc(., names(.), ~dplyr::filter(clusts, k == .x) %>% 
                                    dplyr::select(.y))
  
  c_samples <- 
    boot %>% 
    tidyr::unnest(data) %>%
    dplyr::bind_cols(clusts)
  
  list(all_best_ks = all_best_ks, k_stats = k_stats, best_k = best_k, c_samples = c_samples)
}