
#' Title
#'
#' @param expose the variant you need to read from fread
#' @param samplesize if you want to calculate the Fval
#' @param pval pval
#'
#' @return expose_new
#' @export finn
#'
finn <- function(expose,samplesize=NULL,pval=5e-8){
    expose |> dplyr::filter(pval<{{pval}}) |>
        dplyr::select(rsids,ref,alt,pval,beta,sebeta,af_alt) |>
        purrr::set_names("SNP","other_allele","effect_allele","pval","beta","se","eaf") |>
        dplyr::mutate(samplesize={{samplesize}}) |> readr::write_csv(stringr::str_c("expose",{{pval}},".csv"))
    expose_new <- TwoSampleMR::read_exposure_data(stringr::str_c("expose",{{pval}},".csv"),sep=",",clump = T)
}

