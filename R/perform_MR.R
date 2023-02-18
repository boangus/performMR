#' perform MR for one code
#'
#' @param data After using TwosampleMR harmorize's data
#' @param path which folder you want to save
#' @param save_name the name you want to save
#' @param phenoscanner to check the snp in phenoscanner
#' @param MR_CSV to perform mr and write it to csv
#' @param MR_PIC to get the picture
#'
#' @return three csv file and a PDF which includ three picture
#' @importFrom grDevices dev.off pdf
#' @importFrom utils write.csv
#' @export perform_MR

perform_MR <- function(data,path="",save_name="exposure_outcome",
                       phenoscanner=F,MR_CSV=T,MR_PIC=T){
    if(phenoscanner){
        res <- phenoscanner::phenoscanner(snpquery=data$SNP)
        write.csv(res$results,paste(path,"PS_",save_name,".csv",sep = ""))
    }


    if(MR_CSV){
        TwoSampleMR::generate_odds_ratios(mr_res = TwoSampleMR::mr(data)) |>
            write.csv(paste(path,"MR_",save_name,".csv",sep = ""))

        TwoSampleMR::mr_pleiotropy_test(data) |>
            write.csv(paste(path,"PLE_",save_name,".csv",sep = ""))

        TwoSampleMR::mr_heterogeneity(data) |>
            write.csv(paste(path,"HET_",save_name,".csv",sep = ""))
    }

    if(MR_PIC){
        pdf(paste(path,"PIC_",save_name,".pdf",sep = ""))

        p1<- TwoSampleMR::mr_scatter_plot(mr_results = TwoSampleMR::mr(data,method_list =  c("mr_ivw","mr_egger_regression","mr_weighted_median")),data)
        print(p1)

        p2 <- TwoSampleMR::mr_funnel_plot(singlesnp_results = TwoSampleMR::mr_singlesnp(data))
        print(p2)

        p3 <- TwoSampleMR::mr_leaveoneout_plot(leaveoneout_results = TwoSampleMR::mr_leaveoneout(data))
        print(p3)

        res_single <- TwoSampleMR::mr_singlesnp(data)
        p4 <- TwoSampleMR::mr_forest_plot(res_single)
        dev.off()

    }

}

