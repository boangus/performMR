#' Calculate F value
#'
#' @param data expose data,it's better to use TwosampleMR read_exposed_data's data
#' @param eaf frequency col
#' @param beta effective col
#' @param se se col
#' @param N samplesize col
#' @param path the way you save the expose data
#' @param filename the name you save the expose data
#'
#' @return the total F value, and a file of the expose data which includ the F value respectively
#' and a select column which based on the F value more than 10.
#' @importFrom utils write.csv
#' @export Fval


Fval <- function(data,eaf="eaf.exposure",beta="beta.exposure",
                 se="se.exposure",N="samplesize.exposure",path="",filename="Fval.csv"){
    eaf <- data[,eaf]
    beta <- data[,beta]
    se <- data[,se]
    N <- data[,N]
    beta_sd <- beta/(se*sqrt(N))
    R2 <- 2*(1-eaf)*eaf*beta_sd*beta_sd
    F_val <- (N-2)*R2/(1-R2)

    data$R2 <- R2
    data$Fval <- F_val

    r2 <- sum(R2)
    Ftotal <- (r2/(1-r2))*(N-nrow(data)-1)/nrow(data)
    print(paste("Ftotal",Ftotal[nrow(data)]))
    data$select <- ifelse(data$Fval>10,"T","F")
    write.csv(data,paste(path,filename,sep=""),row.names = F)
    data <- data[data$Fval>10,]
    return(data)
}

