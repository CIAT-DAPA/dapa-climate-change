#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#August 2012


#in the KS, if P>0.05 then the distributions come from the same parent distributions
#hence the distributions would be different of the P-value is below the threshold
#chosen. In this case at P<0.001 (***), P<0.05 (**), P<0.1(*), the distributions are
#said to be different
ks_selection <- function(sk_data,best_post) {
  exp_list <- names(sk_data)
  exp_list <- exp_list[grep("EXP",exp_list)]
  #exp_list <- exp_list[-grep(best_post,exp_list)]
  
  out_ks <- data.frame()
  for (exp in exp_list) {
    if (exp == best_post) {
      ksr <- data.frame(EXP=exp,D=NA,P=NA)
    } else {
      ksr <- ks.test(best_dist,sk_data[,exp])
      ksr <- data.frame(EXP=exp,D=ksr$statistic,P=ksr$p.value)
    }
    out_ks <- rbind(out_ks,ksr)
  }
  out_ks$SIGNIF <- "NS*"
  out_ks$SIGNIF[which(out_ks$P > 0.1)] <- "NS"
  out_ks$SIGNIF[which(out_ks$P <= 0.1)] <- "*"
  out_ks$SIGNIF[which(out_ks$P <= 0.05)] <- "**"
  out_ks$SIGNIF[which(out_ks$P <= 0.001)] <- "***"
  row.names(out_ks) <- 1:nrow(out_ks)
  return(out_ks)
}
