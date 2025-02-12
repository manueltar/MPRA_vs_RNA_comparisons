
suppressMessages(library("plyr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("data.table", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("crayon", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("withr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("ggplot2", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("farver", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("labeling", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("optparse", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("dplyr", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("withr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("backports", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("broom", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("rstudioapi", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("cli", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("tzdb", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("svglite", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("ggeasy", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("sandwich", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("digest", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("tidyverse", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("RColorBrewer", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("svglite", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("cowplot", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))


opt = NULL

options(warn = 1)



data_wrangling = function(option_list)
{

  opt_in = option_list
  opt <<- option_list
  
  cat("All options:\n")
  printList(opt)
  
  
  #### READ and transform type ----
  
  type = opt$type
  
  cat("TYPE_\n")
  cat(sprintf(as.character(type)))
  cat("\n")

  
  #### READ and transform out ----
  
  out = opt$out
  
  cat("out_\n")
  cat(sprintf(as.character(out)))
  cat("\n")
  
 
  #### READ MPRA_results ----
  
  MPRA_results<-readRDS(file=opt$MPRA_results)
  
  cat("MPRA_results_0\n")
  cat(str(MPRA_results))
  cat("\n")
  cat(str(unique(MPRA_results$VAR)))
  cat("\n")
  
  
 
  ### Keep ASE ----
  
  MPRA_results_ASE<-droplevels(MPRA_results[which(MPRA_results$lof2Skew_meta_padj >=1),])
  
  
  cat("MPRA_results_ASE_0\n")
  cat(str(MPRA_results_ASE))
  cat("\n")
  cat(str(unique(MPRA_results_ASE$VAR)))
  cat("\n")
  
  
  MPRA_results_ASE$DIRECTIONALITY<-NA
  
  
  MPRA_results_ASE$DIRECTIONALITY[which(MPRA_results_ASE$log2Skew_meta < 0)]<-'REF'
  MPRA_results_ASE$DIRECTIONALITY[which(MPRA_results_ASE$log2Skew_meta > 0)]<-'ALT'
  
  MPRA_results_ASE$DIRECTIONALITY<-factor(MPRA_results_ASE$DIRECTIONALITY,
                                         levels = c('REF','ALT'),
                                         ordered=T)
  
  cat("MPRA_results_ASE_1\n")
  cat(str(MPRA_results_ASE))
  cat("\n")
  
  
  #### Now collapse the genes per variant and DIRECTIONALITY -----
  
  MPRA_results_ASE.dt<-data.table(MPRA_results_ASE, key=c("VAR","DIRECTIONALITY"))
  
  # cat("MPRA_results_ASE.dt_0\n")
  # cat(str(MPRA_results_ASE.dt))
  # cat("\n")
  
  MPRA_results_ASE_collapsed<-as.data.frame(MPRA_results_ASE.dt[,.(string_Cell_Type=paste(unique(Cell_Type), collapse=";")), by=key(MPRA_results_ASE.dt)], stringsAsFactors=F)
  
  
  cat("MPRA_results_ASE_collapsed_0\n")
  cat(str(MPRA_results_ASE_collapsed))
  cat("\n")
  cat(str(unique(MPRA_results_ASE_collapsed$VAR)))
  cat("\n")
  
  #### SAVE -------
  
  setwd(out)
  
  saveRDS(MPRA_results_ASE_collapsed, file="MPRA_results_ASE_collapsed_DIRECTIONALITY.rds")
  
  write.table(MPRA_results_ASE_collapsed, file="MPRA_results_ASE_collapsed_DIRECTIONALITY.tsv", sep="\t", quote=F, row.names = F)

 
 
}



printList = function(l, prefix = "    ") {
  list.df = data.frame(val_name = names(l), value = as.character(l))
  list_strs = apply(list.df, MARGIN = 1, FUN = function(x) { paste(x, collapse = " = ")})
  cat(paste(paste(paste0(prefix, list_strs), collapse = "\n"), "\n"))
}


#### main script ----

main = function() {
  cmd_line = commandArgs()
  cat("Command line:\n")
  cat(paste(gsub("--file=", "", cmd_line[4], fixed=T),
            paste(cmd_line[6:length(cmd_line)], collapse = " "),
            "\n\n"))
  option_list <- list(
    make_option(c("--MPRA_results"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--type"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--out"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required.")
  )
  parser = OptionParser(usage = "140__Rscript_v106.R
                        --subset type
                        --TranscriptEXP FILE.txt
                        --cadd FILE.txt
                        --ncboost FILE.txt
                        --type type
                        --out filename",
                        option_list = option_list)
  opt <<- parse_args(parser)
  
  data_wrangling(opt)


  
  
}


###########################################################################

system.time( main() )