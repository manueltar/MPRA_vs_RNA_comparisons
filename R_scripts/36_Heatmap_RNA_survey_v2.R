
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
suppressMessages(library("BiocGenerics", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("S4Vectors", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("IRanges", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("GenomeInfoDb", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("GenomicRanges", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("Biobase", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("AnnotationDbi", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("GO.db", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("org.Hs.eg.db", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("TxDb.Hsapiens.UCSC.hg19.knownGene", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("rtracklayer", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("cowplot", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("RColorBrewer", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))


opt = NULL

options(warn = 1)

data_wrangling_and_heatmap = function(option_list)
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
  
  
  #### READ Table_S6 ----
  
  Table_S6<-readRDS(file=opt$Table_S6)
  
  
  cat("Table_S6_0\n")
  cat(str(Table_S6))
  cat("\n")
  cat(str(unique(Table_S6$VAR)))
  cat("\n")
  
  Table_S6$chr<-gsub("_.+$","",Table_S6$VAR)
  Table_S6$pos<-gsub("^[^_]+_","",Table_S6$VAR)
  Table_S6$pos<-as.integer(gsub("_.+$","",Table_S6$pos))
  Table_S6$ref<-gsub("^[^_]+_[^_]+_","",Table_S6$VAR)
  Table_S6$ref<-gsub("_.+$","",Table_S6$ref)
  Table_S6$alt<-gsub("^[^_]+_[^_]+_[^_]+_","",Table_S6$VAR)
  
  Table_S6$chr<-factor(Table_S6$chr,
                       levels=c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11",
                                "chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr20","chr21",
                                "chr22","chr23","chrX","chrY"), ordered=T)
  
  
  cat("Table_S6_1\n")
  cat(str(Table_S6))
  cat("\n")
  cat(str(unique(Table_S6$chr)))
  cat("\n")
  cat(str(unique(Table_S6$pos)))
  cat("\n")
  cat(str(unique(Table_S6$ref)))
  cat("\n")
  cat(str(unique(Table_S6$alt)))
  cat("\n")
  
  indx.int<-c(which(colnames(Table_S6) == 'VAR'),which(colnames(Table_S6) == 'rs'),which(colnames(Table_S6) == 'chr'),which(colnames(Table_S6) == 'pos'),which(colnames(Table_S6) == 'ref'),which(colnames(Table_S6) == 'alt'))
  
  Table_S6_subset<-unique(Table_S6[,indx.int])
  
  cat("Table_S6_subset_0\n")
  cat(str(Table_S6_subset))
  cat("\n")
  cat(str(unique(Table_S6_subset$chr)))
  cat("\n")
  cat(str(unique(Table_S6_subset$pos)))
  cat("\n")
  cat(str(unique(Table_S6_subset$ref)))
  cat("\n")
  cat(str(unique(Table_S6_subset$alt)))
  cat("\n")
  
  #### READ Table_S7 ----
  
  Table_S7<-readRDS(file=opt$Table_S7)
  
  cat("Table_S7_0\n")
  cat(str(Table_S7))
  cat("\n")
  cat(str(unique(Table_S7$VAR)))
  cat("\n")
  cat(str(unique(Table_S7$ensembl_gene_id)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7$RNASeq_source))))
  cat("\n")
  
  Table_S7$Analysis[which(Table_S7$Analysis == 'DTU')]<-'ATU'
  
  Table_S7<-merge(Table_S6_subset,
                  Table_S7,
                  by=c('VAR','rs'))
  
  cat("Table_S7_1\n")
  cat(str(Table_S7))
  cat("\n")
  cat(str(unique(Table_S7$VAR)))
  cat("\n")
  cat(str(unique(Table_S7$ensembl_gene_id)))
  cat("\n")
  
  Table_S7<-Table_S7[order(Table_S7$chr,Table_S7$pos),]
  
  rs_vector<-unique(Table_S7$rs)
  
  cat("Table_S7_1.5\n")
  cat(str(Table_S7))
  cat("\n")
  cat(str(unique(Table_S7$VAR)))
  cat("\n")
  cat(str(unique(Table_S7$ensembl_gene_id)))
  cat("\n")
  cat("rs_vector_0\n")
  cat(str(rs_vector))
  cat("\n")
  
  
  
  Table_S7$RNASeq_source_2<-revalue(Table_S7$RNASeq_source, 
                                                               c("Whole blood"="Whole blood",
                                                                 "Monocyte"="Monocytes",
                                                                 "Neutrophil"="Neutrophils",
                                                                 "Tcell"="naive T-CD4 Cells"))
  
  Table_S7<-Table_S7[,-which(colnames(Table_S7) == 'RNASeq_source')]
  
  colnames(Table_S7)[which(colnames(Table_S7) == 'RNASeq_source_2')]<-'RNASeq_source'
  
  Table_S7$Analysis<-factor(Table_S7$Analysis,
                                                       levels = c('DE','ATU'),
                                                       ordered=T)
  
  Table_S7$RNASeq_source<-factor(Table_S7$RNASeq_source,
                                                              levels=c("Whole blood","Monocytes","Neutrophils","naive T-CD4 Cells"),
                                                              ordered=T)
  
  Table_S7$rs<-factor(Table_S7$rs,
                                 levels=rs_vector,
                                 ordered=T)
  
  cat("Table_S7_2\n")
  cat(str(Table_S7))
  cat("\n")
  cat(str(unique(Table_S7$VAR)))
  cat("\n")
  cat(str(unique(Table_S7$ensembl_gene_id)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7$RNASeq_source))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7$RNASeq_source))))
  cat("\n")
  
  
  #### ATU select the transcript with the highest adjust lopval ----
  
  Table_S7.dt<-data.table(Table_S7, key=c("rs","ensembl_gene_id","HGNC","Analysis","RNASeq_source"))
  
  
  Table_S7_MAX_adjusted_minus_logpval<-as.data.frame(Table_S7.dt[,.SD[which.max(adjusted_minus_logpval)], by=key(Table_S7.dt)], stringsAsFactors=F)
  
  Table_S7_MAX_adjusted_minus_logpval$RNASeq_source<-factor(Table_S7_MAX_adjusted_minus_logpval$RNASeq_source)
  
  cat("Table_S7_MAX_adjusted_minus_logpval_0\n")
  cat(str(Table_S7_MAX_adjusted_minus_logpval))
  cat("\n")
  cat(str(unique(Table_S7_MAX_adjusted_minus_logpval$rs)))
  cat("\n")
  cat(str(unique(Table_S7_MAX_adjusted_minus_logpval$ensembl_gene_id)))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Table_S7_MAX_adjusted_minus_logpval$Analysis))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Table_S7_MAX_adjusted_minus_logpval$Analysis)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7_MAX_adjusted_minus_logpval$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7_MAX_adjusted_minus_logpval$RNASeq_source))))
  cat("\n")
  
  

  

  #### Factorize & order columns 
  
  Table_S7_MAX_adjusted_minus_logpval$Significance<-NA
  
  Table_S7_MAX_adjusted_minus_logpval$Significance[which(Table_S7_MAX_adjusted_minus_logpval$adjusted_minus_logpval >= 1.3)]<-'YES'
  Table_S7_MAX_adjusted_minus_logpval$Significance[which(Table_S7_MAX_adjusted_minus_logpval$adjusted_minus_logpval < 1.3)]<-'NO'
  
  Table_S7_MAX_adjusted_minus_logpval$Significance<-factor(Table_S7_MAX_adjusted_minus_logpval$Significance,
                                                              levels=c('NO','YES'),
                                                              ordered=T)
  

  
  Table_S7_MAX_adjusted_minus_logpval$rs_HGNC<-paste(Table_S7_MAX_adjusted_minus_logpval$rs,Table_S7_MAX_adjusted_minus_logpval$HGNC, sep=' ')
  
  

  
  
  cat("Table_S7_MAX_adjusted_minus_logpval_1\n")
  cat(str(Table_S7_MAX_adjusted_minus_logpval))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7_MAX_adjusted_minus_logpval$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7_MAX_adjusted_minus_logpval$RNASeq_source))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7_MAX_adjusted_minus_logpval$Analysis)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7_MAX_adjusted_minus_logpval$Analysis))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7_MAX_adjusted_minus_logpval$Significance)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7_MAX_adjusted_minus_logpval$Significance))))
  cat("\n")
  
  

  
  
  #### select rs_HGNC combinations ----
  
  Table_S7_MAX_adjusted_minus_logpval_SIG<-Table_S7_MAX_adjusted_minus_logpval[which(Table_S7_MAX_adjusted_minus_logpval$Significance == 'YES'),]
  
  Table_S7_MAX_adjusted_minus_logpval_SIG<-Table_S7_MAX_adjusted_minus_logpval_SIG[order(Table_S7_MAX_adjusted_minus_logpval_SIG$rs),]
  
  
  Table_S7_MAX_adjusted_minus_logpval_SIG$rs_HGNC<-paste(Table_S7_MAX_adjusted_minus_logpval_SIG$rs,Table_S7_MAX_adjusted_minus_logpval_SIG$HGNC, sep=' ')
  
  
  cat("------------------------------------------>rs_HGNC\n")
  cat(sprintf(as.character(unique(Table_S7_MAX_adjusted_minus_logpval_SIG$rs_HGNC))))
  cat("\n")
  
  
  cat("Table_S7_MAX_adjusted_minus_logpval_0\n")
  cat(str(Table_S7_MAX_adjusted_minus_logpval))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7_MAX_adjusted_minus_logpval$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7_MAX_adjusted_minus_logpval$RNASeq_source))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7_MAX_adjusted_minus_logpval$Analysis)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7_MAX_adjusted_minus_logpval$Analysis))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7_MAX_adjusted_minus_logpval$Significance)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7_MAX_adjusted_minus_logpval$Significance))))
  cat("\n")
  
  SELECTED_rs_HGNC<-unique(Table_S7_MAX_adjusted_minus_logpval_SIG$rs_HGNC)
  
  cat("SELECTED_VAR_HGNC_0\n")
  cat(str(SELECTED_rs_HGNC))
  cat("\n")
  
  REP<-Table_S7_MAX_adjusted_minus_logpval[which(Table_S7_MAX_adjusted_minus_logpval$rs_HGNC%in%SELECTED_rs_HGNC),]
  
  
  REP$rs_HGNC<-factor(REP$rs_HGNC,
                           levels=rev(SELECTED_rs_HGNC),
                           ordered=T)
  
  cat("REP_0\n")
  cat(str(REP))
  cat("\n")
  cat(str(unique(REP$rs_HGNC)))
  cat("\n")
  cat(sprintf(as.character(names(summary(REP$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(REP$RNASeq_source))))
  cat("\n")
  cat(sprintf(as.character(names(summary(REP$Analysis)))))
  cat("\n")
  cat(sprintf(as.character(summary(REP$Analysis))))
  cat("\n")
  cat(sprintf(as.character(names(summary(REP$Significance)))))
  cat("\n")
  cat(sprintf(as.character(summary(REP$Significance))))
  cat("\n")
  cat(sprintf(as.character(names(summary(REP$Beta_Z_score)))))
  cat("\n")
  cat(sprintf(as.character(summary(REP$Beta_Z_score))))
  cat("\n")
  
  setwd(out)
  
  saveRDS(REP, file='REP_DE_ATU_hetmap_table.rds')
  
  ##### pivot wider by analysis ----
  
  # c("rs","ensembl_gene_id","HGNC","Analysis","adjusted_minus_logpval","Beta","Beta_Z_score","transcript_id","adjusted_pval","Significance",)
  
  REP_wide<-as.data.frame(pivot_wider(REP,
                        id_cols=c("rs_HGNC"),
                        names_from=c("RNASeq_source","Analysis"),
                        names_sep='|',
                        values_from=Beta_Z_score), stringAsFactors=F)
  
  # REP_wide$rs_HGNC<-factor(REP_wide$rs_HGNC,
  #                          levels=rev(SELECTED_rs_HGNC),
  #                          ordered=T)
  
  REP_wide$MOCK_COORD<-1
  
  cat("REP_wide_0\n")
  cat(str(REP_wide))
  cat("\n")
  
  
  
  
  
  #### Check the effect size scales ----
  
  # WB_DE
  
  indx_WB_DE<-which(colnames(REP_wide) =='Whole blood|DE')
 
  summary_Beta_Z_indx_WB_DE<-summary(REP_wide[,indx_WB_DE])
  
  cat("summary_Beta_Z_indx_WB_DE\n")
  cat(sprintf(as.character(names(summary_Beta_Z_indx_WB_DE))))
  cat("\n")
  cat(sprintf(as.character(summary_Beta_Z_indx_WB_DE)))
  cat("\n")
  
  lower_tile_value<--0.2
  upper_tile_value<-0.2
  
  REP_wide[,indx_WB_DE][which(!is.na(REP_wide[,indx_WB_DE]) &
                   REP_wide[,indx_WB_DE] < lower_tile_value )]<-lower_tile_value
  
  REP_wide[,indx_WB_DE][which(!is.na(REP_wide[,indx_WB_DE]) &
                   REP_wide[,indx_WB_DE] > upper_tile_value )]<-upper_tile_value
  
  breaks.WB_DE<-seq(lower_tile_value,upper_tile_value,by=0.1)
  labels.WB_DE<-as.character(breaks.WB_DE)
  
  cat("labels.WB_DE_0\n")
  cat(str(labels.WB_DE))
  cat("\n")
  
  ### preparatory things
    
  REP_wide$MOCK_COORD<-1
  
  SELECTED_VAR_HGNC_WB_DE<-unique(REP$rs_HGNC[which(REP$RNASeq_source == 'Whole blood' & REP$Analysis == 'DE' & REP$Significance == 'YES')])

  cat("SELECTED_VAR_HGNC_WB_DE_0\n")
  cat(str(SELECTED_VAR_HGNC_WB_DE))
  cat("\n")
  
  
  REP_wide$Significance<-NA
  
  REP_wide$Significance[which(REP_wide$rs_HGNC%in%SELECTED_VAR_HGNC_WB_DE)]<-'YES'
  REP_wide$Significance[-which(REP_wide$rs_HGNC%in%SELECTED_VAR_HGNC_WB_DE)]<-'NO'
  
  REP_wide$Significance<-factor(REP_wide$Significance,
                                                           levels=c('NO','YES'),
                                                           ordered=T)
  
  cat("REP_wide_1\n")
  cat(str(REP_wide))
  cat("\n")
  cat(sprintf(as.character(names(summary(REP_wide$Significance)))))
  cat("\n")
  cat(sprintf(as.character(summary(REP_wide$Significance))))
  cat("\n")
  
  ggheatmap_WB_DE <-ggplot(data=REP_wide,
                     aes(x=MOCK_COORD, y=rs_HGNC, fill = REP_wide[,indx_WB_DE]))+
    geom_tile()+
    geom_tile(data=subset(REP_wide, Significance == 'YES'), fill = NA, color = "black", size = 1)+
    scale_fill_gradient2(name=paste("Beta","z scored","Whole Blood DE", sep="\n"),
                         low = "blue", high = "red",mid="white",midpoint=0,
                         na.value = NA,
                         breaks=breaks.WB_DE,
                         labels=labels.WB_DE,
                         limits=c(breaks.WB_DE[1],
                                  breaks.WB_DE[length(breaks.WB_DE)]))+
    theme_minimal()+ # minimal theme
    scale_y_discrete(name=NULL, drop=F)+
    scale_x_discrete(name=NULL, drop=F)+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(family="sans"))+
    theme(legend.position="bottom",legend.title=element_blank(), legend.text = element_text(family="sans"))+
    coord_fixed()
  

 
  # WB_ATU
  
  indx_WB_ATU<-which(colnames(REP_wide) =='Whole blood|ATU')
  
  summary_Beta_Z_indx_WB_ATU<-summary(REP_wide[,indx_WB_ATU])
  
  cat("summary_Beta_Z_indx_WB_ATU\n")
  cat(sprintf(as.character(names(summary_Beta_Z_indx_WB_ATU))))
  cat("\n")
  cat(sprintf(as.character(summary_Beta_Z_indx_WB_ATU)))
  cat("\n")
  
  lower_tile_value<--1
  upper_tile_value<-1
  
  REP_wide[,indx_WB_ATU][which(!is.na(REP_wide[,indx_WB_ATU]) &
                                REP_wide[,indx_WB_ATU] < lower_tile_value )]<-lower_tile_value
  
  REP_wide[,indx_WB_ATU][which(!is.na(REP_wide[,indx_WB_ATU]) &
                                REP_wide[,indx_WB_ATU] > upper_tile_value )]<-upper_tile_value
  
  breaks.WB_ATU<-seq(lower_tile_value,upper_tile_value,by=0.5)
  labels.WB_ATU<-as.character(breaks.WB_ATU)
  
  cat("labels.WB_ATU_0\n")
  cat(str(labels.WB_ATU))
  cat("\n")
  
  ### preparatory things
  
  
  
  SELECTED_VAR_HGNC_WB_ATU<-unique(REP$rs_HGNC[which(REP$RNASeq_source == 'Whole blood' & REP$Analysis == 'ATU' & REP$Significance == 'YES')])
  
  cat("SELECTED_VAR_HGNC_WB_ATU_0\n")
  cat(str(SELECTED_VAR_HGNC_WB_ATU))
  cat("\n")
  
  
  REP_wide$Significance<-NA
  
  REP_wide$Significance[which(REP_wide$rs_HGNC%in%SELECTED_VAR_HGNC_WB_ATU)]<-'YES'
  REP_wide$Significance[-which(REP_wide$rs_HGNC%in%SELECTED_VAR_HGNC_WB_ATU)]<-'NO'
  
  REP_wide$Significance<-factor(REP_wide$Significance,
                                levels=c('NO','YES'),
                                ordered=T)
  
  cat("REP_wide_1\n")
  cat(str(REP_wide))
  cat("\n")
  cat(sprintf(as.character(names(summary(REP_wide$Significance)))))
  cat("\n")
  cat(sprintf(as.character(summary(REP_wide$Significance))))
  cat("\n")
  
  ggheatmap_WB_ATU <-ggplot(data=REP_wide,
                           aes(x=MOCK_COORD, y=rs_HGNC, fill = REP_wide[,indx_WB_ATU]))+
    geom_tile()+
    geom_tile(data=subset(REP_wide, Significance == 'YES'), fill = NA, color = "black", size = 1)+
    scale_fill_gradient2(name=paste("Beta","z scored","Whole Blood ATU", sep="\n"),
                         low = "blue", high = "red",mid="white",midpoint=0,
                         na.value = NA,
                         breaks=breaks.WB_ATU,
                         labels=labels.WB_ATU,
                         limits=c(breaks.WB_ATU[1],
                                  breaks.WB_ATU[length(breaks.WB_ATU)]))+
    theme_minimal()+ # minimal theme
    scale_y_discrete(name=NULL, drop=F)+
    scale_x_discrete(name=NULL, drop=F)+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())+
    theme(legend.position="bottom",legend.title=element_blank(), legend.text = element_text(family="sans"))+
    coord_fixed()
  
  
  # MO_DE
  
  indx_MO_DE<-which(colnames(REP_wide) =='Monocytes|DE')
  
  summary_Beta_Z_indx_MO_DE<-summary(REP_wide[,indx_MO_DE])
  
  cat("summary_Beta_Z_indx_MO_DE\n")
  cat(sprintf(as.character(names(summary_Beta_Z_indx_MO_DE))))
  cat("\n")
  cat(sprintf(as.character(summary_Beta_Z_indx_MO_DE)))
  cat("\n")
  
  lower_tile_value<--1
  upper_tile_value<-1
  
  REP_wide[,indx_MO_DE][which(!is.na(REP_wide[,indx_MO_DE]) &
                                 REP_wide[,indx_MO_DE] < lower_tile_value )]<-lower_tile_value
  
  REP_wide[,indx_MO_DE][which(!is.na(REP_wide[,indx_MO_DE]) &
                                 REP_wide[,indx_MO_DE] > upper_tile_value )]<-upper_tile_value
  
  breaks.MO_DE<-seq(lower_tile_value,upper_tile_value,by=0.5)
  labels.MO_DE<-as.character(breaks.MO_DE)
  
  cat("labels.MO_DE_0\n")
  cat(str(labels.MO_DE))
  cat("\n")
  
  ### preparatory things
  
  REP_wide$MOCK_COORD<-1
  
  SELECTED_VAR_HGNC_MO_DE<-unique(REP$rs_HGNC[which(REP$RNASeq_source == 'Monocytes' & REP$Analysis == 'DE' & REP$Significance == 'YES')])
  
  cat("SELECTED_VAR_HGNC_MO_DE_0\n")
  cat(str(SELECTED_VAR_HGNC_MO_DE))
  cat("\n")
  
  
  REP_wide$Significance<-NA
  
  REP_wide$Significance[which(REP_wide$rs_HGNC%in%SELECTED_VAR_HGNC_MO_DE)]<-'YES'
  REP_wide$Significance[-which(REP_wide$rs_HGNC%in%SELECTED_VAR_HGNC_MO_DE)]<-'NO'
  
  REP_wide$Significance<-factor(REP_wide$Significance,
                                levels=c('NO','YES'),
                                ordered=T)
  
  cat("REP_wide_1\n")
  cat(str(REP_wide))
  cat("\n")
  cat(sprintf(as.character(names(summary(REP_wide$Significance)))))
  cat("\n")
  cat(sprintf(as.character(summary(REP_wide$Significance))))
  cat("\n")
  
  ggheatmap_MO_DE <-ggplot(data=REP_wide,
                            aes(x=MOCK_COORD, y=rs_HGNC, fill = REP_wide[,indx_MO_DE]))+
    geom_tile()+
    geom_tile(data=subset(REP_wide, Significance == 'YES'), fill = NA, color = "black", size = 1)+
    scale_fill_gradient2(name=paste("Beta","z scored","Monocytes DE", sep="\n"),
                         low = "blue", high = "red",mid="white",midpoint=0,
                         na.value = NA,
                         breaks=breaks.MO_DE,
                         labels=labels.MO_DE,
                         limits=c(breaks.MO_DE[1],
                                  breaks.MO_DE[length(breaks.MO_DE)]))+
    theme_minimal()+ # minimal theme
    scale_y_discrete(name=NULL, drop=F)+
    scale_x_discrete(name=NULL, drop=F)+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())+
    theme(legend.position="bottom",legend.title=element_blank(), legend.text = element_text(family="sans"))+
    coord_fixed()
  

  # MO_ATU
  
  indx_MO_ATU<-which(colnames(REP_wide) =='Monocytes|ATU')
  
  summary_Beta_Z_indx_MO_ATU<-summary(REP_wide[,indx_MO_ATU])
  
  cat("summary_Beta_Z_indx_MO_ATU\n")
  cat(sprintf(as.character(names(summary_Beta_Z_indx_MO_ATU))))
  cat("\n")
  cat(sprintf(as.character(summary_Beta_Z_indx_MO_ATU)))
  cat("\n")
  
  lower_tile_value<--1
  upper_tile_value<-1
  
  REP_wide[,indx_MO_ATU][which(!is.na(REP_wide[,indx_MO_ATU]) &
                                REP_wide[,indx_MO_ATU] < lower_tile_value )]<-lower_tile_value
  
  REP_wide[,indx_MO_ATU][which(!is.na(REP_wide[,indx_MO_ATU]) &
                                REP_wide[,indx_MO_ATU] > upper_tile_value )]<-upper_tile_value
  
  breaks.MO_ATU<-seq(lower_tile_value,upper_tile_value,by=0.5)
  labels.MO_ATU<-as.character(breaks.MO_ATU)
  
  cat("labels.MO_ATU_0\n")
  cat(str(labels.MO_ATU))
  cat("\n")
  
  ### preparatory things
  
  REP_wide$MOCK_COORD<-1
  
  SELECTED_VAR_HGNC_MO_ATU<-unique(REP$rs_HGNC[which(REP$RNASeq_source == 'Monocytes' & REP$Analysis == 'ATU' & REP$Significance == 'YES')])
  
  cat("SELECTED_VAR_HGNC_MO_ATU_0\n")
  cat(str(SELECTED_VAR_HGNC_MO_ATU))
  cat("\n")
  
  
  REP_wide$Significance<-NA
  
  REP_wide$Significance[which(REP_wide$rs_HGNC%in%SELECTED_VAR_HGNC_MO_ATU)]<-'YES'
  REP_wide$Significance[-which(REP_wide$rs_HGNC%in%SELECTED_VAR_HGNC_MO_ATU)]<-'NO'
  
  REP_wide$Significance<-factor(REP_wide$Significance,
                                levels=c('NO','YES'),
                                ordered=T)
  
  cat("REP_wide_1\n")
  cat(str(REP_wide))
  cat("\n")
  cat(sprintf(as.character(names(summary(REP_wide$Significance)))))
  cat("\n")
  cat(sprintf(as.character(summary(REP_wide$Significance))))
  cat("\n")
  
  ggheatmap_MO_ATU <-ggplot(data=REP_wide,
                           aes(x=MOCK_COORD, y=rs_HGNC, fill = REP_wide[,indx_MO_ATU]))+
    geom_tile()+
    geom_tile(data=subset(REP_wide, Significance == 'YES'), fill = NA, color = "black", size = 1)+
    scale_fill_gradient2(name=paste("Beta","z scored","Monocytes DE", sep="\n"),
                         low = "blue", high = "red",mid="white",midpoint=0,
                         na.value = NA,
                         breaks=breaks.MO_ATU,
                         labels=labels.MO_ATU,
                         limits=c(breaks.MO_ATU[1],
                                  breaks.MO_ATU[length(breaks.MO_ATU)]))+
    theme_minimal()+ # minimal theme
    scale_y_discrete(name=NULL, drop=F)+
    scale_x_discrete(name=NULL, drop=F)+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())+
    theme(legend.position="bottom",legend.title=element_blank(), legend.text = element_text(family="sans"))+
    coord_fixed()
  
 
  # NF_DE
  
  indx_NF_DE<-which(colnames(REP_wide) =='Neutrophils|DE')
  
  summary_Beta_Z_indx_NF_DE<-summary(REP_wide[,indx_NF_DE])
  
  cat("summary_Beta_Z_indx_NF_DE\n")
  cat(sprintf(as.character(names(summary_Beta_Z_indx_NF_DE))))
  cat("\n")
  cat(sprintf(as.character(summary_Beta_Z_indx_NF_DE)))
  cat("\n")
  
  lower_tile_value<--1
  upper_tile_value<-1
  
  REP_wide[,indx_NF_DE][which(!is.na(REP_wide[,indx_NF_DE]) &
                                 REP_wide[,indx_NF_DE] < lower_tile_value )]<-lower_tile_value
  
  REP_wide[,indx_NF_DE][which(!is.na(REP_wide[,indx_NF_DE]) &
                                 REP_wide[,indx_NF_DE] > upper_tile_value )]<-upper_tile_value
  
  breaks.NF_DE<-seq(lower_tile_value,upper_tile_value,by=0.5)
  labels.NF_DE<-as.character(breaks.NF_DE)
  
  cat("labels.NF_DE_0\n")
  cat(str(labels.NF_DE))
  cat("\n")
  
  ### preparatory things
  
  REP_wide$MOCK_COORD<-1
  
  SELECTED_VAR_HGNC_NF_DE<-unique(REP$rs_HGNC[which(REP$RNASeq_source == 'Neutrophils' & REP$Analysis == 'DE' & REP$Significance == 'YES')])
  
  cat("SELECTED_VAR_HGNC_NF_DE_0\n")
  cat(str(SELECTED_VAR_HGNC_NF_DE))
  cat("\n")
  
  
  REP_wide$Significance<-NA
  
  REP_wide$Significance[which(REP_wide$rs_HGNC%in%SELECTED_VAR_HGNC_NF_DE)]<-'YES'
  REP_wide$Significance[-which(REP_wide$rs_HGNC%in%SELECTED_VAR_HGNC_NF_DE)]<-'NO'
  
  REP_wide$Significance<-factor(REP_wide$Significance,
                                levels=c('NO','YES'),
                                ordered=T)
  
  cat("REP_wide_1\n")
  cat(str(REP_wide))
  cat("\n")
  cat(sprintf(as.character(names(summary(REP_wide$Significance)))))
  cat("\n")
  cat(sprintf(as.character(summary(REP_wide$Significance))))
  cat("\n")
  
  ggheatmap_NF_DE <-ggplot(data=REP_wide,
                            aes(x=MOCK_COORD, y=rs_HGNC, fill = REP_wide[,indx_NF_DE]))+
    geom_tile()+
    geom_tile(data=subset(REP_wide, Significance == 'YES'), fill = NA, color = "black", size = 1)+
    scale_fill_gradient2(name=paste("Beta","z scored","Neutrophils DE", sep="\n"),
                         low = "blue", high = "red",mid="white",midpoint=0,
                         na.value = NA,
                         breaks=breaks.NF_DE,
                         labels=labels.NF_DE,
                         limits=c(breaks.NF_DE[1],
                                  breaks.NF_DE[length(breaks.NF_DE)]))+
    theme_minimal()+ # minimal theme
    scale_y_discrete(name=NULL, drop=F)+
    scale_x_discrete(name=NULL, drop=F)+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(family="sans"))+
    theme(legend.position="bottom",legend.title=element_blank(), legend.text = element_text(family="sans"))+
    coord_fixed()
  
 
  # NF_ATU
  
  indx_NF_ATU<-which(colnames(REP_wide) =='Neutrophils|ATU')
  
  summary_Beta_Z_indx_NF_ATU<-summary(REP_wide[,indx_NF_ATU])
  
  cat("summary_Beta_Z_indx_NF_ATU\n")
  cat(sprintf(as.character(names(summary_Beta_Z_indx_NF_ATU))))
  cat("\n")
  cat(sprintf(as.character(summary_Beta_Z_indx_NF_ATU)))
  cat("\n")
  
  lower_tile_value<--1
  upper_tile_value<-1
  
  REP_wide[,indx_NF_ATU][which(!is.na(REP_wide[,indx_NF_ATU]) &
                                REP_wide[,indx_NF_ATU] < lower_tile_value )]<-lower_tile_value
  
  REP_wide[,indx_NF_ATU][which(!is.na(REP_wide[,indx_NF_ATU]) &
                                REP_wide[,indx_NF_ATU] > upper_tile_value )]<-upper_tile_value
  
  breaks.NF_ATU<-seq(lower_tile_value,upper_tile_value,by=0.5)
  labels.NF_ATU<-as.character(breaks.NF_ATU)
  
  cat("labels.NF_ATU_0\n")
  cat(str(labels.NF_ATU))
  cat("\n")
  
  ### preparatory things
  
  REP_wide$MOCK_COORD<-1
  
  SELECTED_VAR_HGNC_NF_ATU<-unique(REP$rs_HGNC[which(REP$RNASeq_source == 'Neutrophils' & REP$Analysis == 'ATU' & REP$Significance == 'YES')])
  
  cat("SELECTED_VAR_HGNC_NF_ATU_0\n")
  cat(str(SELECTED_VAR_HGNC_NF_ATU))
  cat("\n")
  
  
  REP_wide$Significance<-NA
  
  REP_wide$Significance[which(REP_wide$rs_HGNC%in%SELECTED_VAR_HGNC_NF_ATU)]<-'YES'
  REP_wide$Significance[-which(REP_wide$rs_HGNC%in%SELECTED_VAR_HGNC_NF_ATU)]<-'NO'
  
  REP_wide$Significance<-factor(REP_wide$Significance,
                                levels=c('NO','YES'),
                                ordered=T)
  
  cat("REP_wide_1\n")
  cat(str(REP_wide))
  cat("\n")
  cat(sprintf(as.character(names(summary(REP_wide$Significance)))))
  cat("\n")
  cat(sprintf(as.character(summary(REP_wide$Significance))))
  cat("\n")
  
  ggheatmap_NF_ATU <-ggplot(data=REP_wide,
                           aes(x=MOCK_COORD, y=rs_HGNC, fill = REP_wide[,indx_NF_ATU]))+
    geom_tile()+
    geom_tile(data=subset(REP_wide, Significance == 'YES'), fill = NA, color = "black", size = 1)+
    scale_fill_gradient2(name=paste("Beta","z scored","Neutrophils ATU", sep="\n"),
                         low = "blue", high = "red",mid="white",midpoint=0,
                         na.value = NA,
                         breaks=breaks.NF_ATU,
                         labels=labels.NF_ATU,
                         limits=c(breaks.NF_ATU[1],
                                  breaks.NF_ATU[length(breaks.NF_ATU)]))+
    theme_minimal()+ # minimal theme
    scale_y_discrete(name=NULL, drop=F)+
    scale_x_discrete(name=NULL, drop=F)+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())+
    theme(legend.position="bottom",legend.title=element_blank(), legend.text = element_text(family="sans"))+
    coord_fixed()
  
 
  
  # TCELL_DE
  
  indx_TCELL_DE<-which(colnames(REP_wide) =='naive T-CD4 Cells|DE')
  
  summary_Beta_Z_indx_TCELL_DE<-summary(REP_wide[,indx_TCELL_DE])
  
  cat("summary_Beta_Z_indx_TCELL_DE\n")
  cat(sprintf(as.character(names(summary_Beta_Z_indx_TCELL_DE))))
  cat("\n")
  cat(sprintf(as.character(summary_Beta_Z_indx_TCELL_DE)))
  cat("\n")
  
  lower_tile_value<--1
  upper_tile_value<-1
  
  REP_wide[,indx_TCELL_DE][which(!is.na(REP_wide[,indx_TCELL_DE]) &
                                 REP_wide[,indx_TCELL_DE] < lower_tile_value )]<-lower_tile_value
  
  REP_wide[,indx_TCELL_DE][which(!is.na(REP_wide[,indx_TCELL_DE]) &
                                 REP_wide[,indx_TCELL_DE] > upper_tile_value )]<-upper_tile_value
  
  breaks.TCELL_DE<-seq(lower_tile_value,upper_tile_value,by=0.5)
  labels.TCELL_DE<-as.character(breaks.TCELL_DE)
  
  cat("labels.TCELL_DE_0\n")
  cat(str(labels.TCELL_DE))
  cat("\n")
  
  ### preparatory things
  
  REP_wide$MOCK_COORD<-1
  
  SELECTED_VAR_HGNC_TCELL_DE<-unique(REP$rs_HGNC[which(REP$RNASeq_source == 'naive T-CD4 Cells' & 
                                                          REP$Analysis == 'DE' & 
                                                          REP$Significance == 'YES')])
  
  cat("SELECTED_VAR_HGNC_TCELL_DE_0\n")
  cat(str(SELECTED_VAR_HGNC_TCELL_DE))
  cat("\n")
  
  
  REP_wide$Significance<-NA
  
  REP_wide$Significance[which(REP_wide$rs_HGNC%in%SELECTED_VAR_HGNC_TCELL_DE)]<-'YES'
  REP_wide$Significance[-which(REP_wide$rs_HGNC%in%SELECTED_VAR_HGNC_TCELL_DE)]<-'NO'
  
  REP_wide$Significance<-factor(REP_wide$Significance,
                                levels=c('NO','YES'),
                                ordered=T)
  
  cat("REP_wide_1\n")
  cat(str(REP_wide))
  cat("\n")
  cat(sprintf(as.character(names(summary(REP_wide$Significance)))))
  cat("\n")
  cat(sprintf(as.character(summary(REP_wide$Significance))))
  cat("\n")
  
  ggheatmap_TCELL_DE <-ggplot(data=REP_wide,
                            aes(x=MOCK_COORD, y=rs_HGNC, fill = REP_wide[,indx_TCELL_DE]))+
    geom_tile()+
    geom_tile(data=subset(REP_wide, Significance == 'YES'), fill = NA, color = "black", size = 1)+
    scale_fill_gradient2(name=paste("Beta","z scored","naive T-CD4 Cells DE", sep="\n"),
                         low = "blue", high = "red",mid="white",midpoint=0,
                         na.value = NA,
                         breaks=breaks.TCELL_DE,
                         labels=labels.TCELL_DE,
                         limits=c(breaks.TCELL_DE[1],
                                  breaks.TCELL_DE[length(breaks.TCELL_DE)]))+
    theme_minimal()+ # minimal theme
    scale_y_discrete(name=NULL, drop=F)+
    scale_x_discrete(name=NULL, drop=F)+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())+
    theme(legend.position="bottom",legend.title=element_blank(), legend.text = element_text(family="sans"))+
    coord_fixed()
  
  
  # TCELL_ATU
  
  indx_TCELL_ATU<-which(colnames(REP_wide) =='naive T-CD4 Cells|ATU')
  
  summary_Beta_Z_indx_TCELL_ATU<-summary(REP_wide[,indx_TCELL_ATU])
  
  cat("summary_Beta_Z_indx_TCELL_ATU\n")
  cat(sprintf(as.character(names(summary_Beta_Z_indx_TCELL_ATU))))
  cat("\n")
  cat(sprintf(as.character(summary_Beta_Z_indx_TCELL_ATU)))
  cat("\n")
  
  lower_tile_value<--1
  upper_tile_value<-1
  
  REP_wide[,indx_TCELL_ATU][which(!is.na(REP_wide[,indx_TCELL_ATU]) &
                                 REP_wide[,indx_TCELL_ATU] < lower_tile_value )]<-lower_tile_value
  
  REP_wide[,indx_TCELL_ATU][which(!is.na(REP_wide[,indx_TCELL_ATU]) &
                                 REP_wide[,indx_TCELL_ATU] > upper_tile_value )]<-upper_tile_value
  
  breaks.TCELL_ATU<-seq(lower_tile_value,upper_tile_value,by=0.5)
  labels.TCELL_ATU<-as.character(breaks.TCELL_ATU)
  
  cat("labels.TCELL_ATU_0\n")
  cat(str(labels.TCELL_ATU))
  cat("\n")
  
  ### preparatory things
  
  REP_wide$MOCK_COORD<-1
  
  SELECTED_VAR_HGNC_TCELL_ATU<-unique(REP$rs_HGNC[which(REP$RNASeq_source == 'naive T-CD4 Cells' & 
                                                          REP$Analysis == 'ATU' & 
                                                          REP$Significance == 'YES')])
  
  cat("SELECTED_VAR_HGNC_TCELL_ATU_0\n")
  cat(str(SELECTED_VAR_HGNC_TCELL_ATU))
  cat("\n")
  
  
  REP_wide$Significance<-NA
  
  REP_wide$Significance[which(REP_wide$rs_HGNC%in%SELECTED_VAR_HGNC_TCELL_ATU)]<-'YES'
  REP_wide$Significance[-which(REP_wide$rs_HGNC%in%SELECTED_VAR_HGNC_TCELL_ATU)]<-'NO'
  
  REP_wide$Significance<-factor(REP_wide$Significance,
                                levels=c('NO','YES'),
                                ordered=T)
  
  cat("REP_wide_1\n")
  cat(str(REP_wide))
  cat("\n")
  cat(sprintf(as.character(names(summary(REP_wide$Significance)))))
  cat("\n")
  cat(sprintf(as.character(summary(REP_wide$Significance))))
  cat("\n")
  
  ggheatmap_TCELL_ATU <-ggplot(data=REP_wide,
                              aes(x=MOCK_COORD, y=rs_HGNC, fill = REP_wide[,indx_TCELL_ATU]))+
    geom_tile()+
    geom_tile(data=subset(REP_wide, Significance == 'YES'), fill = NA, color = "black", size = 1)+
    scale_fill_gradient2(name=paste("Beta","z scored","naive T-CD4 Cells ATU", sep="\n"),
                         low = "blue", high = "red",mid="white",midpoint=0,
                         na.value = NA,
                         breaks=breaks.TCELL_ATU,
                         labels=labels.TCELL_ATU,
                         limits=c(breaks.TCELL_ATU[1],
                                  breaks.TCELL_ATU[length(breaks.TCELL_ATU)]))+
    theme_minimal()+ # minimal theme
    scale_y_discrete(name=NULL, drop=F)+
    scale_x_discrete(name=NULL, drop=F)+
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank())+
    theme(legend.position="bottom",legend.title=element_blank(), legend.text = element_text(family="sans"))+
    coord_fixed()
  
  graph_DEF_partI<-plot_grid(ggheatmap_WB_DE, 
                             ggheatmap_WB_ATU,
                             ggheatmap_MO_DE,
                             ggheatmap_MO_ATU,
                             ncol = 4,
                             rel_widths=c(1,0.25,0.25,0.25))
  
  setwd(out)
  
  svgname<-paste("test_partI",".svg", sep='')
  
  
  ggsave(svgname, plot= graph_DEF_partI,
         device="svg",
         height=13, width=13)
  
  graph_DEF_partII<-plot_grid(ggheatmap_NF_DE,
                              ggheatmap_NF_ATU,
                              ggheatmap_TCELL_DE,
                              ggheatmap_TCELL_ATU,
                              ncol = 4,
                              rel_widths=c(1,0.25,0.25,0.25))
  
  
  svgname<-paste("test_partII",".svg", sep='')
  
  
  ggsave(svgname, plot= graph_DEF_partII,
         device="svg",
         height=13, width=13)

}

heatmap_discretized = function(option_list)
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
  
  
  #### Read REP_long ----

  setwd(out)
  

  REP_long<-readRDS(file='REP_DE_ATU_hetmap_table.rds')
  
  cat("REP_long_0\n")
  cat(str(REP_long))
  cat("\n")
  
  
  # REP_long$interaction<-interaction(REP_long$RNASeq_source,
  #                                   REP_long$Analysis,
  #                                   sep='|')
  
  
  cat("REP_long_1\n")
  cat(str(REP_long))
  cat("\n")
  
  
  REP_long$Y1<-cut(REP_long$Beta_Z_score,breaks = c(-6,-1,-0.75,-0.5,-0.25,-0.1,-0.05,-0.025,-0.01,0,
                                                        0.01,0.025,0.05,0.1,0.25,0.5,0.75,1,35),right = FALSE)
  
  cat("REP_long_1\n")
  cat(str(REP_long))
  cat("\n")
  cat(sprintf(as.character(names(summary(REP_long$Y1)))))
  cat("\n")
  cat(sprintf(as.character(summary(REP_long$Y1))))
  cat("\n")
  
  vector_fill<-c(rev(brewer.pal(9, "Blues")),brewer.pal(9, "Reds"))
  
  cat("vector_fill_0\n")
  cat(str(vector_fill))
  cat("\n")
  
  # geom_tile(data=subset(REP_long, Significance == 'YES'), fill = NA, color = "black", size = 1)+
  # REP_long %>%
  #   mutate(myaxis = paste0(rs," ","(", HGNC,")"), drop=F) %>%
  #   mutate(myaxis=fct_reorder(myaxis,as.numeric(rs_HGNC)), drop=F) %>%
  
  heatmap_RNA <-ggplot(data=REP_long,
           aes(y=reorder(paste(rs," ","(", HGNC,")",sep=''),as.numeric(rs_HGNC)),x=Analysis,fill=Y1))+
    geom_tile()+
    geom_tile(data=subset(REP_long, Significance == 'NO'), fill = NA, color = "black", size = 0.1)+
    geom_tile(data=subset(REP_long, Significance == 'YES'), fill = NA, color = "black", size = 1)+
    scale_fill_manual(name=paste("Effect","size","z-score", sep="\n"),
                      values=vector_fill)
  
  
  
  
  
  heatmap_RNA <-heatmap_RNA+
    theme_cowplot(font_size = 1)+
    facet_grid(. ~ RNASeq_source , scales='free_x', space='free_x', switch="y")+
    theme_minimal()+ # minimal theme
    scale_y_discrete(name=NULL, drop=T)+
    scale_x_discrete(name=NULL, drop=T)+
    theme(plot.title=element_blank(),
          axis.title=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_text(color="black", family="sans", face="italic"),
          axis.text.x=element_text(angle=45,color="black", family="sans"))+
    theme(legend.title = element_text(family="sans"),
          legend.text = element_text(family="sans"),
          legend.key.size = unit(0.35, 'cm'), #change legend key size
          legend.key.height = unit(0.35, 'cm'), #change legend key height
          legend.key.width = unit(0.35, 'cm'), #change legend key width
          legend.position="bottom")+
  ggeasy::easy_center_title()
  
  
  
  setwd(out)
  
  svgname<-paste("heatmap_RNA_discretized",".svg", sep='')
  
  
  ggsave(svgname, plot= heatmap_RNA,
         device="svg",
         width=4, height=13)
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
    make_option(c("--Table_S7"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Table_S6"), type="numeric", default=NULL, 
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
  
  data_wrangling_and_heatmap(opt)
  heatmap_discretized(opt)
  
  
}


###########################################################################

system.time( main() )