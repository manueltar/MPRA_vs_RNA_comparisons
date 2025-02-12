
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
suppressMessages(library("VennDiagram", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("nVennR", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("ggupset", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))


opt = NULL

options(warn = 1)

Convert_up <- function(TB){
  
  # from https://karobben.github.io/2021/03/31/R/ggupset/ and https://github.com/const-ae/ggupset
  
  TB_tmp = TB
  
  cat("TB_tmp_initial\n")
  cat(str(TB_tmp))
  cat("\n")
  
  for(i in colnames(TB)){
    # cat("-------------------------->i\t")
    # cat(sprintf(as.character(i)))
    # cat("\n")
    
    TB_tmp[i] = i
    
    # cat("TB_tmp_0\n")
    # cat(str(TB_tmp))
    # cat("\n")
    
  }
  TB_tmp[TB == 0] = ""
  
  # cat("TB_tmp_1\n")
  # cat(str(TB_tmp))
  # cat("\n")
  
  TB_t <- data.frame(t(TB_tmp), stringsAsFactors = F)
  
  # cat("TB_t_0\n")
  # cat(str(TB_t))
  # cat("\n")
  
  TB_tmp$upset = as.list(TB_t)
  
  # cat("TB_tmp_FIN\n")
  # cat(str(TB_tmp))
  # cat("\n")
  
  return(TB_tmp)
}

upsetr_and_venn_preprint = function(option_list)
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
  
  path_graphs<-paste(out,'graphs','/',sep='')
  
  if(file.exists(path_graphs)){
    
  }else{
    dir.create(file.path(path_graphs))
  }#path_graphs
  
  
  #### READ Table_S6 ----
  
  Table_S6<-readRDS(file=opt$Table_S6)
  
  
  # Table_S6<-droplevels(Table_S6[-which(Table_S6$Mechanistic_Class == 'No_RNA_Seq_HET_carriers'),])
  
  cat("Table_S6_0\n")
  cat(str(Table_S6))
  cat("\n")
  cat(str(unique(Table_S6$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S6$MPRA_CLASS)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S6$MPRA_CLASS))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S6$Mechanistic_Class)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S6$Mechanistic_Class))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S6$Manual_curation)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S6$Manual_curation))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S6$Multi_Lineage)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S6$Multi_Lineage))))
  cat("\n")
  
  levels_MPRA_CLASS<-rev(levels(Table_S6$MPRA_CLASS))
  
  cat("levels_MPRA_CLASS\n")
  cat(str(levels_MPRA_CLASS))
  cat("\n")
  
  Table_S6$MPRA_CLASS<-factor(Table_S6$MPRA_CLASS,
                              levels=levels_MPRA_CLASS,
                              ordered=T)
  
  
  
  levels_Mechanistic_Class<-levels(Table_S6$Mechanistic_Class)
  
  cat("levels_Mechanistic_Class\n")
  cat(str(levels_Mechanistic_Class))
  cat("\n")
  
  levels_Manual_curation<-levels(Table_S6$Manual_curation)
  
  cat("levels_Manual_curation\n")
  cat(str(levels_Manual_curation))
  cat("\n")
  
  levels_Multi_Lineage<-levels(Table_S6$Multi_Lineage)
  
  cat("levels_Multi_Lineage\n")
  cat(str(levels_Multi_Lineage))
  cat("\n")
  
  pool_levels<-levels_Mechanistic_Class
  
  cat("pool_levels_0\n")
  cat(str(pool_levels))
  cat("\n")
  
  
  Table_S6$Mechanistic_Class_compressed<-NA
  
  Table_S6$Mechanistic_Class_compressed[which(Table_S6$Mechanistic_Class%in%pool_levels[c(1:3)])]<-'DE and or ATU'
  Table_S6$Mechanistic_Class_compressed[which(Table_S6$Mechanistic_Class%in%pool_levels[4])]<-pool_levels[4]
  
  Table_S6$Mechanistic_Class_compressed<-factor(Table_S6$Mechanistic_Class_compressed,
                                                levels=c('DE and or ATU',pool_levels[4]),
                                                ordered=T)
  
  cat(sprintf(as.character(names(summary(Table_S6$Mechanistic_Class_compressed)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S6$Mechanistic_Class_compressed))))
  cat("\n")
  
  
  
  
  Table_S6$interaction<-droplevels(interaction(Table_S6$MPRA_CLASS,Table_S6$Mechanistic_Class_compressed, sep='|'))


  pool_levels_interaction<-levels(Table_S6$interaction)

  cat("Table_S6$interaction_0\n")
  cat(str(pool_levels_interaction))
  cat("\n")

  cat(sprintf(as.character(names(summary(Table_S6$interaction)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S6$interaction))))
  cat("\n")
  
  
  #### Freq by mech class ----
  
  Table_S6<-droplevels(Table_S6[which(Table_S6$Mechanistic_Class != "No_RNA_Seq_HET_carriers"),])
  
  
  DEBUG <- 1
  
  Table_S6.dt<-data.table(Table_S6, key=c('interaction','Mechanistic_Class'))
  
  
  Freq_instances<-as.data.frame(Table_S6.dt[,.(instances=.N),by=key(Table_S6.dt)], stringsAsFactors=F)
  
  if(DEBUG == 1)
  {
    cat("Freq_instances_0\n")
    cat(str(Freq_instances))
    cat("\n")
    #quit(status = 1)
  }
  
  Table_S6.dt<-data.table(Table_S6, key=c("interaction"))
  
  Freq_TOTAL_interaction<-as.data.frame(Table_S6.dt[,.(TOTAL=.N),by=key(Table_S6.dt)], stringsAsFactors=F)
  

  if(DEBUG == 1)
  {
    cat("Freq_TOTAL_interaction_0\n")
    cat(str(Freq_TOTAL_interaction))
    cat("\n")
    #quit(status = 1)
  }
  
  
  Table_S6.dt<-data.table(Table_S6, key=c("MPRA_CLASS"))
  
  Freq_TOTAL_MPRA<-as.data.frame(Table_S6.dt[,.(TOTAL=.N),by=key(Table_S6.dt)], stringsAsFactors=F)
  
  colnames(Freq_TOTAL_MPRA)[which(colnames(Freq_TOTAL_MPRA) == 'MPRA_CLASS')]<-'CLASS'
  
  if(DEBUG == 1)
  {
    cat("Freq_TOTAL_MPRA_0\n")
    cat(str(Freq_TOTAL_MPRA))
    cat("\n")
    #quit(status = 1)
  }
  
  Table_S6.dt<-data.table(Table_S6, key=c("Mechanistic_Class_compressed"))
  
  Freq_TOTAL_Mechanistic_Class_compressed<-as.data.frame(Table_S6.dt[,.(TOTAL=.N),by=key(Table_S6.dt)], stringsAsFactors=F)
  
  colnames(Freq_TOTAL_Mechanistic_Class_compressed)[which(colnames(Freq_TOTAL_Mechanistic_Class_compressed) == 'Mechanistic_Class_compressed')]<-'CLASS'
  
  if(DEBUG == 1)
  {
    cat("Freq_TOTAL_Mechanistic_Class_compressed_0\n")
    cat(str(Freq_TOTAL_Mechanistic_Class_compressed))
    cat("\n")
    #quit(status = 1)
  }
  
 
  
  
  Freq_instances<-merge(Freq_instances,
                        Freq_TOTAL_interaction,
                        by="interaction")
  
  if(DEBUG == 1)
  {
    cat("Freq_instances_1\n")
    cat(str(Freq_instances))
    cat("\n")
    #quit(status = 1)
  }
  
  Freq_instances$Perc<-round(100*(Freq_instances$instances/Freq_instances$TOTAL),2)
  
  if(DEBUG == 1)
  {
    cat("Freq_instances_2\n")
    cat(str(Freq_instances))
    cat("\n")
    #quit(status = 1)
  }
  
  breaks.Rank<-unique(sort(c(0,100,seq(0,100, by=25))))
  labels.Rank<-as.character(breaks.Rank)
  
  if(DEBUG == 1)
  {
    cat("labels.Rank\n")
    cat(str(labels.Rank))
    cat("\n")
    #quit(status = 1)
  }
  
  vector.fill<-c(brewer.pal(length(levels(Freq_instances$Mechanistic_Class)), "Dark2"))
  
  if(DEBUG == 1)
  {
    cat("vector.fill_0\n")
    cat(str(vector.fill))
    cat("\n")
    #quit(status = 1)
  }
  
  upset<-Freq_instances %>%
    mutate(myaxis = paste0(interaction,"\n","n=", TOTAL), drop=T) %>%
    mutate(myaxis=fct_reorder(myaxis,as.numeric(interaction)), drop=F) %>%
    ggplot(aes(x=myaxis,
                    y=Perc))+
    geom_hline(yintercept=breaks.Rank, size=0.25, linetype="dashed", color='gray')+
    geom_bar(aes(fill=Mechanistic_Class), stat="identity",color='white')+
    scale_y_continuous(name=paste('Percentage of variants'),breaks=breaks.Rank,labels=labels.Rank,
                       limits=c(breaks.Rank[1],breaks.Rank[length(breaks.Rank)]), expand = c(0.01, 0.01))+
    scale_x_discrete(name=NULL, drop=F)
  
  
  upset<-upset+
    theme_classic()+
    theme(axis.title.y=element_text(size=8, color="black", family="sans"),
          axis.title.x=element_text(size=8, color="black", family="sans"),
          axis.text.y=element_text(size=6, color="black", family="sans"),
          axis.text.x=element_text(angle=45,hjust=1,size=4, color="black", family="sans"),
          axis.line.x = element_line(size = 0.2),
          axis.ticks.x = element_line(size = 0.2),
          axis.ticks.y = element_line(size = 0.2),
          axis.line.y = element_line(size = 0.2))+
    scale_fill_manual(values=vector.fill, drop=F)+
    theme(legend.title = element_blank(),
          legend.text = element_text(size=6),
          legend.key.size = unit(0.25, 'cm'), #change legend key size
          legend.key.height = unit(0.25, 'cm'), #change legend key height
          legend.key.width = unit(0.25, 'cm'), #change legend key width
          legend.position="right")
  
  
  subgraph_df<-rbind(Freq_TOTAL_MPRA,
                     Freq_TOTAL_Mechanistic_Class_compressed)
  
  levels_CLASS<-subgraph_df$CLASS
  
  subgraph_df$CLASS<-factor(subgraph_df$CLASS,
                            levels=rev(levels_CLASS),
                            ordered=T)
  if(DEBUG == 1)
  {
    cat("subgraph_df_0\n")
    cat(str(subgraph_df))
    cat("\n")
    #quit(status = 1)
  }
  
 
  A<-summary(subgraph_df$TOTAL)
  
  breaks.Rank<-unique(sort(c(0,max(A),seq(0,max(A), by=10))))
  # breaks.Rank<-unique(sort(c(0,100,seq(0,100, by=25))))
  labels.Rank<-as.character(breaks.Rank)
  
  if(DEBUG == 1)
  {
    cat("labels.Rank\n")
    cat(str(labels.Rank))
    cat("\n")
    #quit(status = 1)
  }
  
  vector_colors_INTIAL<-brewer.pal(8, "Paired")
  
  
  vector_colors_DEF<-c(vector_colors_INTIAL[3:4],vector_colors_INTIAL[5:6],vector_colors_INTIAL[1:2])
  
  
  
  subgraph<-ggplot(data=subgraph_df,
                   aes(y=CLASS,
                       x=TOTAL,
                       fill=CLASS)) +
    geom_vline(xintercept=breaks.Rank, size=0.25, linetype="dashed", color='gray')+
    geom_bar(stat="identity",colour='white')+
    scale_fill_manual(values=vector_colors_DEF, drop=F)
  
  
  
  
  subgraph<-subgraph+
    theme_classic()+
    theme(axis.title.y=element_text(size=8, color="black", family="sans"),
          axis.title.x=element_text(size=8, color="black", family="sans"),
          axis.text.y=element_text(size=6, color="black", family="sans"),
          axis.text.x=element_text(size=6, color="black", family="sans"),
          axis.line.x = element_line(size = 0.2),
          axis.ticks.x = element_line(size = 0.2),
          axis.ticks.y = element_line(size = 0.2),
          axis.line.y = element_line(size = 0.2))+
    scale_x_reverse(name='Number of variants',breaks=breaks.Rank,labels=labels.Rank)+
    scale_y_discrete(position="right",name=NULL, drop=F)+
    theme(legend.title = element_text(size=6),
          legend.text = element_text(size=6),
          legend.key.size = unit(0.25, 'cm'), #change legend key size
          legend.key.height = unit(0.25, 'cm'), #change legend key height
          legend.key.width = unit(0.25, 'cm'), #change legend key width
          legend.position="hidden")+
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  
  # limits=c(breaks.Rank[1],breaks.Rank[length(breaks.Rank)])
  
  if(DEBUG ==1)
  {
    cat("subgraph DONE\n")
  }
  
  graph_FINAL<-plot_grid(NULL,upset,subgraph,NULL,
                         nrow = 2,
                         ncol=2,
                         rel_heights = c(0.75, 0.5),
                         rel_widths=c(0.5,1))
  
  setwd(path_graphs)
  
  svglite(paste('Modalities_UpsetR_with_subgraph_preprint_analysis','.svg',sep=''), width = 8, height = 8)
  print(graph_FINAL)
  dev.off()
  

 
}


upsetr_and_venn_now = function(option_list)
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
  
  path_graphs<-paste(out,'graphs','/',sep='')
  
  if(file.exists(path_graphs)){
    
  }else{
    dir.create(file.path(path_graphs))
  }#path_graphs
  
  #### READ NEW_Table_S6 ----
  
  NEW_Table_S6<-readRDS(file=opt$NEW_Table_S6)
  
  
  # NEW_Table_S6<-droplevels(NEW_Table_S6[-which(NEW_Table_S6$Mechanistic_Class == 'No_RNA_Seq_HET_carriers'),])
  
  cat("NEW_Table_S6_0\n")
  cat(str(NEW_Table_S6))
  cat("\n")
  cat(str(unique(NEW_Table_S6$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(NEW_Table_S6$MPRA_CLASS)))))
  cat("\n")
  cat(sprintf(as.character(summary(NEW_Table_S6$MPRA_CLASS))))
  cat("\n")
  cat(sprintf(as.character(names(summary(NEW_Table_S6$Mechanistic_Class)))))
  cat("\n")
  cat(sprintf(as.character(summary(NEW_Table_S6$Mechanistic_Class))))
  cat("\n")
  cat(sprintf(as.character(names(summary(NEW_Table_S6$Manual_curation)))))
  cat("\n")
  cat(sprintf(as.character(summary(NEW_Table_S6$Manual_curation))))
  cat("\n")
  cat(sprintf(as.character(names(summary(NEW_Table_S6$Multi_Lineage)))))
  cat("\n")
  cat(sprintf(as.character(summary(NEW_Table_S6$Multi_Lineage))))
  cat("\n")
  
  
  
  check.VARs<-NEW_Table_S6[which(NEW_Table_S6$VAR%in%c("chr20_55990370_A_T","chr15_65174438_C_A","chr15_65174494_A_G")),]
  
  cat("check.VARs_3\n")
  cat(str(check.VARs))
  cat("\n")
  
  
  levels_MPRA_CLASS<-rev(levels(NEW_Table_S6$MPRA_CLASS))
  
  cat("levels_MPRA_CLASS\n")
  cat(str(levels_MPRA_CLASS))
  cat("\n")
  
  NEW_Table_S6$MPRA_CLASS<-factor(NEW_Table_S6$MPRA_CLASS,
                              levels=levels_MPRA_CLASS,
                              ordered=T)
  
 
  
  levels_Mechanistic_Class<-levels(NEW_Table_S6$Mechanistic_Class)
  
  cat("levels_Mechanistic_Class\n")
  cat(str(levels_Mechanistic_Class))
  cat("\n")
  
  levels_Manual_curation<-levels(NEW_Table_S6$Manual_curation)
  
  cat("levels_Manual_curation\n")
  cat(str(levels_Manual_curation))
  cat("\n")
  
  levels_Multi_Lineage<-levels(NEW_Table_S6$Multi_Lineage)
  
  cat("levels_Multi_Lineage\n")
  cat(str(levels_Multi_Lineage))
  cat("\n")
  
  pool_levels<-levels_Mechanistic_Class
  
  cat("pool_levels_0\n")
  cat(str(pool_levels))
  cat("\n")
  
  
 
  NEW_Table_S6$interaction<-droplevels(interaction(NEW_Table_S6$MPRA_CLASS,NEW_Table_S6$Mechanistic_Class_compressed, sep='|'))
  
  
  pool_levels_interaction<-levels(NEW_Table_S6$interaction)
  
  cat("NEW_Table_S6$interaction_0\n")
  cat(str(pool_levels_interaction))
  cat("\n")
  
  cat(sprintf(as.character(names(summary(NEW_Table_S6$interaction)))))
  cat("\n")
  cat(sprintf(as.character(summary(NEW_Table_S6$interaction))))
  cat("\n")
  
  
  setwd(out)
  
  saveRDS(NEW_Table_S6, file="NEW_Table_S6_with_interaction.rds")
  
  
  #### Freq by mech class ----
  
  NEW_Table_S6<-droplevels(NEW_Table_S6[which(NEW_Table_S6$Mechanistic_Class != "No_RNA_Seq_HET_carriers"),])
  
  DEBUG <- 1
  
  NEW_Table_S6.dt<-data.table(NEW_Table_S6, key=c('interaction','Mechanistic_Class'))
  
  
  Freq_instances<-as.data.frame(NEW_Table_S6.dt[,.(instances=.N),by=key(NEW_Table_S6.dt)], stringsAsFactors=F)
  
  if(DEBUG == 1)
  {
    cat("Freq_instances_0\n")
    cat(str(Freq_instances))
    cat("\n")
    #quit(status = 1)
  }
  
  NEW_Table_S6.dt<-data.table(NEW_Table_S6, key=c("interaction"))
  
  Freq_TOTAL_interaction<-as.data.frame(NEW_Table_S6.dt[,.(TOTAL=.N),by=key(NEW_Table_S6.dt)], stringsAsFactors=F)
  
  
  if(DEBUG == 1)
  {
    cat("Freq_TOTAL_interaction_0\n")
    cat(str(Freq_TOTAL_interaction))
    cat("\n")
    #quit(status = 1)
  }
  
  
  NEW_Table_S6.dt<-data.table(NEW_Table_S6, key=c("MPRA_CLASS"))
  
  Freq_TOTAL_MPRA<-as.data.frame(NEW_Table_S6.dt[,.(TOTAL=.N),by=key(NEW_Table_S6.dt)], stringsAsFactors=F)
  
  colnames(Freq_TOTAL_MPRA)[which(colnames(Freq_TOTAL_MPRA) == 'MPRA_CLASS')]<-'CLASS'
  
  if(DEBUG == 1)
  {
    cat("Freq_TOTAL_MPRA_0\n")
    cat(str(Freq_TOTAL_MPRA))
    cat("\n")
    #quit(status = 1)
  }
  
  NEW_Table_S6.dt<-data.table(NEW_Table_S6, key=c("Mechanistic_Class_compressed"))
  
  Freq_TOTAL_Mechanistic_Class_compressed<-as.data.frame(NEW_Table_S6.dt[,.(TOTAL=.N),by=key(NEW_Table_S6.dt)], stringsAsFactors=F)
  
  colnames(Freq_TOTAL_Mechanistic_Class_compressed)[which(colnames(Freq_TOTAL_Mechanistic_Class_compressed) == 'Mechanistic_Class_compressed')]<-'CLASS'
  
  if(DEBUG == 1)
  {
    cat("Freq_TOTAL_Mechanistic_Class_compressed_0\n")
    cat(str(Freq_TOTAL_Mechanistic_Class_compressed))
    cat("\n")
    #quit(status = 1)
  }
  
  
  
  Freq_instances<-merge(Freq_instances,
                        Freq_TOTAL_interaction,
                        by="interaction")
  
  if(DEBUG == 1)
  {
    cat("Freq_instances_1\n")
    cat(str(Freq_instances))
    cat("\n")
    #quit(status = 1)
  }
  
  Freq_instances$Perc<-round(100*(Freq_instances$instances/Freq_instances$TOTAL),2)
  
  if(DEBUG == 1)
  {
    cat("Freq_instances_2\n")
    cat(str(Freq_instances))
    cat("\n")
    #quit(status = 1)
  }
  
  #### subgraph ------------------------------------
  
  subgraph_df<-rbind(Freq_TOTAL_MPRA,
                     Freq_TOTAL_Mechanistic_Class_compressed)
  
  levels_CLASS<-subgraph_df$CLASS
  
  subgraph_df$CLASS<-factor(subgraph_df$CLASS,
                            levels=rev(levels_CLASS),
                            ordered=T)
  if(DEBUG == 1)
  {
    cat("subgraph_df_0\n")
    cat(str(subgraph_df))
    cat("\n")
    #quit(status = 1)
  }
  
  
  
  A<-summary(subgraph_df$TOTAL)
  
  breaks.Rank<-unique(sort(c(0,max(A),seq(0,max(A), by=10))))
  # breaks.Rank<-unique(sort(c(0,100,seq(0,100, by=25))))
  labels.Rank<-as.character(breaks.Rank)
  
  if(DEBUG == 1)
  {
    cat("labels.Rank\n")
    cat(str(labels.Rank))
    cat("\n")
    #quit(status = 1)
  }
  
  vector_colors_INTIAL<-brewer.pal(8, "Paired")
  
  
  vector_colors_DEF<-c(vector_colors_INTIAL[3:4],vector_colors_INTIAL[5:6],vector_colors_INTIAL[1:2])
  
  
  
  subgraph<-ggplot(data=subgraph_df,
                   aes(y=CLASS,
                       x=TOTAL,
                       fill=CLASS)) +
    geom_vline(xintercept=breaks.Rank, size=0.25, linetype="dashed", color='gray')+
    geom_bar(stat="identity",colour='white')+
    scale_fill_manual(values=vector_colors_DEF, drop=F)
  
  
  
  
  subgraph<-subgraph+
    theme_classic()+
    theme(axis.title.y=element_text(size=8, color="black", family="sans"),
          axis.title.x=element_text(size=8, color="black", family="sans"),
          axis.text.y=element_text(size=6, color="black", family="sans"),
          axis.text.x=element_text(size=6, color="black", family="sans"),
          axis.line.x = element_line(size = 0.2),
          axis.ticks.x = element_line(size = 0.2),
          axis.ticks.y = element_line(size = 0.2),
          axis.line.y = element_line(size = 0.2))+
    scale_x_reverse(name='Number of variants',breaks=breaks.Rank,labels=labels.Rank)+
    scale_y_discrete(position="right",name=NULL, drop=F)+
    theme(legend.title = element_text(size=6),
          legend.text = element_text(size=6),
          legend.key.size = unit(0.25, 'cm'), #change legend key size
          legend.key.height = unit(0.25, 'cm'), #change legend key height
          legend.key.width = unit(0.25, 'cm'), #change legend key width
          legend.position="hidden")+
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  
  # limits=c(breaks.Rank[1],breaks.Rank[length(breaks.Rank)])
  
  if(DEBUG ==1)
  {
    cat("subgraph DONE\n")
  }
  
  
  #### UpsetR ----
  
  DEBUG<-1
  
  NEW_Table_S6_subset<-NEW_Table_S6[,which(colnames(NEW_Table_S6)%in%c('VAR','Mechanistic_Class','Mechanistic_Class_compressed','MPRA_CLASS'))]
  
  if(DEBUG == 1)
  {
    cat("NEW_Table_S6_subset_0\n")
    cat(str(NEW_Table_S6_subset))
    cat("\n")
    #quit(status = 1)
  }
  
  NEW_Table_S6.m<-melt(NEW_Table_S6_subset, id.vars=c("VAR","Mechanistic_Class"), variable.name="variable", value.name="value")
  NEW_Table_S6.m$Presence<-1
  
  if(DEBUG == 1)
  {
    cat("NEW_Table_S6.m_0\n")
    cat(str(NEW_Table_S6.m))
    cat("\n")
    #quit(status = 1)
  }
  
  
  
  NEW_Table_S6.m_wide<-unique(as.data.frame(pivot_wider(NEW_Table_S6.m,
                                                                     id_cols=c("VAR","Mechanistic_Class"),
                                                                     names_from=value,
                                                                     values_from=Presence), stringsAsFactors=F))
  
  
  if(DEBUG == 1)
  {
    cat("NEW_Table_S6.m_wide_0\n")
    cat(str(NEW_Table_S6.m_wide))
    cat("\n")
    cat(str(unique(NEW_Table_S6.m_wide$VAR)))
    cat("\n")
  }
  
  NEW_Table_S6.m_wide[is.na(NEW_Table_S6.m_wide)]<-0
  
  if(DEBUG == 1)
  {
    cat("NEW_Table_S6.m_wide_1\n")
    cat(str(NEW_Table_S6.m_wide))
    cat("\n")
    cat(str(unique(NEW_Table_S6.m_wide$VAR)))
    cat("\n")
  }
  
  
  
  
  indx.dep<-c(which(colnames(NEW_Table_S6.m_wide) == 'VAR'), which(colnames(NEW_Table_S6.m_wide) == 'Mechanistic_Class'))
  
  TMP <- Convert_up(NEW_Table_S6.m_wide[,-indx.dep]) # remove the VAR & Mechanistic_Class
  
  if(DEBUG == 1)
  {
    cat("TMP_0\n")
    cat(str(TMP))
    cat("\n")
    
  }
  
  TMP$VAR<-NEW_Table_S6.m_wide$VAR
  TMP$Mechanistic_Class<-NEW_Table_S6.m_wide$Mechanistic_Class

  if(DEBUG == 1)
  {
    cat("TMP_1\n")
    cat(str(TMP))
    cat("\n")
    
  }
  
  # TMP<-merge(TMP,
  #            NEW_Table_S6.m_wide,
  #            by=c('VAR','Cell_Type'),
  #            all.x=T)
  # 
  # if(DEBUG == 1)
  # {
  #   cat("TMP_2\n")
  #   cat(str(TMP))
  #   cat("\n")
  #   
  # }
  
  
  
  
  vector.fill<-c(brewer.pal(7, "Set2"))
  
  if(DEBUG == 1)
  {
    cat("vector.fill\n")
    cat(str(vector.fill))
    cat("\n")
  }
  
  
  
  UpsetR_plot<-ggplot(data=TMP,
                      aes(x = upset)) +
    geom_bar(aes(fill= Mechanistic_Class), color='white') +
    scale_x_upset(order_by = "degree")+
    geom_text(stat='count', aes(label=after_stat(count)), vjust=-1) +
    theme_combmatrix(
      combmatrix.label.make_space = TRUE,
      combmatrix.label.width = NULL,
      combmatrix.label.height = NULL,
      combmatrix.label.extra_spacing = 3,
      combmatrix.label.total_extra_spacing = unit(10, "pt"),
      combmatrix.label.text = NULL,
      combmatrix.panel.margin = unit(c(1.5, 1.5), "pt"),
      combmatrix.panel.striped_background = TRUE,
      combmatrix.panel.striped_background.color.one = "white",
      combmatrix.panel.striped_background.color.two = "#F7F7F7",
      combmatrix.panel.point.size = 1,
      combmatrix.panel.line.size = 1,
      combmatrix.panel.point.color.fill = "black",
      combmatrix.panel.point.color.empty = "#E0E0E0")
  
  UpsetR_plot<-UpsetR_plot+
    ggtitle(paste("Total variants compared =",length(unique(NEW_Table_S6.m$VAR)),sep=' '))+
    theme_classic()+
    scale_fill_manual(values=vector.fill, drop=F)+
    theme(plot.title=element_text(size=8, color="black", family="sans"),
          axis.title.y=element_text(size=8, color="black", family="sans"),
          axis.title.x=element_blank(),
          axis.text.y=element_text(size=8, color="black", family="sans"),
          axis.text.x=element_blank(),
          axis.line.x = element_line(size = 0.2),
          axis.line.y = element_line(size = 0.2),
          axis.ticks.x = element_line(size = 0.2),
          axis.ticks.y = element_line(size = 0.2))+
    theme(legend.key.size = unit(0.25, 'cm'), #change legend key size
          legend.key.height = unit(0.25, 'cm'), #change legend key height
          legend.key.width = unit(0.25, 'cm'), #change legend key width
          legend.title = element_blank(), #change legend title font size
          legend.text = element_text(size=6, family="sans"),
          legend.position="bottom")+ #change legend text font size
    ggeasy::easy_center_title()
  
  
  ##### FINAL arrangement ----
  
  subgraph_FINAL<-plot_grid(NULL,subgraph,
                            nrow = 2,
                            ncol=1,
                            rel_heights = c(0.715, 0.285))
  
  graph_FINAL<-plot_grid(subgraph_FINAL,UpsetR_plot,
                         nrow = 1,
                         ncol=2,
                         rel_widths=c(0.4,0.6))
  
  
  
  
  setwd(path_graphs)
  
  svgname<-paste('Modalities_UpsetR_with_subgraph_MPRA_S','.svg',sep='')
  makesvg = TRUE
  
  if (makesvg == TRUE)
  {
    ggsave(svgname, plot= graph_FINAL,
           device="svg",
           height=4, width=4.5)
  }
  
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
    make_option(c("--NEW_Table_S6"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Table_S6"), type="character", default=NULL, 
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
  
  upsetr_and_venn_preprint(opt)
  upsetr_and_venn_now(opt)

  
  
}


###########################################################################

system.time( main() )