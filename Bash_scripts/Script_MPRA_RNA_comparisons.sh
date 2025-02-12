#!/bin/bash
 
MASTER_ROUTE=$1
analysis=$2

 

Rscripts_path=$(echo "/home/manuel.tardaguila/Scripts/R/")
module load R/4.1.0


bashrc_file=$(echo "/home/manuel.tardaguila/.bashrc")

source $bashrc_file
eval "$(conda shell.bash hook)"


output_dir=$(echo "$MASTER_ROUTE""$analysis""/")

Log_files=$(echo "$output_dir""/""Log_files/")

rm -rf $Log_files
mkdir -p $Log_files

#### Heatmap_RNA_survey #############################


type=$(echo "Heatmap_RNA_survey""_""$analysis")
outfile_Heatmap_RNA_survey=$(echo "$Log_files""outfile_0_""$type"".log")
touch $outfile_Heatmap_RNA_survey
echo -n "" > $outfile_Heatmap_RNA_survey
name_Heatmap_RNA_survey=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_Heatmap_RNA_survey=$(echo "$Rscripts_path""36_Heatmap_RNA_survey_v2.R")
 
Table_S6=$(echo "/group/soranzo/manuel.tardaguila/paper_relaunch/MPRA_S_no_global_metanalysis/Explore_RNA_analysis/NEW_Table_S6.rds")
Table_S7=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S7_Provisional.rds")


myjobid_Heatmap_RNA_survey=$(sbatch --job-name=$name_Heatmap_RNA_survey --output=$outfile_Heatmap_RNA_survey --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_Heatmap_RNA_survey --Table_S7 $Table_S7 --Table_S6 $Table_S6 --type $type --out $output_dir")
myjobid_seff_Heatmap_RNA_survey=$(sbatch --dependency=afterany:$myjobid_Heatmap_RNA_survey --open-mode=append --output=$outfile_Heatmap_RNA_survey --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_Heatmap_RNA_survey >> $outfile_Heatmap_RNA_survey")

#### UpsetR_modalities #############################


type=$(echo "UpsetR_modalities""_""$analysis")
outfile_UpsetR_modalities=$(echo "$Log_files""outfile_1_""$type"".log")
touch $outfile_UpsetR_modalities
echo -n "" > $outfile_UpsetR_modalities
name_UpsetR_modalities=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_UpsetR_modalities=$(echo "$Rscripts_path""367_UpSetR_experimental_modalities_v3.R")

Table_S6=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S6_Provisional.rds")
NEW_Table_S6=$(echo "/group/soranzo/manuel.tardaguila/paper_relaunch/MPRA_S_no_global_metanalysis/NEW_Table_S6.rds")

myjobid_UpsetR_modalities=$(sbatch --job-name=$name_UpsetR_modalities --output=$outfile_UpsetR_modalities --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=512M --parsable --wrap="Rscript $Rscript_UpsetR_modalities --Table_S6 $Table_S6 --NEW_Table_S6 $NEW_Table_S6 --type $type --out $output_dir")
myjobid_seff_UpsetR_modalities=$(sbatch --dependency=afterany:$myjobid_UpsetR_modalities --open-mode=append --output=$outfile_UpsetR_modalities --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_UpsetR_modalities >> $outfile_UpsetR_modalities")

#### range_analysis #############################


type=$(echo "range_analysis""_""$analysis")
outfile_range_analysis=$(echo "$Log_files""outfile_9_""$type"".log")
touch $outfile_range_analysis
echo -n "" > $outfile_range_analysis
name_range_analysis=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_range_analysis=$(echo "$Rscripts_path""389_range_analysis_v2.R")


range_analysis=$(echo "/group/soranzo/manuel.tardaguila/paper_relaunch/MPRA_S_no_global_metanalysis/Table_S2/Range_analisis.tsv")
Table_S6=$(echo "/group/soranzo/manuel.tardaguila/paper_relaunch/MPRA_S_no_global_metanalysis/NEW_Table_S6.rds")
GWAS_parameters=$(echo "PP,Absolute_effect_size,credset_size,MAF")


myjobid_range_analysis=$(sbatch --job-name=$name_range_analysis --output=$outfile_range_analysis --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=4 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_range_analysis --range_analysis $range_analysis --Table_S6 $Table_S6 --type $type --out $output_dir")
myjobid_seff_range_analysis=$(sbatch --dependency=afterany:$myjobid_range_analysis --open-mode=append --output=$outfile_range_analysis --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_range_analysis >> $outfile_range_analysis")


### Barplot_MPRA_vs_Manual_curation ####################################################################


type=$(echo "Barplot_MPRA_vs_Manual_curation")
outfile_Barplot_MPRA_vs_Manual_curation=$(echo "$Log_files""outfile_10_""$type"".log")
touch $outfile_Barplot_MPRA_vs_Manual_curation
echo -n "" > $outfile_Barplot_MPRA_vs_Manual_curation
name_Barplot_MPRA_vs_Manual_curation=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")


Rscript_Barplot_MPRA_vs_Manual_curation=$(echo "$Rscripts_path""75_MPRA_vs_manual_curation_barplot_v2.R")

Table_S6=$(echo "/group/soranzo/manuel.tardaguila/paper_relaunch/MPRA_S_no_global_metanalysis/NEW_Table_S6.rds")

myjobid_Barplot_MPRA_vs_Manual_curation=$(sbatch --job-name=$name_Barplot_MPRA_vs_Manual_curation --output=$outfile_Barplot_MPRA_vs_Manual_curation --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_Barplot_MPRA_vs_Manual_curation --Table_S6 $Table_S6 --type $type --out $output_dir")
myjobid_seff_Barplot_MPRA_vs_Manual_curation=$(sbatch --dependency=afterany:$myjobid_Barplot_MPRA_vs_Manual_curation --open-mode=append --output=$outfile_Barplot_MPRA_vs_Manual_curation --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_Barplot_MPRA_vs_Manual_curation >> $outfile_Barplot_MPRA_vs_Manual_curation")



### chisq_comparisons ####################################################################


type=$(echo "chisq_comparisons")
outfile_chisq_comparisons=$(echo "$Log_files""outfile_11_""$type"".log")
touch $outfile_chisq_comparisons
echo -n "" > $outfile_chisq_comparisons
name_chisq_comparisons=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")


Rscript_chisq_comparisons=$(echo "$Rscripts_path""395_chisq_tests_v2.R")

Table_S6=$(echo "/group/soranzo/manuel.tardaguila/paper_relaunch/MPRA_S_no_global_metanalysis/NEW_Table_S6.rds")

myjobid_chisq_comparisons=$(sbatch --job-name=$name_chisq_comparisons --output=$outfile_chisq_comparisons --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=4 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_chisq_comparisons --Table_S6 $Table_S6 --type $type --out $output_dir")
myjobid_seff_chisq_comparisons=$(sbatch --dependency=afterany:$myjobid_chisq_comparisons --open-mode=append --output=$outfile_chisq_comparisons --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_chisq_comparisons >> $outfile_chisq_comparisons")

###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################

#### Classification_DE_directionality #############################


type=$(echo "Classification_DE_directionality""_""$analysis")
outfile_Classification_DE_directionality=$(echo "$Log_files""outfile_12_""$type"".log")
touch $outfile_Classification_DE_directionality
echo -n "" > $outfile_Classification_DE_directionality
name_Classification_DE_directionality=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_Classification_DE_directionality=$(echo "$Rscripts_path""259_Concordance_analysys_1_classification_DE_v2.R")

NEW_Table_S6=$(echo "/group/soranzo/manuel.tardaguila/paper_relaunch/MPRA_S_no_global_metanalysis/NEW_Table_S6.rds")
Table_S7=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S7_Provisional.rds")


myjobid_Classification_DE_directionality=$(sbatch --job-name=$name_Classification_DE_directionality --output=$outfile_Classification_DE_directionality --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_Classification_DE_directionality --Table_S7 $Table_S7 --NEW_Table_S6 $NEW_Table_S6 --type $type --out $output_dir")
myjobid_seff_Classification_DE_directionality=$(sbatch --dependency=afterany:$myjobid_Classification_DE_directionality --open-mode=append --output=$outfile_Classification_DE_directionality --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_Classification_DE_directionality >> $outfile_Classification_DE_directionality")


#### Classification_MPRA_directionality #############################


type=$(echo "Classification_MPRA_directionality""_""$analysis")
outfile_Classification_MPRA_directionality=$(echo "$Log_files""outfile_13_""$type"".log")
touch $outfile_Classification_MPRA_directionality
echo -n "" > $outfile_Classification_MPRA_directionality
name_Classification_MPRA_directionality=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_Classification_MPRA_directionality=$(echo "$Rscripts_path""260_Concordance_analysys_2_classification_MPRA_v2.R")

MPRA_results=$(echo "/group/soranzo/manuel.tardaguila/paper_relaunch/MPRA_S_no_global_metanalysis/MPRA_results_meta_analysis_collapsed_by_SNP_Threshold_log2FC_0.255_Threshold_FC_meta_padj_0.01_VAR_added.rds")


myjobid_Classification_MPRA_directionality=$(sbatch --job-name=$name_Classification_MPRA_directionality --output=$outfile_Classification_MPRA_directionality --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_Classification_MPRA_directionality --MPRA_results $MPRA_results --type $type --out $output_dir")
myjobid_seff_Classification_MPRA_directionality=$(sbatch --dependency=afterany:$myjobid_Classification_MPRA_directionality --open-mode=append --output=$outfile_Classification_MPRA_directionality --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_Classification_MPRA_directionality >> $outfile_Classification_MPRA_directionality")



#### Merge_and_results #############################


type=$(echo "Merge_and_results""_""$analysis")
outfile_Merge_and_results=$(echo "$Log_files""outfile_14_""$type"".log")
touch $outfile_Merge_and_results
echo -n "" > $outfile_Merge_and_results
name_Merge_and_results=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_Merge_and_results=$(echo "$Rscripts_path""261_Concordance_analysys_3_Merge_results_v2.R")

NEW_Table_S6=$(echo "/group/soranzo/manuel.tardaguila/paper_relaunch/MPRA_S_no_global_metanalysis/NEW_Table_S6.rds")
MPRA_DIRECTIONALITY=$(echo "$output_dir""MPRA_results_ASE_collapsed_DIRECTIONALITY.rds")
DE_DIRECTIONALITY=$(echo "$output_dir""Table_S7_DE_SIG_collapsed_DIRECTIONALITY.rds")

myjobid_Merge_and_results=$(sbatch --dependency=afterany:$myjobid_Classification_DE_directionality:$myjobid_Classification_MPRA_directionality --job-name=$name_Merge_and_results --output=$outfile_Merge_and_results --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_Merge_and_results --MPRA_DIRECTIONALITY $MPRA_DIRECTIONALITY --DE_DIRECTIONALITY $DE_DIRECTIONALITY --NEW_Table_S6 $NEW_Table_S6 --type $type --out $output_dir")
myjobid_seff_Merge_and_results=$(sbatch --dependency=afterany:$myjobid_Merge_and_results --open-mode=append --output=$outfile_Merge_and_results --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_Merge_and_results >> $outfile_Merge_and_results")


