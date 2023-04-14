library(SAIGE)
prep_ped_file_for_plink<-function(phenotype_data){
  # Family ID Sample ID Paternal ID Maternal ID Sex Phenotype
  fam_file<-phenotype_data%>%
    select(`Sample ID`=id,
           Phenotype=phenotype,
           alleles)%>%
    mutate(`Family ID`=seq(1,nrow(phenotype_data),1),
           `Paternal ID`=0,
           `Maternal ID`=0,
           Sex=2)%>%
    relocate(Phenotype,alleles, .after = last_col())%>%
    relocate(`Family ID`,.before = 1)
  return(fam_file)
}
