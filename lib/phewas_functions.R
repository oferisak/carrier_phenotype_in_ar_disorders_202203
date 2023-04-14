get_case_control_counts<-function(phenotypes,genotype_data){
  case_control_counts<-phenotypes%>%
    pivot_longer(-id)%>%
    left_join(genotype_data)%>%
    group_by(name)%>%
    summarize(non_na_cases=sum(ifelse(!is.na(value) & genotype==1,1,0),na.rm = T),
              cases_with_pheno=sum(ifelse(value & genotype==1,1,0),na.rm = T),
              non_na_controls=sum(ifelse(!is.na(value) & genotype==0,1,0),na.rm = T),
              controls_with_pheno=sum(ifelse(value & genotype==0,1,0),na.rm = T))%>%
    rename(phenotype='name')
  return(case_control_counts)
}

plot_phewas_or<-function(results_dat,pval_thresh=0.05,phen_col_name='phenotype'){
  g<-results_dat%>%
    filter(p<pval_thresh)%>%
    ggplot(aes(y=exp(beta),x=factor(!!sym(phen_col_name),levels=results_dat%>%arrange(beta)%>%pull(phen_col_name)),
               ymin=exp(beta-1.96*SE),
               ymax=exp(beta+1.96*SE)))+
    geom_point()+geom_errorbar(width=0.1)+
    geom_hline(yintercept = 1,linetype=2)+
    labs(y='OR',x='phenotype')+scale_y_log10()+
    coord_flip()+
    theme_minimal()
  print(g)
  return(g)
  
}
