library(MASS)
library(Matrix)
library(glmnet) 
library(igraph)
library(RBGL) 
library(ggm) 


ALPHA<-0.1
Cor<-0.7              



for(Type in c(1) )  # Start FOR loop of vertaxs
{
                
for(nV in c(50) )  # Start FOR loop of vertaxs
{

for(Sample_size in c(500) )
{

if(Sample_size==300)
{
Num_Parents_list <- 1
}
if(Sample_size==500)
{
Num_Parents_list <- 2
}

for( Num_Parents in Num_Parents_list )
{

for(SEED in 1)
{

  lambda_list <- sort( c( seq(0.002,0.008,by =0.002), seq(0.01,0.04,by =0.01) ) , decreasing = TRUE )   # sort(c(seq(0.03,0.2,by =0.01),seq(0.22,0.5,by =0.02),seq(0.5,3,by =0.04)),decreasing = TRUE)

for(lambda in lambda_list[1] )
{

if(lambda==-1)
{
lambda<-round(2/Sample_size^0.5 *qnorm(ALPHA/2/(nV*(nV-1)), mean = 0, sd = 1, lower.tail = FALSE),5)
}
  
    
  file_name=paste("C:/Step_1_Estimate_by_NS_DIST/Cor_",Cor,"_Type_",Type, sep="")
  dir.create(file_name)

  file_name=paste("C:/Step_1_Estimate_by_NS_DIST/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents, sep="")
  dir.create(file_name)

  file_name=paste("C:/Step_1_Estimate_by_NS_DIST/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED, sep="")
  dir.create(file_name)


  # extract data

  file_name=paste("C:/Step_0_Simulate_Data_High/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"_Samples_stand.csv", sep="")
  Samples_stand<-read.table(file_name, header=TRUE, sep=",")
                   
  Data_size<-dim(Samples_stand)[1]
  nV<-dim(Samples_stand)[2] 


###############################
  
  colnames(Samples_stand)<-paste("X",c(1:nV),sep="")

######################
  ## Call the function DIST_ADAP_Improving_Search().
  source(paste("C:/Adaptive_Lasso_R/Sub1_adap_cLasso_Improving_Search.R",sep=""))
  source(paste("C:/Adaptive_Lasso_R/Sub2_Find_Leaving_Edge.R",sep=""))
  source(paste("C:/Adaptive_Lasso_R/Sub2_Update_DELTA_Weight_Matrix.R",sep=""))


  starting_time_DIS <- proc.time()
  gamma_weight<-0.15

  #x.stand=Samples_stand
  #lambda=lambda
  #search.index=1
  #gamma_weight=gamma_weight
  #initial_weight ="Lasso"
  
 result.DIST_ADAP.Improving.Search <- Sub1_adap_cLasso_Improving_Search(x.stand=Samples_stand,lambda=lambda,search.index=1,gamma_weight=gamma_weight,initial_weight ="Lasso",initial_lambda=0.1)


  time_difference_DIS <- proc.time() - starting_time_DIS
  
  ## Obtain the result matrix, est.T, est.A, DELTA, and Space.
  est_T_DIS<-result.DIST_ADAP.Improving.Search$est_T
  est_A_DIS<-result.DIST_ADAP.Improving.Search$est_A
  objective_value_DIS <- result.DIST_ADAP.Improving.Search$objective_value

###############################

  est_T_short_DIS<-matrix(NA,nrow=1,ncol=3)  
  for(i in 1:nV)
  {
    for(j in 1:nV)
    {
      if(est_T_DIS[i,j]==1)
      {
      est_T_short_DIS<-rbind(est_T_short_DIS,c(i,j,est_A_DIS[i,j]))
      }
    }
  }
  colnames(est_T_short_DIS)<-c("row","col","value")
  est_T_short_DIS<-est_T_short_DIS[!is.na(est_T_short_DIS[,1]),]

  Summary_after_DIS<-c(Num_Parents,SEED,objective_value_DIS,time_difference_DIS[3])

  file_name_Summary_after_DIS=paste("C:/Step_1_Estimate_by_NS_DIST/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/lambda_",lambda,"_Z_Summary_est_T.csv", sep="")
  write.table(t(Summary_after_DIS),row.names=FALSE,col.names=c("Num_Parents","SEED","objective_value","time_difference"), file=file_name_Summary_after_DIS,sep=",")
  
  if(length(est_T_short_DIS)==2)
  {
  file_name_est_T_short_DIS=paste("C:/Step_1_Estimate_by_NS_DIST/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/lambda_",lambda,"_Z_est_T_short.csv", sep="")
  write.table(t(est_T_short_DIS),row.names=FALSE,col.names=TRUE, file=file_name_est_T_short_DIS,sep=",")
  }else{
  file_name_est_T_short_DIS=paste("C:/Step_1_Estimate_by_NS_DIST/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/lambda_",lambda,"_Z_est_T_short.csv", sep="")
  write.table(est_T_short_DIS,row.names=FALSE,col.names=TRUE, file=file_name_est_T_short_DIS,sep=",")
  }
  
  
}
}
}
}
}
}
 

