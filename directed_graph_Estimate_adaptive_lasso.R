library(MASS)
#install.packages("bnlearn")
library(bnlearn)
#install.packages("ggm")
library(ggm)
library(Matrix)
#install.packages("glmnet")
library(glmnet) 


setwd("D:/data/onedrive/OneDrive - korea.ac.kr/2020-1/확률그래프모델및네트워크데이터/프로젝트/")

ALPHA<-0.1
Rep<-20
Cor <- 0.8

#for(nV in c(50,200))
#{
  nV<- 10
  Sample_size<-3952
  #for(Type in c("scale.f")) #"gen.r" "gen.h" "peng.r", "peng.h","scale.f","band" ###"block.r", "block.h" is diff
  #{
  Type<- "Random"
    
    #for(Sample_size in c(100))
    #{
    Sample_size<-3952
    
      #for(Num_Parents in c(1,2))
      #{
      Num_Parents<-1

        file_name=paste("Directed_graph_Estimate_adaptive_lasso", sep="")
        dir.create(file_name)
      
        file_name=paste("Directed_graph_Estimate_adaptive_lasso/Cor_",Cor,"_Type_",Type, sep="")
        dir.create(file_name)

        file_name=paste("Directed_graph_Estimate_adaptive_lasso/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents, sep="")
        dir.create(file_name)
       
       SEED<-1
       #for(SEED in 1:20)
       #{
         file_name=paste("Directed_graph_Estimate_adaptive_lasso/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED, sep="")
         dir.create(file_name)
         
         for(penalty_lambda in c(1,0.5, 0.3 ,0.1) ) # seq(12,0.01,-0.01)
         {
         
         lambda<-penalty_lambda
         
            if(lambda==-1)
            {
            lambda<-round(2/Sample_size^0.5 *qnorm(ALPHA/2/(nV*(nV-1)), mean = 0, sd = 1, lower.tail = FALSE),5)
            }
                   
           
           file_name = 'data/df_10_features_dropped_sgg_standardized.csv'
           
           Samples_stand<-read.table(file_name, header=TRUE, sep=",")
           

######################
  ## Call the function DIST_ADAP_Improving_Search().
  source(paste("./Adaptive_Lasso_R/Sub1_adap_cLasso_Improving_Search.R",sep=""))
  source(paste("./Adaptive_Lasso_R/Sub2_Find_Leaving_Edge.R",sep=""))
  source(paste("./Adaptive_Lasso_R/Sub2_Update_DELTA_Weight_Matrix.R",sep=""))
          

  gamma_weight<-0.15
  
  est_adaptive_lasso<-matrix(0,ncol=nV,nrow=nV)
  
  starting_time<- proc.time()
 
 result.DIST_ADAP.Improving.Search <- Sub1_adap_cLasso_Improving_Search(x.stand=Samples_stand,lambda=lambda,search.index=1,gamma_weight=gamma_weight,initial_weight ="Lasso",initial_lambda=0.1)
  
           time_difference <- proc.time() - starting_time
           
   ## Obtain the result matrix, est.T, est.A, DELTA, and Space.
  est_adaptive_lasso<-result.DIST_ADAP.Improving.Search$est_T
  est_A_DIS<-result.DIST_ADAP.Improving.Search$est_A
  objective_value_DIS <- result.DIST_ADAP.Improving.Search$objective_value
  est_adaptive_lasso<-as.matrix(est_adaptive_lasso)        
   
           
           file_name=paste("Directed_graph_Estimate_adaptive_lasso/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/time_difference_lambda_",lambda,".csv", sep="")
           write.table(t(time_difference),row.names=FALSE,col.names=TRUE, file=file_name,sep=",")
           
           est_adaptive_lasso_short<-matrix(NA,nrow=1,ncol=3)
           for(i in 1:nV)
           {
             for(j in 1:nV)
             {
               if(est_adaptive_lasso[i,j]!=0)
               {
                 est_adaptive_lasso_short<-rbind(est_adaptive_lasso_short,c(i,j,est_adaptive_lasso[i,j]))
               }
             }
           }
           colnames(est_adaptive_lasso_short)<-c("row","col","coeff")
           est_adaptive_lasso_short<-est_adaptive_lasso_short[!is.na(est_adaptive_lasso_short[,1]),]
           file_name=paste("Directed_graph_Estimate_adaptive_lasso/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/est_adaptive_lasso_short_lambda_",lambda,".csv", sep="")
           write.table(est_adaptive_lasso_short,row.names=FALSE,col.names=TRUE, file=file_name,sep=",")

           file_name=paste("Directed_graph_Estimate_adaptive_lasso/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/est_adaptive_lasso_lambda_",lambda,".csv", sep="")
           write.table(est_adaptive_lasso,row.names=FALSE,col.names=TRUE, file=file_name,sep=",")


##################################
         

colnames(est_adaptive_lasso) <- colnames(Samples_stand)
rownames(est_adaptive_lasso) <- colnames(Samples_stand)

#install.packages("network")
library(network)
est_X_net<-network(t(est_adaptive_lasso))
 
main_name=paste("adaptive_lasso: ",Type,", p=",nV,", lambda=",lambda,sep="")
  
plot(est_X_net, main=main_name,displaylabels=TRUE,boxed.labels=FALSE,mode="fruchtermanreingold",arrowhead.cex=1) #,coord=coordinates)

####################################
 
file_name_plots=paste("Directed_graph_Estimate_adaptive_lasso/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/adaptive_lasso_lambda_",lambda,".eps", sep="")
postscript(file=file_name_plots, paper="letter",horizontal =TRUE)  
par(mfrow=c(1,1))
plot(est_X_net, main=main_name,displaylabels=TRUE,boxed.labels=FALSE,mode="fruchtermanreingold",arrowhead.cex=1) #,coord=coordinates)
    
dev.off()  
             
####################################
 
file_name_plots=paste("Directed_graph_Estimate_adaptive_lasso/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/adaptive_lasso_lambda_",lambda,".pdf", sep="")
pdf(file=file_name_plots)  
plot(est_X_net, main=main_name,displaylabels=TRUE,boxed.labels=FALSE,mode="fruchtermanreingold",arrowhead.cex=1) #,coord=coordinates)
            
dev.off()  
  
######################################

equi_adaptive_lasso <- t(essentialGraph(t(est_adaptive_lasso)))
equi_X_net<-network(t(equi_adaptive_lasso))
 
file_name_plots=paste("Directed_graph_Estimate_adaptive_lasso/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/Equi_adaptive_lasso_lambda_",lambda,".pdf", sep="")
pdf(file=file_name_plots)  
plot(equi_X_net, main=main_name,displaylabels=TRUE,boxed.labels=FALSE,mode="fruchtermanreingold",arrowhead.cex=1)#,coord=coordinates)
            
dev.off()  
  
######################################

           
      
      
           
           
         }
         
        
         
       }
      }
    }
  }
}
