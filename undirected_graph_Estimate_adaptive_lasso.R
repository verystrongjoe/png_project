library(MASS)
#install.packages("glmnet")
library(glmnet) 
#install.packages("ggm")
library(ggm)

setwd("D:/data/onedrive/OneDrive - korea.ac.kr/2020-1/확률그래프모델및네트워크데이터/프로젝트/")

Rep<-20
                         
Cor <- 0.8
gamma_weight <- 0.15
initial_lambda <- 0.1

#for(nV in c(50,200))
#{
  nV<- 10
  Sample_size<-3952
  
  #for(Type in c("scale.f")) #"gen.r" "gen.h" "peng.r", "peng.h","scale.f","band" ###"block.r", "block.h" is diff
  #{
  Type<- "Random"
    
    #for(Sample_size in c(100))
    #{
    
      #for(Num_Parents in c(1,2))
      #{
      Num_Parents<-1

        file_name=paste("UnDirected_graph_Estimate_adaptive_lasso", sep="")
        dir.create(file_name)
      
        file_name=paste("UnDirected_graph_Estimate_adaptive_lasso/Cor_",Cor,"_Type_",Type, sep="")
        dir.create(file_name)

        file_name=paste("UnDirected_graph_Estimate_adaptive_lasso/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents, sep="")
        dir.create(file_name)
       
       SEED<-1
       #for(SEED in 1:20)
       #{
         file_name=paste("UnDirected_graph_Estimate_adaptive_lasso/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED, sep="")
         dir.create(file_name)
         
         for(penalty_lambda in c(2,1,0.5) ) # seq(12,0.01,-0.01)
         {
         
           #file_name=paste("Step_0_Simulate_Data/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"_Samples_stand.csv", sep="")
           file_name = 'data/df_10_features_dropped_sgg_standardized.csv'
         
           Samples_stand<-as.matrix(read.table(file_name, header=TRUE, sep=","))
           
           starting_time <- proc.time()
           
           penalty_lambda_2<-penalty_lambda/2
           
           est_adaptive_lasso<-matrix(0,nrow=nV,ncol=nV)
           
           lambda <- penalty_lambda
                      
           for(index in c(1:nV))
           {
             
             yy<-Samples_stand[,index]                   # Decide yy (child) and xx (parents) in the LASSO type penalized LR
             xx<-Samples_stand[,-index]
############### ### weigtht             
             
             lm_fit<-glmnet(xx, yy,family=c("gaussian"),lambda=initial_lambda/2) # Run lm
             coef_list<-coef(lm_fit)[-1]
             coef_list[coef_list==0]<-1/10^4
             
             Weight_list<-1/abs(coef_list)
             Weight_list[Weight_list>10^4]<-10^4
             
             Weight_list<-Weight_list^gamma_weight
             Weight_list[Weight_list<1] <- 1
             
             
             lambda_2<-lambda/2*sum(Weight_list)/length(Weight_list)
             
             glmnet_fit<-glmnet(xx, yy,family=c("gaussian"),lambda=lambda_2,penalty.factor=Weight_list) # Run glmnet
             coef_list_2<-coef(glmnet_fit)[-1]
                
             est_adaptive_lasso[index,-index]<-coef_list_2
   
           }
           
           
           time_difference <- proc.time() - starting_time
           
           file_name=paste("UnDirected_graph_Estimate_adaptive_lasso/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/time_difference_lambda_",penalty_lambda,".csv", sep="")
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
           file_name=paste("UnDirected_graph_Estimate_adaptive_lasso/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/est_adaptive_lasso_short_lambda_",penalty_lambda,".csv", sep="")
           write.table(est_adaptive_lasso_short,row.names=FALSE,col.names=TRUE, file=file_name,sep=",")

           file_name=paste("UnDirected_graph_Estimate_adaptive_lasso/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/est_adaptive_lasso_lambda_",penalty_lambda,".csv", sep="")
           write.table(est_adaptive_lasso,row.names=FALSE,col.names=TRUE, file=file_name,sep=",")


##################################


           #file_name=paste("Step_0_Simulate_Data/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"_coordinates.csv", sep="")
           #coordinates<-as.matrix(read.table(file_name, header=TRUE, sep=","))
          


colnames(est_adaptive_lasso) <- colnames(Samples_stand)
rownames(est_adaptive_lasso) <- colnames(Samples_stand)

#install.packages("network")
library(network)
est_X_net<-network(t(est_adaptive_lasso))

 
main_name=paste("Estimated: ",Type,", p=",nV,", lamda=",penalty_lambda,sep="")

  
plot(est_X_net, main=main_name,displaylabels=TRUE,boxed.labels=FALSE,mode="fruchtermanreingold",arrowhead.cex=0)# ,coord=coordinates)

####################################
 
file_name_plots=paste("UnDirected_graph_Estimate_adaptive_lasso/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/Estimated_Graph_lambda_",penalty_lambda,".eps", sep="")
postscript(file=file_name_plots, paper="letter",horizontal =TRUE)  
par(mfrow=c(1,1))
plot(est_X_net, main=main_name,displaylabels=TRUE,boxed.labels=FALSE,mode="fruchtermanreingold",arrowhead.cex=0) #,coord=coordinates)
    
dev.off()  
             
####################################
 
file_name_plots=paste("UnDirected_graph_Estimate_adaptive_lasso/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/Estimated_Graph_lambda_",penalty_lambda,".pdf", sep="")
pdf(file=file_name_plots)  
plot(est_X_net, main=main_name,displaylabels=TRUE,boxed.labels=FALSE,mode="fruchtermanreingold",arrowhead.cex=0) #,coord=coordinates)
            
dev.off()  
  
######################################

#equi_adaptive_lasso <- t(essentialGraph(t(est_adaptive_lasso)))

           
      
      
           
           
         }
         
        
         
       }
      }
    }
  }
}
