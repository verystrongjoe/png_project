library(MASS)
#install.packages("glmnet")
library(glmnet) 
#install.packages("ggm")
library(ggm)

setwd("D:/data/onedrive/OneDrive - korea.ac.kr/2020-1/확률그래프모델및네트워크데이터/프로젝트/")

#Rep<-20
                         
Cor <- 0.8

#for(nV in c(50,200))
#{
nV<- 79
Sample_size<-3640
  
  #for(Type in c("scale.f")) #"gen.r" "gen.h" "peng.r", "peng.h","scale.f","band" ###"block.r", "block.h" is diff
  #{
  Type<- "Random"
    
    #for(Sample_size in c(100))
    #{

    
      #for(Num_Parents in c(1,2))
      #{
      Num_Parents<-1

        file_name=paste("UnDirected_graph_Estimate_MB", sep="")
        dir.create(file_name)
      
        file_name=paste("UnDirected_graph_Estimate_MB/Cor_",Cor,"_Type_",Type, sep="")
        dir.create(file_name)

        file_name=paste("UnDirected_graph_Estimate_MB/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents, sep="")
        dir.create(file_name)
       
       SEED<-1
       #for(SEED in 1:20)
       #{
         file_name=paste("UnDirected_graph_Estimate_MB/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED, sep="")
         dir.create(file_name)
         
         for(penalty_lambda in c(2,1,0.5) ) # seq(12,0.01,-0.01)
         {
         #penalty_lambda<-2
         
           #file_name=paste("Step_0_Simulate_Data/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"_Samples_stand.csv", sep="")
           file_name = 'data/dataset_prices_btn_sgg_standardized.csv'
           Samples_stand<-as.matrix(read.table(file_name, header=TRUE, sep=","))
           
           starting_time <- proc.time()
           
           penalty_lambda_2<-penalty_lambda/2
           
           est_MB<-matrix(0,nrow=nV,ncol=nV)
           
           for(index in c(1:nV))
           {
             
             yy<-Samples_stand[,index]                   # Decide yy (child) and xx (parents) in the LASSO type penalized LR
             xx<-Samples_stand[,-index]
             glmnet_fit<-glmnet(xx, yy,family=c("gaussian"),lambda=penalty_lambda_2)
             inter_coef_list<-coef(glmnet_fit)
             coef_list<-inter_coef_list[-1]
             
             est_MB[index,-index]<-coef_list
   
           }
           
           
           time_difference <- proc.time() - starting_time
           
           file_name=paste("UnDirected_graph_Estimate_MB/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/time_difference_lambda_",penalty_lambda,".csv", sep="")
           write.table(t(time_difference),row.names=FALSE,col.names=TRUE, file=file_name,sep=",")
           
           est_MB_short<-matrix(NA,nrow=1,ncol=3)
           for(i in 1:nV)
           {
             for(j in 1:nV)
             {
               if(est_MB[i,j]!=0)
               {
                 est_MB_short<-rbind(est_MB_short,c(i,j,est_MB[i,j]))
               }
             }
           }
           colnames(est_MB_short)<-c("row","col","coeff")
           est_MB_short<-est_MB_short[!is.na(est_MB_short[,1]),]
           file_name=paste("UnDirected_graph_Estimate_MB/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/est_MB_short_lambda_",penalty_lambda,".csv", sep="")
           write.table(est_MB_short,row.names=FALSE,col.names=TRUE, file=file_name,sep=",")

           file_name=paste("UnDirected_graph_Estimate_MB/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/est_MB_lambda_",penalty_lambda,".csv", sep="")
           write.table(est_MB,row.names=FALSE,col.names=TRUE, file=file_name,sep=",")


##################################
         
           #file_name=paste("Step_0_Simulate_Data/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"_coordinates.csv", sep="")
           #coordinates<-as.matrix(read.table(file_name, header=TRUE, sep=","))
          

colnames(est_MB) <- colnames(Samples_stand)
rownames(est_MB) <- colnames(Samples_stand)

#install.packages("network")
library(network)
est_X_net<-network(t(est_MB))
 
main_name=paste("Estimated: ",Type,", p=",nV,", lamda=",penalty_lambda,sep="")
  
plot(est_X_net, main=main_name,displaylabels=TRUE,boxed.labels=FALSE,mode="fruchtermanreingold",arrowhead.cex=0) #,coord=coordinates)

####################################
 
file_name_plots=paste("UnDirected_graph_Estimate_MB/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/Estimated_Graph_lambda_",penalty_lambda,".eps", sep="")
postscript(file=file_name_plots, paper="letter",horizontal =TRUE)  
par(mfrow=c(1,1))
plot(est_X_net, main=main_name,displaylabels=TRUE,boxed.labels=FALSE,mode="fruchtermanreingold",arrowhead.cex=0) #,coord=coordinates)
    
dev.off()  
             
####################################
 
file_name_plots=paste("UnDirected_graph_Estimate_MB/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/Estimated_Graph_lambda_",penalty_lambda,".pdf", sep="")
pdf(file=file_name_plots)  
plot(est_X_net, main=main_name,displaylabels=TRUE,boxed.labels=FALSE,mode="fruchtermanreingold",arrowhead.cex=0) #,coord=coordinates)
            
dev.off()  
  
######################################

#equi_MB <- t(essentialGraph(t(est_MB)))

           
      
      
           
           
         }
         
        
         
       }
      }
    }
  }
}
