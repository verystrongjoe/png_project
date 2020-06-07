library(MASS)
#install.packages("bnlearn")
library(bnlearn)
#install.packages("ggm")
library(ggm)

setwd("D:/data/onedrive/OneDrive - korea.ac.kr/2020-1/확률그래프모델및네트워크데이터/프로젝트/")

Rep<-20
                         
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
    #Sample_size<-100
    
      #for(Num_Parents in c(1,2))
      #{
      Num_Parents<-1

        file_name=paste("Directed_graph_Estimate_hc", sep="")
        dir.create(file_name)
      
        file_name=paste("Directed_graph_Estimate_hc/Cor_",Cor,"_Type_",Type, sep="")
        dir.create(file_name)

        file_name=paste("Directed_graph_Estimate_hc/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents, sep="")
        dir.create(file_name)
       
       SEED<-1
       #for(SEED in 1:20)
       #{
         file_name=paste("Directed_graph_Estimate_hc/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED, sep="")
         dir.create(file_name)
         
         #for(penalty_lambda in c(2,1,0.5) ) # seq(12,0.01,-0.01)
         #{
         #c(1,0.5, 0.3 ,0.1)
         
           #file_name=paste("Step_0_Simulate_Data/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"_Samples_stand.csv", sep="")
           file_name = 'data/dataset_prices_btn_sgg_standardized.csv'
           Samples_stand<-read.table(file_name, header=TRUE, sep=",")
           #colnames(Samples_stand) <- 
           
           

  est_hc<-matrix(0,ncol=nV,nrow=nV)
  
  starting_time<- proc.time()
 
  hc.fit<-hc(Samples_stand)   #,restrict.args=alpha_MMHC)
  g<-hc.fit$arcs

  if( length(g)>0 )
  {
  g[,1]<-matrix(unlist(strsplit(g[,1], split='X', fixed=TRUE)),ncol=dim(g)[1],nrow=2)[2,]
  g[,2]<-matrix(unlist(strsplit(g[,2], split='X', fixed=TRUE)),ncol=dim(g)[1],nrow=2)[2,]

  gg<-matrix(as.numeric(g),ncol=2,nrow=dim(g)[1])

  if(dim(gg)[1]!=0)
  {
  for(i in 1:dim(gg)[1])
  {
  est_hc[gg[i,2],gg[i,1]]<-1
  }
  }
  }
  
           time_difference <- proc.time() - starting_time
           
           file_name=paste("Directed_graph_Estimate_hc/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/time_difference.csv", sep="")
           write.table(t(time_difference),row.names=FALSE,col.names=TRUE, file=file_name,sep=",")
           
           est_hc_short<-matrix(NA,nrow=1,ncol=3)
           for(i in 1:nV)
           {
             for(j in 1:nV)
             {
               if(est_hc[i,j]!=0)
               {
                 est_hc_short<-rbind(est_hc_short,c(i,j,est_hc[i,j]))
               }
             }
           }
           colnames(est_hc_short)<-c("row","col","coeff")
           est_hc_short<-est_hc_short[!is.na(est_hc_short[,1]),]
           file_name=paste("Directed_graph_Estimate_hc/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/est_hc_short.csv", sep="")
           write.table(est_hc_short,row.names=FALSE,col.names=TRUE, file=file_name,sep=",")

           file_name=paste("Directed_graph_Estimate_hc/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/est_hc.csv", sep="")
           write.table(est_hc,row.names=FALSE,col.names=TRUE, file=file_name,sep=",")


##################################
         
           #file_name=paste("Step_0_Simulate_Data/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"_coordinates.csv", sep="")
           #coordinates<-as.matrix(read.table(file_name, header=TRUE, sep=","))
          

colnames(est_hc) <- colnames(Samples_stand)
rownames(est_hc) <- colnames(Samples_stand)

#install.packages("network")
library(network)
est_X_net<-network(t(est_hc))
 
main_name=paste("hc: ",Type,", p=",nV ,sep="")
  
plot(est_X_net, main=main_name,displaylabels=TRUE,boxed.labels=FALSE,mode="fruchtermanreingold",arrowhead.cex=1)#,coord=coordinates)

####################################
 
file_name_plots=paste("Directed_graph_Estimate_hc/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/Estimated_hc_Graph.eps", sep="")
postscript(file=file_name_plots, paper="letter",horizontal =TRUE)  
par(mfrow=c(1,1))
plot(est_X_net, main=main_name,displaylabels=TRUE,boxed.labels=FALSE,mode="fruchtermanreingold",arrowhead.cex=1)#,coord=coordinates)
    
dev.off()  
             
####################################
 
file_name_plots=paste("Directed_graph_Estimate_hc/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/Estimated_hc_Graph.pdf", sep="")
pdf(file=file_name_plots)  
plot(est_X_net, main=main_name,displaylabels=TRUE,boxed.labels=FALSE,mode="fruchtermanreingold",arrowhead.cex=1)#,coord=coordinates)
            
dev.off()  
  
     
####################################
  
equi_hc <- t(essentialGraph(t(est_hc)))
equi_X_net<-network(t(equi_hc))
 
file_name_plots=paste("Directed_graph_Estimate_hc/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/Equi_hc_Graph.pdf", sep="")
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
