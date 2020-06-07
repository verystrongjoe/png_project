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

        file_name=paste("Directed_graph_Estimate_mmhc", sep="")
        dir.create(file_name)
      
        file_name=paste("Directed_graph_Estimate_mmhc/Cor_",Cor,"_Type_",Type, sep="")
        dir.create(file_name)

        file_name=paste("Directed_graph_Estimate_mmhc/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents, sep="")
        dir.create(file_name)
       
       SEED<-1
       #for(SEED in 1:20)
       #{
         file_name=paste("Directed_graph_Estimate_mmhc/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED, sep="")
         dir.create(file_name)
         
         for(penalty_lambda in c(1,0.5, 0.3, 0.1) ) # seq(12,0.01,-0.01)
         {
         alpha_MMHC<-penalty_lambda
         
           file_name = 'data/dataset_prices_btn_sgg_standardized.csv'
           Samples_stand<-read.table(file_name, header=TRUE, sep=",")
           #colnames(Samples_stand) <- 
           
           starting_time <- proc.time()

  est_mmhc<-matrix(0,ncol=nV,nrow=nV)
  
  starting_time<- proc.time()
 
  #mmhc.fit<-rsmax2(Samples_stand,restrict ="mmpc",maximize = "hc",restrict.args = c("alpha",0.05)) 
   mmhc.fit<- mmhc(Samples_stand, restrict.args =list(alpha=alpha_MMHC) )

  g<-mmhc.fit$arcs

  if( length(g)>0 )
  {
  g[,1]<-matrix(unlist(strsplit(g[,1], split='X', fixed=TRUE)),ncol=dim(g)[1],nrow=2)[2,]
  g[,2]<-matrix(unlist(strsplit(g[,2], split='X', fixed=TRUE)),ncol=dim(g)[1],nrow=2)[2,]

  gg<-matrix(as.numeric(g),ncol=2,nrow=dim(g)[1])

  if(dim(gg)[1]!=0)
  {
  for(i in 1:dim(gg)[1])
  {
  est_mmhc[gg[i,2],gg[i,1]]<-1
  }
  }
  }
  
           time_difference <- proc.time() - starting_time
           
           file_name=paste("Directed_graph_Estimate_mmhc/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/time_difference_alpha_MMHC_",alpha_MMHC,".csv", sep="")
           write.table(t(time_difference),row.names=FALSE,col.names=TRUE, file=file_name,sep=",")
           
           est_mmhc_short<-matrix(NA,nrow=1,ncol=3)
           for(i in 1:nV)
           {
             for(j in 1:nV)
             {
               if(est_mmhc[i,j]!=0)
               {
                 est_mmhc_short<-rbind(est_mmhc_short,c(i,j,est_mmhc[i,j]))
               }
             }
           }
           colnames(est_mmhc_short)<-c("row","col","coeff")
           est_mmhc_short<-est_mmhc_short[!is.na(est_mmhc_short[,1]),]
           file_name=paste("Directed_graph_Estimate_mmhc/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/est_mmhc_short_alpha_MMHC_",alpha_MMHC,".csv", sep="")
           write.table(est_mmhc_short,row.names=FALSE,col.names=TRUE, file=file_name,sep=",")

           file_name=paste("Directed_graph_Estimate_mmhc/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/est_mmhc_alpha_MMHC_",alpha_MMHC,".csv", sep="")
           write.table(est_mmhc,row.names=FALSE,col.names=TRUE, file=file_name,sep=",")




##################################
         
          # file_name=paste("Step_0_Simulate_Data/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"_coordinates.csv", sep="")
           #coordinates<-as.matrix(read.table(file_name, header=TRUE, sep=","))
          

colnames(est_mmhc) <- colnames(Samples_stand)
rownames(est_mmhc) <- colnames(Samples_stand)

#install.packages("network")
library(network)
est_X_net<-network(t(est_mmhc))
 
main_name=paste("mmhc: ",Type,", p=",nV,", alpha_MMHC=",alpha_MMHC,sep="")
  
plot(est_X_net, main=main_name,displaylabels=TRUE,boxed.labels=FALSE,mode="fruchtermanreingold",arrowhead.cex=1)#,coord=coordinates)

####################################
 
file_name_plots=paste("Directed_graph_Estimate_mmhc/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/Estimated_Graph_alpha_MMHC_",alpha_MMHC,".eps", sep="")
postscript(file=file_name_plots, paper="letter",horizontal =TRUE)  
par(mfrow=c(1,1))
plot(est_X_net, main=main_name,displaylabels=TRUE,boxed.labels=FALSE,mode="fruchtermanreingold",arrowhead.cex=1)#,coord=coordinates)
    
dev.off()  
             
####################################
 
file_name_plots=paste("Directed_graph_Estimate_mmhc/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/Estimated_Graph_alpha_MMHC_",alpha_MMHC,".pdf", sep="")
pdf(file=file_name_plots)  
plot(est_X_net, main=main_name,displaylabels=TRUE,boxed.labels=FALSE,mode="fruchtermanreingold",arrowhead.cex=1)#,coord=coordinates)
            
dev.off()  
           
####################################
  
equi_mmhc <- t(essentialGraph(t(est_mmhc)))
equi_X_net<-network(t(equi_mmhc))
 
file_name_plots=paste("Directed_graph_Estimate_mmhc/Cor_",Cor,"_Type_",Type,"/nV_",nV,"_Size_",Sample_size,"_edge_",Num_Parents,"/SEED_",SEED,"/Estimated_Graph_alpha_MMHC_",alpha_MMHC,".pdf", sep="")
pdf(file=file_name_plots)  
plot(equi_X_net, main=main_name,displaylabels=TRUE,boxed.labels=FALSE,mode="fruchtermanreingold",arrowhead.cex=1)#,coord=coordinates)
            
dev.off()  
  
######################################

equi_hc <- t(essentialGraph(t(est_hc)))

           
      
      
           
           
         }
         
        
         
       }
      }
    }
  }
}
