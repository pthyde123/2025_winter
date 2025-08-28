


#########  Genotype Effect  ###########

library(StageWise)



pheno.file <- system.file("vignette_data", "pheno1a.csv", package = "StageWise")

ans1 <- Stage1(filename=pheno.file,traits=c("total.yield","vine.maturity","fry.color"),
               effects <- data.frame(name=c("block","stand.count"),
                                     fixed=c(FALSE,TRUE),
                                     factor=c(TRUE,FALSE)))

names(ans1)

geno.file <- system.file("vignette_data", "geno1.csv", package = "StageWise")


geno <- read_geno(geno.file,ploidy=4,map=TRUE,dominance = TRUE)


### rrBLUP
library(rrBLUP)

#random population of 200 lines with 1000 markers
M <- matrix(rep(0,200*1000),200,1000)
for (i in 1:200) {
  M[i,] <- ifelse(runif(1000)<0.5,-1,1)
}
#random phenotypes
u <- rnorm(1000)
g <- as.vector(crossprod(t(M),u))
h2 <- 0.5 #heritability
y <- g + rnorm(200,mean=0,sd=sqrt((1-h2)/h2*var(g)))
#predict marker effects
ans <- mixed.solve(y,Z=M) #By default K = I
accuracy <- cor(u,ans$u)
#predict breeding values
ans <- mixed.solve(y,K=A.mat(M))
accuracy <- cor(g,ans$u)


#### T3 Genotype download





########### Kinship Matrix  ##########

## Install stable version
install.packages("AGHmatrix")


## Load 
library(AGHmatrix)


data(ped.mrode)
ped.mrode
str(ped.mrode) #check the structure

Amatrix(ped.mrode, ploidy=6, w=0.1)


library(readr)
SOAP_t3_pedigree_practice <- read_csv("data/SOAP_t3_pedigree_practice.csv")

data <- as.data.frame(SOAP_t3_pedigree_practice)
data$Ind <- as.factor(data$Ind)
data$Par1 <- as.factor(data$Par1)
data$Par2 <- as.factor(data$Par2)

str(data)


library(tidyverse)

df<-Amatrix(data, ploidy=6, w=0.1)

df2<- as_tibble(df, rownames = "accessions")


SOAP_t3_pedigree_practice$stock_uniquename


K <- df2 %>% 
  filter(accessions %in% c(SOAP_t3_pedigree_practice$stock_uniquename)) %>% 
  select(accessions, c(SOAP_t3_pedigree_practice$stock_uniquename)) %>% 
  column_to_rownames(var = "accessions") %>%
  as.matrix()






#########  COMA  ################

library(COMA)




geno.file <- system.file("vignette_data", "geno.csv", package = "COMA")
geno <- read.csv(geno.file,check.names=F)
geno[1:4,1:5]



K.file <- system.file("vignette_data", "kinship.csv", package = "COMA")

#preview files
K <- as.matrix(read.csv(K.file,check.names=F,row.names=1))
K[1:3,1:3]


summary(K[upper.tri(K,diag=F)])


#inbreeding coefficients from A matrix
F.A <- (4*diag(K)-1)/(4-1)
summary(F.A)



library(COMA)
data <- read_data(geno.file=geno.file,
                  kinship.file=K.file,
                  ploidy=4, matings="all",
                  standardize=TRUE, n.core=2)

head(data$parents)


head(data$matings)





parents <- data.frame(id=data$parents$id, min=0, max=1)
matings <- data.frame(data$matings, min=0, max=1)
ans0 <- oma(dF=c(-1,0.005), parents=parents, matings=matings, ploidy=4, K=data$K)
ans0$response



ans1 <- oma(dF=c(-1,0.005), parents=parents, matings=matings, ploidy=4, K=data$K,
            dF.adapt=list(step=0.001,max=0.05))
ans1$response

ans2 <- oma(dF=c(0.005,0.008), parents=parents, matings=matings, ploidy=4, K=data$K,
            dF.adapt=list(step=0.001,max=0.05))
ans2$response


head(ans2$oc)


head(ans2$om)



library(ggplot2)
plot_ribbon(oc=list(`-0.008`=ans1$oc,`0.005`=ans2$oc)) + 
  theme(legend.key.size=unit(0.1,'cm'))







