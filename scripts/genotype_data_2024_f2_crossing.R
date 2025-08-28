library(vcfR)



vcf <- read.vcfR("data/WOP_genotype_output.vcf", verbose = FALSE )


chrom <- create.chromR(name='Supercontig', vcf=vcf)



plot(chrom)


head(chrom)


chrom

head(vcf)


index <- c(1, 3, 5, 10)



head(vcf[,index])


colnames(vcf@gt)

accessions <-c("AURORA","AVE265_59","AWNLESS_CURLED","CW559","DOMACA_ZOB","Gerard 227","KARCAGI","NC20-4452","NC20-4526","NC21-6429","NF01404A","NF12AS-107-4_4","NF12AS-108-4_1","NF13-4126-4_3","NF97405B2","PA8014-840","PI344818","SEGER|PI306405","SEGETAL","TRISPERNIA|CIAV5100","WESTFINNISCHER_SCHWARZ","WINTER_TURF|CIAV996")


select(as_tibble(vcf@gt),one_of( c("AURORA","AVE265_59","AWNLESS_CURLED","CW559","DOMACA_ZOB","Gerard 227","KARCAGI","NC20-4452","NC20-4526","NC21-6429","NF01404A","NF12AS-107-4_4","NF12AS-108-4_1","NF13-4126-4_3","NF97405B2","PA8014-840","PI344818","SEGER|PI306405","SEGETAL","TRISPERNIA|CIAV5100","WESTFINNISCHER_SCHWARZ","WINTER_TURF|CIAV996")))


write.table(colnames(vcf@gt), "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)
