### J. Gamboa
### Ph.D student at Texas A&M University.
### Aramayo lab
### j.a.r.gamboa@gmail.com
### jgamboa@bio.tamu.edu
###
### Short script to take the results from CD-HIT clustering and Reciprocal
### BLAST hit against a primate database in order to make inferences about
### how the proteomes of different primates might recapitulate the phylogeny
### (spoiler alert: it does not entirely for the tax closest to H. sapiens)

prot.taxonomy <- read.csv("prot.taxonomy.csv", header = T, row.names = 1)

library(ggplot2)
library(viridis)

q <- ggplot(prot.taxonomy, aes(y=prot.num, x= reorder(taxa, -prot.num))) + 
  geom_bar(aes(fill=as.factor(comm.name)), 
           stat="identity", position="stack", alpha=0.8) + 
  theme_pander() + 
  theme(text=element_text(family="sans", face="plain", color="#000000", 
                          size=15, hjust=0.5, vjust=0.5)) + 
  guides(fill=guide_legend(title="Common name")) + 
  ggtitle("Taxonomical relatedness among primates 
  given the similarity of their proteomes") + 
  xlab("ENSEMBL ID") + 
  ylab("Number of common proteins")

q + scale_fill_viridis_d(taxa)

  
