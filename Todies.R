setwd("~/Desktop/Colonialism:Science/colonialism-biodiversity")
library(tidyverse)
library(ggplot2)
library(rgbif)
library(viridis)
library(maps)
library(mapdata)

#Dominican Republic
todies.do.eb<-occ_search(country = "DO", taxonKey = 2475383, datasetKey = "4fa7b334-ce0d-4e88-aaae-2e0c138d049e", limit = 10000)$data
todies.do.in<-occ_search(country = "DO", taxonKey = 2475383, datasetKey = "50c9509d-22c7-4a22-a47d-8c48425ef4a7", limit = 10000)$data
todies.do.ps<-occ_search(country = "DO", taxonKey = 2475383, basisOfRecord = "PRESERVED_SPECIMEN", limit = 10000)$data

#Haiti
todies.ht.eb<-occ_search(country = "HT", taxonKey = 2475383, datasetKey = "4fa7b334-ce0d-4e88-aaae-2e0c138d049e", limit = 10000)$data
todies.ht.in<-occ_search(country = "HT", taxonKey = 2475383, datasetKey = "50c9509d-22c7-4a22-a47d-8c48425ef4a7", limit = 10000)$data
todies.ht.ps<-occ_search(country = "HT", taxonKey = 2475383, basisOfRecord = "PRESERVED_SPECIMEN", limit = 10000)$data

#Cuba
todies.cu.eb<-occ_search(country = "CU", taxonKey = 2475383, datasetKey = "4fa7b334-ce0d-4e88-aaae-2e0c138d049e", limit = 10000)$data
todies.cu.in<-occ_search(country = "CU", taxonKey = 2475383, datasetKey = "50c9509d-22c7-4a22-a47d-8c48425ef4a7", limit = 10000)$data
todies.cu.ps<-occ_search(country = "CU", taxonKey = 2475383, basisOfRecord = "PRESERVED_SPECIMEN", limit = 10000)$data

#Puerto Rico
todies.pr.eb<-occ_search(country = "PR", taxonKey = 2475383, datasetKey = "4fa7b334-ce0d-4e88-aaae-2e0c138d049e", limit = 15000)$data
todies.pr.in<-occ_search(country = "PR", taxonKey = 2475383, datasetKey = "50c9509d-22c7-4a22-a47d-8c48425ef4a7", limit = 10000)$data
todies.pr.ps<-occ_search(country = "PR", taxonKey = 2475383, basisOfRecord = "PRESERVED_SPECIMEN", limit = 10000)$data

#Jamaica
todies.jm.eb<-occ_search(country = "JM", taxonKey = 2475383, datasetKey = "4fa7b334-ce0d-4e88-aaae-2e0c138d049e", limit = 10000)$data
todies.jm.in<-occ_search(country = "JM", taxonKey = 2475383, datasetKey = "50c9509d-22c7-4a22-a47d-8c48425ef4a7", limit = 10000)$data
todies.jm.ps<-occ_search(country = "JM", taxonKey = 2475383, basisOfRecord = "PRESERVED_SPECIMEN", limit = 10000)$data


#binding all together
todies.do<-bind_rows(todies.do.eb, todies.do.in, todies.do.ps)
todies.ht<-bind_rows(todies.ht.eb, todies.ht.in, todies.ht.ps)
todies.cu<-bind_rows(todies.cu.eb, todies.cu.in, todies.cu.ps)
todies.pr<-bind_rows(todies.pr.eb, todies.pr.in, todies.pr.ps)
todies.jm<-bind_rows(todies.jm.eb, todies.jm.in, todies.jm.ps)

todies.all<-bind_rows(todies.do, todies.ht, todies.cu, todies.pr, todies.jm)
eb.all<-bind_rows(todies.do.eb, todies.ht.eb, todies.cu.eb, todies.pr.eb, todies.jm.eb)
in.all<-bind_rows(todies.do.in, todies.ht.in, todies.cu.in, todies.pr.in, todies.jm.in)
ps.all<-bind_rows(todies.do.ps, todies.ht.ps, todies.cu.ps, todies.pr.ps, todies.jm.ps)


#Map
map('worldHires', c('Cuba', 'Haiti', 'Puerto Rico', 'Dominican Republic', 'Jamaica'), fill=TRUE, col = "white") 
points(eb.all$decimalLongitude, eb.all$decimalLatitude, pch=21, col="cadetblue4", cex=.3, lwd=.3)
points(in.all$decimalLongitude, in.all$decimalLatitude, pch=21, col="cadetblue3", cex=.3, lwd=.3)
points(ps.all$decimalLongitude, ps.all$decimalLatitude, pch=8, col="chartreuse", cex=.3, lwd=.3)


#Histogram
do.t.eb<-todies.do.eb %>% 
  select(country, year, datasetKey)
do.t.in<-todies.do.in %>% 
  select(country, year, datasetKey)
do.t.ps<-todies.do.ps %>% 
  select(country, year, basisOfRecord)

do.todies<-bind_rows(do.t.eb, do.t.in, do.t.ps)

ht.t.eb<-todies.ht.eb %>% 
  select(country, year, datasetKey)
ht.t.in<-todies.ht.in %>% 
  select(country, year, datasetKey)
ht.t.ps<-todies.ht.ps %>% 
  select(country, year, basisOfRecord)

ht.todies<-bind_rows(ht.t.eb, ht.t.in, ht.t.ps)

cu.t.eb<-todies.cu.eb %>% 
  select(country, year, datasetKey)
cu.t.in<-todies.cu.in %>% 
  select(country, year, datasetKey)
cu.t.ps<-todies.cu.ps %>% 
  select(country, year, basisOfRecord)

cu.todies<-bind_rows(cu.t.eb, cu.t.in, cu.t.ps)

pr.t.eb<-todies.pr.eb %>% 
  select(country, year, datasetKey)
pr.t.in<-todies.pr.in %>% 
  select(country, year, datasetKey)
pr.t.ps<-todies.pr.ps %>% 
  select(country, year, basisOfRecord)

pr.todies<-bind_rows(pr.t.eb, pr.t.in, pr.t.ps)

jm.t.eb<-todies.jm.eb %>% 
  select(country, year, datasetKey)
jm.t.in<-todies.jm.in %>% 
  select(country, year, datasetKey)
jm.t.ps<-todies.jm.ps %>% 
  select(country, year, basisOfRecord)

jm.todies<-bind_rows(jm.t.eb, jm.t.in, jm.t.ps)

todies.hist.all<-bind_rows(do.todies, ht.todies, cu.todies, pr.todies, jm.todies)


todies.histogram<-todies.hist.all %>% 
  mutate(basis.of.info = ifelse(is.na(basisOfRecord), ifelse(datasetKey=="4fa7b334-ce0d-4e88-aaae-2e0c138d049e", "eBird", "iNaturalist"), "Preserved Specimen"))

todies.histogram %>% 
  ggplot()+
  geom_bar(aes(x=year, fill=basis.of.info))+
  theme_classic()+
  labs(x="Year", y="Number of Occurrences", fill="Basis of Observation")+
  theme(legend.position = c(.25, .75))


#Pie Charts
#cuba
cu.eb.t<-occ_count(country = "CU", taxonKey = 2475383, datasetKey = "4fa7b334-ce0d-4e88-aaae-2e0c138d049e")
cu.in.t<-occ_count(country = "CU", taxonKey = 2475383, datasetKey = "50c9509d-22c7-4a22-a47d-8c48425ef4a7")
cu.ps.t<-occ_count(country = "CU", taxonKey = 2475383, basisOfRecord = "PRESERVED_SPECIMEN")

cu.slices<-c(cu.eb.t, cu.in.t, cu.ps.t)
#lbls<-c("eBird", "iNaturalist", "Preserved Specimens")
lbls.cu<-c(paste0("eBird", " (", cu.eb.t, ")"), paste0("iNaturalist", " (", cu.in.t, ")"), paste0("Preserved Specimens", " (", cu.ps.t, ")"))
pie(cu.slices, labels = lbls.cu, main = "Occurrences from the Genus Todus in Cuba")

#Jamaica
jm.eb.t<-occ_count(country = "JM", taxonKey = 2475383, datasetKey = "4fa7b334-ce0d-4e88-aaae-2e0c138d049e")
jm.in.t<-occ_count(country = "JM", taxonKey = 2475383, datasetKey = "50c9509d-22c7-4a22-a47d-8c48425ef4a7")
jm.ps.t<-occ_count(country = "JM", taxonKey = 2475383, basisOfRecord = "PRESERVED_SPECIMEN")

jm.slices<-c(jm.eb.t, jm.in.t, jm.ps.t)
lbls.jm<-c(paste0("eBird", " (", jm.eb.t, ")"), paste0("iNaturalist", " (", jm.in.t, ")"), paste0("Preserved Specimens", " (", jm.ps.t, ")"))
pie(jm.slices, labels = lbls.jm, main = "Occurrences from the Genus Todus in Jamaica")

#Puerto Rico
pr.eb.t<-occ_count(country = "PR", taxonKey = 2475383, datasetKey = "4fa7b334-ce0d-4e88-aaae-2e0c138d049e")
pr.in.t<-occ_count(country = "PR", taxonKey = 2475383, datasetKey = "50c9509d-22c7-4a22-a47d-8c48425ef4a7")
pr.ps.t<-occ_count(country = "PR", taxonKey = 2475383, basisOfRecord = "PRESERVED_SPECIMEN")

pr.slices<-c(pr.eb.t, pr.in.t, pr.ps.t)
lbls.pr<-c(paste0("eBird", " (", pr.eb.t, ")"), paste0("iNaturalist", " (", pr.in.t, ")"), paste0("Preserved Specimens", " (", pr.ps.t, ")"))
pie(pr.slices, labels = lbls.pr, main = "Occurrences from the Genus Todus in Puerto Rico")

#Haiti
ht.eb.t<-occ_count(country = "HT", taxonKey = 2475383, datasetKey = "4fa7b334-ce0d-4e88-aaae-2e0c138d049e")
ht.in.t<-occ_count(country = "HT", taxonKey = 2475383, datasetKey = "50c9509d-22c7-4a22-a47d-8c48425ef4a7")
ht.ps.t<-occ_count(country = "HT", taxonKey = 2475383, basisOfRecord = "PRESERVED_SPECIMEN")

ht.slices<-c(ht.eb.t, ht.in.t, ht.ps.t)
lbls.ht<-c(paste0("eBird", " (", ht.eb.t, ")"), paste0("iNaturalist", " (", ht.in.t, ")"), paste0("Preserved Specimens", " (", ht.ps.t, ")"))
pie(ht.slices, labels = lbls.ht, main = "Occurrences from the Genus Todus in Haiti")

#Dominican Republic
do.eb.t<-occ_count(country = "DO", taxonKey = 2475383, datasetKey = "4fa7b334-ce0d-4e88-aaae-2e0c138d049e")
do.in.t<-occ_count(country = "DO", taxonKey = 2475383, datasetKey = "50c9509d-22c7-4a22-a47d-8c48425ef4a7")
do.ps.t<-occ_count(country = "DO", taxonKey = 2475383, basisOfRecord = "PRESERVED_SPECIMEN")

do.slices<-c(do.eb.t, do.in.t, do.ps.t)
lbls.do<-c(paste0("eBird", " (", do.eb.t, ")"), paste0("iNaturalist", " (", do.in.t, ")"), paste0("Preserved Specimens", " (", do.ps.t, ")"))
pie(do.slices, labels = lbls.do, main = "Occurrences from the Genus Todus in the Dominican Republic")




