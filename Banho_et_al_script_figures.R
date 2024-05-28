library(readxl)
library(gridExtra)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(dplyr)
library(ggalluvial)
library(treeio)
library(seraphim)
library(ggtree)
library(tidyverse)
library(tidytree)
library(readxl)
library(ggalluvial)

###Figure 1###

#Download shape files from: https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html


RHD_XV <- read_sf("/home/user/Downloads/SP_Municipios_2022/SP_Municipios_2022.shp")

map1=dplyr::full_join(RHD_XV, COVID.19_Total_cases_per_city, by ="NM_MUN")
ggplot(data = map1) +
  geom_sf(aes(fill = Group, color=Group)) + theme_classic() + scale_color_manual(values= c("khaki3", "red4", "violetred4", "#C4DFE6" ,"coral3", "#99B898", "#07575B", "#8E7970","#FFCCBB"), na.value="grey87")+
  scale_fill_manual(values= c("khaki3", "red4", "violetred4", "#C4DFE6" ,"coral3", "#99B898", "#07575B", "#8E7970","#FFCCBB"), na.value="grey87")


map2=dplyr::full_join(RHD_XV, Sequenced_genomes_per_city, by ="NM_MUN")
ggplot(data = map2) +
  geom_sf(aes(fill = Number.of.sequenced.genomes, color=Number.of.sequenced.genomes)) + theme_classic() +scale_color_gradientn(colours =c("#99B898", "#FFCCBB", "coral3", "violetred4"),breaks=seq(0,120000,1000),na.value="grey87")+
  scale_fill_gradientn(colours =c("#99B898", "#FFCCBB", "coral3", "violetred4"),breaks=seq(0,120000,1000),na.value="grey87")


####Figure 2####

mycols <- c("khaki3", "#C4DFE6" ,"coral3", "#07575B", "#8E7970","#FFCCBB") 

Percentage.of.circulating.lineages.in.Brazil$date <- as.Date(Percentage.of.circulating.lineages.in.Brazil$date)
Percentage.of.circulating.lineages.in.Brazil %>% 
  ggplot(aes(date, percentage, fill = lineage)) +geom_area() +scale_fill_manual(values = mycols,name = NULL) + scale_x_date(date_labels = "%B-%Y", date_breaks = "1 month") +theme_bw()+ 
  theme(axis.text.x= element_text(size=10,hjust = 1,vjust=0.5, angle=90))



Percentage.of.circulating.lineages.in.RHD.XV$date <- as.Date(Percentage.of.circulating.lineages.in.RHD.XV$date)
Percentage.of.circulating.lineages.in.RHD.XV %>% 
  ggplot(aes(date, percentage, fill = lineage)) +geom_area() +scale_fill_manual(values = mycols,name = NULL) + scale_x_date(date_labels = "%B-%Y", date_breaks = "1 month") +theme_bw()+ 
  theme(axis.text.x= element_text(size=10,hjust = 1,vjust=0.5, angle=90))



mycols1 <- c("violetred4", "#99B898","coral3", "#07575B","#FFCCBB")

Vaccination.data.Brazil$date <- as.Date(Vaccination.data.Brazil$date)
ggplot(data=Vaccination.data.Brazil, aes(x=date, y=value, fill=vaccination)) +theme_bw()+
  geom_bar(stat="identity", width = 1) + scale_x_date(date_labels = "%B-%Y", date_breaks = "1 month") + scale_fill_manual(values = mycols1)+
  theme(axis.text.x= element_text(size=10,hjust = 1,vjust=0.5, angle=90))



mycols2 <- c("violetred4", "#FFCCBB", "#99B898","coral3", "#07575B")

Vaccination.data.RHD.XV$date <- as.Date(Vaccination.data.RHD.XV$date)
ggplot(data=Vaccination.data.RHD.XV, aes(x=date, y=value, fill=vaccination)) + theme_bw()+
  geom_bar(stat="identity",  width=6) + scale_x_date(date_labels = "%B-%Y", date_breaks = "1 month") + scale_fill_manual(values = mycols2)+
  theme(axis.text.x= element_text(size=10,hjust = 1,vjust=0.5, angle=90))

## Data from notified and confirmed COVID-19 cases in Brazil were provided by the Brazilian Ministry of Health, available at https://github.com/wcota/covid19br 

##Moving average of cases and deaths - Brazil##
Brazil_total_cases_and_deaths$date<- as.Date(Brazil_total_cases_and_deaths$date, format="%Y-%m-%d")
Brazil_total_cases_and_deaths<- Brazil_total_cases_and_deaths %>% dplyr::select("date","newCases","newDeaths")
Brazil_total_cases_and_deaths<- plyr::ddply(Brazil_total_cases_and_deaths, plyr::.(date), summarize, cases=sum(newCases), deaths=sum(newDeaths))

Brazil_total_cases_and_deaths <- Brazil_total_cases_and_deaths %>% 
  dplyr::mutate(cases_7day = zoo::rollmean(cases, k = 7, fill = NA),
                deaths_7day = zoo::rollmean(deaths, k = 7, fill = NA)) %>% 
  
  dplyr::ungroup()

ggplot(data=Brazil_total_cases_and_deaths, aes(x=date, y=cases_7day)) +
  geom_line()+theme_bw()+scale_x_date(date_breaks = "1 month")


ggplot(data=Brazil_total_cases_and_deaths, aes(x=date, y=deaths_7day)) +
  geom_line()+theme_bw()+scale_x_date(date_breaks = "1 month") 


##Moving average of cases and deaths - RHD XV##
RHD_XV_total_cases_and_deaths$date<- as.Date(RHD_XV_total_cases_and_deaths$date, format="%Y-%m-%d")
RHD_XV_total_cases_and_deaths<- RHD_XV_total_cases_and_deaths %>% dplyr::select("date","newCases","newDeaths")
RHD_XV_total_cases_and_deaths<- plyr::ddply(RHD_XV_total_cases_and_deaths, plyr::.(date), summarize, cases=sum(newCases), deaths=sum(newDeaths))

RHD_XV_total_cases_and_deaths <- RHD_XV_total_cases_and_deaths %>% 
  dplyr::mutate(cases_7day = zoo::rollmean(cases, k = 7, fill = NA),
                deaths_7day = zoo::rollmean(deaths, k = 7, fill = NA)) %>% 
  
  dplyr::ungroup()

ggplot(data=RHD_XV_total_cases_and_deaths, aes(x=date, y=cases_7day)) +
  geom_line()+theme_bw()+scale_x_date(date_breaks = "1 month")

ggplot(data=RHD_XV_total_cases_and_deaths, aes(x=date, y=deaths_7day)) +
  geom_line()+theme_bw()+scale_x_date(date_breaks = "1 month")



####Figure 5####

tree_1 <- read.nexus("~/Brazil_RHDXV_ML_tree.nexus")

p<-ggtree(tree_1, mrsd="2022-04-28", as.Date=TRUE,color='grey70',size=0.3) %<+% metada + theme_tree2() 

p1<- p+scale_color_manual(values= c("khaki3", "black", "violetred4", "#C4DFE6" ,"coral3", "#99B898", "#07575B", "#8E7970","#FFCCBB"),na.value="grey90") +
  geom_tippoint(aes(color=lineage, fill=lineage, shape=region),size=4, alpha= 0.8, stroke=0.2) +
  scale_x_date(date_labels = "%B-%Y",date_breaks = "2 month") +
  theme(axis.text=element_text(size=10)) + scale_shape_manual(values=c(8,18, 19)) +
  theme(axis.text.x = element_text(size=10,hjust = 1,vjust=0.5, angle=90))+
  guides(fill = guide_legend(override.aes = list(size=5)))
p1

tree <- read.nexus("~/RHDXV_SJdRP_ML_tree.nexus")

p<-ggtree(tree, mrsd="2022-04-28", as.Date=TRUE,color='grey70',size=0.3) %<+% metada + theme_tree2() 

p2<-p+scale_color_manual(values= c("khaki3", "black", "violetred4", "#C4DFE6" ,"coral3", "#99B898", "#07575B", "#8E7970","#FFCCBB"), name='SARS-CoV-2 lineages',na.value="grey90") +
  geom_tippoint(aes(color=lineage, fill=lineage, shape=drs),size=4, alpha= 0.8, stroke=0.2) +
  scale_x_date(date_labels = "%B-%Y",date_breaks = "2 month") +
  theme(axis.text=element_text(size=10)) + scale_shape_manual(values=c(8,19, 15)) +
  theme(axis.text.x = element_text(size=10,hjust = 1,vjust=0.5, angle=90))+
  guides(fill = guide_legend(override.aes = list(size=5)))
p2

ggplot(Tempest_Brazil_RHDXV, aes(date, distance, color=lineage))+
  geom_point(size = 3.5, alpha= 0.6)+
  geom_smooth(method=lm,se=T, colour="black")+
  theme_bw()+
  ylab("Root-to-tip-distances")+
  xlab("Time (years)")+
  ggtitle("SARS-CoV-2 genomes")+
  annotate("text", fontface =1,x=2020.1, y=0.004, label="Correlation cofficient = 0.84", col='black')+
  annotate("text", fontface =1,x=2020.0, y=0.0035, label="R2=0.70", col='black') + scale_color_manual(values= c("khaki3", "violetred4", "#C4DFE6" ,"coral3", "#99B898", "#07575B", "#8E7970","#FFCCBB"))


ggplot(Tempest_RHDXV_SJdRP, aes(date, distance, color=lineage))+
  geom_point(size = 3.5, alpha= 0.6)+
  geom_smooth(method=lm,se=T, colour="black")+
  theme_bw()+
  ylab("Root-to-tip-distances")+
  xlab("Time (years)")+
  ggtitle("SARS-CoV-2 genomes")+
  annotate("text", fontface =1,x=2020.1, y=0.004, label="Correlation cofficient = 0.83", col='black')+
  annotate("text", fontface =1,x=2020.0, y=0.0035, label="R2=0.69", col='black') + scale_color_manual(values= c("khaki3", "violetred4", "#C4DFE6" ,"coral3", "#99B898", "#07575B", "#8E7970","#FFCCBB"))


###Figure 6###

ggplot(data =  migration_RHDXV_Brazil,
       aes(axis1 = source, axis2 = target, y = value)) +
  geom_alluvium(aes(fill = source)) +
  geom_stratum(aes(fill = source)) +  geom_text(stat = "stratum",
                                                aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Source", "Target"),
                   expand = c(0.15, 0.05)) +
  scale_fill_manual(values = mycols) +
  theme_void()


###phylogeography###

setwd("F:/analysis/")


### Gamma Brazil ###

# 1. Extracting the spatio-temporal information contained in posterior trees

treefile <- "F:/analysis/Gamma_Brazil.trees"

localTreesDirectory = "F:/analysis/Gamma_Brazil_Tree_extractions/"
allTrees = scan(file=treefile, what="", sep="\n", quiet=T)
burnIn = 0
randomSampling = FALSE
nberOfTreesToSample = 100
mostRecentSamplingDatum = 2022.13
coordinateAttributeName = "location"

treeExtractions(localTreesDirectory, allTrees, burnIn, randomSampling, nberOfTreesToSample, mostRecentSamplingDatum, coordinateAttributeName)

# 2. Extracting the spatio-temporal information embedded in the MCC tree

treefile<- "F:/analysis/Gamma_Brazil_mcc.tree"
source("F:/analysis/mccExtractions.r")
mcc_tre = readAnnotatedNexus(treefile)
mcc_tab = mccExtractions(mcc_tre, mostRecentSamplingDatum)
write.csv(mcc_tab, "F:/analysis/Gamma_Brazil_mcc.csv", row.names=F, quote=F)


# 3. Estimating the HPD region for each time slice
nberOfExtractionFiles = nberOfTreesToSample
prob = 0.95; precision = 0.025
startDatum = min(mcc_tab[,"startYear"])

polygons = suppressWarnings(spreadGraphic2(localTreesDirectory, nberOfExtractionFiles, prob, startDatum, precision))


# 4.1 spatial boundaries
my_spdf <- readOGR(
  dsn= paste0("F:/analysis/TM_WORLD_BORDERS_SIMPL-0.3/") ,
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

which(my_spdf@data$NAME=="Brazil")
BRAZILBD <- my_spdf[my_spdf@data$NAME=="Brazil" , ]
template_raster=BRAZILBD
borders = crop(getData("GADM", country="BRA", level=1), extent(template_raster))
plot(template_raster, lwd=1, border="gray50")
plot(borders, add=T, lwd=1, border="gray50")


minYear = min(mcc_tab[,"startYear"]); maxYear = max(mcc_tab[,"endYear"])
endYears_indices = (((mcc_tab[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
mcc_tab_this<- mcc_tab %>% data.frame()
mcc_tab_this<- mcc_tab_this %>% dplyr::filter(startYear>=minYear) %>% dplyr::filter(endYear<=maxYear)
endYears_indices = (((mcc_tab[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
min(mcc_tab_this[,"startYear"])
max(mcc_tab_this[,"endYear"])

n_number_colours_needed<- max(round(endYears_indices))
n_repeats_discrete<- 10
c1<- rev(brewer.pal(6,"Spectral"))
c2<- (brewer.pal(3,"PuRd"))
colours<- rev(rep(c(c1,c2), each=n_repeats_discrete))
colour_scale<- colorRampPalette(colours)(n_number_colours_needed)
endYears_colours = colour_scale[endYears_indices]
polygons_colours = rep(NA, length(polygons))

for (i in 1:length(polygons))
{
  date = as.numeric(names(polygons[[i]]))
  polygon_index = round((((date-minYear)/(maxYear-minYear))*100)+1)
  polygons_colours[i] = paste0(colour_scale[polygon_index],"55")
}

# 5. Generating the dispersal history plot

ptsize<- 1
pitjit<- 0.08
for (i in length(polygons):1)
{
  plot(polygons[[i]], axes=F, col=polygons_colours[i], add=T, border=NA)
}
for (i in 1:dim(mcc_tab_this)[1])
{
  curvedarrow(cbind(mcc_tab_this[i,"startLon"],mcc_tab_this[i,"startLat"]), cbind(mcc_tab_this[i,"endLon"],mcc_tab_this[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=2.5, lty=1, lcol="grey22", arr.pos=FALSE, curve=0.3, dr=NA, endhead=F,  arr.type = "triangle", arr.col = "blue")
  curvedarrow(cbind(mcc_tab_this[i,"startLon"],mcc_tab_this[i,"startLat"]), cbind(mcc_tab_this[i,"endLon"],mcc_tab_this[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=2, lty=1, lcol=endYears_colours[i], arr.pos=FALSE, curve=0.3, dr=NA, endhead=F,  arr.type = "triangle", arr.col = "blue")
}


for (i in dim(mcc_tab_this)[1]:1)
{
  xs<- mcc_tab_this[i,"startLon"]
  ys<- mcc_tab_this[i,"startLat"]
  xe<- jitter(mcc_tab_this[i,"endLon"],pitjit)
  ye<- jitter(mcc_tab_this[i,"endLat"],pitjit)
  if (i == 1)
  {
    points(xs, ys, pch=16, col=colour_scale[1], cex=ptsize)
    points(xs, ys, pch=1, col="gray10", cex=ptsize)
  }
  points(xe, ye, pch=16, col=endYears_colours[i], cex=ptsize)
  points(xe, ye, pch=1, col="gray10", cex=ptsize)
}

for (i in dim(mcc_tab)[1]:1)
{
  if (i == 1)
  {
    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"], pch=16, col=colour_scale[1], cex=1.35)
    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"], pch=1, col="gray10", cex=1.3)
  }
  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"], pch=16, col=endYears_colours[i], cex=1.3)
  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"], pch=1, col="gray10", cex=1.3)
}


xrange<- c(xmin(template_raster), xmax(template_raster))
yrange<- c(ymin(template_raster), ymax(template_raster))
rect(xrange[1], yrange[1], xrange[2], yrange[2], xpd=T, lwd=0.2)
axis(1, c(ceiling(xmin(template_raster)), floor(xmax(template_raster))), pos=ymin(template_raster), mgp=c(0,0.2,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=-0.8, tck=-0.01, col.axis="gray30")
axis(2, c(ceiling(ymin(template_raster)), floor(ymax(template_raster))), pos=xmin(template_raster), mgp=c(0,0.5,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=1, tck=-0.01, col.axis="gray30")
rast = raster(matrix(nrow=1, ncol=2)); rast[1] = min(mcc_tab_this[,"startYear"]); rast[2] = max(mcc_tab_this[,"endYear"])
plot(rast, legend.only=T, add=T, col=colour_scale, legend.width=0.5, legend.shrink=0.3, smallplot=c(0.40,0.80,0.14,0.155),
     legend.args=list(text="", cex=0.7, line=0.3, col="black"), horizontal=T,
     axis.args=list(cex.axis=0.6, lwd=0, lwd.tick=0.2, tck=-0.5, col.axis="black", line=0, mgp=c(0,-0.02,0)))


### Gamma RHD XV ###

# 1. Extracting the spatio-temporal information contained in posterior trees

treefile <- "F:/analysis/Gamma_RHDXV.trees"

localTreesDirectory = "F:/analysis/Gamma_RHDXV_Tree_extractions/"
allTrees = scan(file=treefile, what="", sep="\n", quiet=T)
burnIn = 0
randomSampling = FALSE
nberOfTreesToSample = 100
mostRecentSamplingDatum = 2021.75
coordinateAttributeName = "location"

treeExtractions(localTreesDirectory, allTrees, burnIn, randomSampling, nberOfTreesToSample, mostRecentSamplingDatum, coordinateAttributeName)

# 2. Extracting the spatio-temporal information embedded in the MCC tree

treefile<- "F:/analysis/Gamma_RHDXV_mcc.tree"
source("F:/analysis/mccExtractions.r")
mcc_tre = readAnnotatedNexus(treefile)
mcc_tab = mccExtractions(mcc_tre, mostRecentSamplingDatum)
write.csv(mcc_tab, "F:/analysis/Gamma_RHDXV_mcc.csv", row.names=F, quote=F)


# 3. Estimating the HPD region for each time slice
nberOfExtractionFiles = nberOfTreesToSample
prob = 0.95; precision = 0.025
startDatum = min(mcc_tab[,"startYear"])

polygons = suppressWarnings(spreadGraphic2(localTreesDirectory, nberOfExtractionFiles, prob, startDatum, precision))


# 4.1 spatial boundaries

RHDXV <- readOGR(
  dsn= paste0("F:/analysis/RHDXV") ,
  layer="RHDXV",
  verbose=FALSE
)

template_raster=RHDXV
plot(template_raster, lwd=1, border="gray50")

minYear = min(mcc_tab[,"startYear"]); maxYear = max(mcc_tab[,"endYear"])
endYears_indices = (((mcc_tab[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
mcc_tab_this<- mcc_tab %>% data.frame()
mcc_tab_this<- mcc_tab_this %>% dplyr::filter(startYear>=minYear) %>% dplyr::filter(endYear<=maxYear)
endYears_indices = (((mcc_tab[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
min(mcc_tab_this[,"startYear"])
max(mcc_tab_this[,"endYear"])

n_number_colours_needed<- max(round(endYears_indices))
n_repeats_discrete<- 10
c1<- rev(brewer.pal(6,"Spectral"))
c2<- (brewer.pal(3,"PuRd"))
colours<- rev(rep(c(c1,c2), each=n_repeats_discrete))
colour_scale<- colorRampPalette(colours)(n_number_colours_needed)
endYears_colours = colour_scale[endYears_indices]
polygons_colours = rep(NA, length(polygons))

for (i in 1:length(polygons))
{
  date = as.numeric(names(polygons[[i]]))
  polygon_index = round((((date-minYear)/(maxYear-minYear))*100)+1)
  polygons_colours[i] = paste0(colour_scale[polygon_index],"55")
}

# 5. Generating the dispersal history plot

ptsize<- 1
pitjit<- 0.08
for (i in length(polygons):1)
{
  plot(polygons[[i]], axes=F, col=polygons_colours[i], add=T, border=NA)
}
for (i in 1:dim(mcc_tab_this)[1])
{
  curvedarrow(cbind(mcc_tab_this[i,"startLon"],mcc_tab_this[i,"startLat"]), cbind(mcc_tab_this[i,"endLon"],mcc_tab_this[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=2.5, lty=1, lcol="grey22", arr.pos=FALSE, curve=0.3, dr=NA, endhead=F,  arr.type = "triangle", arr.col = "blue")
  curvedarrow(cbind(mcc_tab_this[i,"startLon"],mcc_tab_this[i,"startLat"]), cbind(mcc_tab_this[i,"endLon"],mcc_tab_this[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=2, lty=1, lcol=endYears_colours[i], arr.pos=FALSE, curve=0.3, dr=NA, endhead=F,  arr.type = "triangle", arr.col = "blue")
}


for (i in dim(mcc_tab_this)[1]:1)
{
  xs<- mcc_tab_this[i,"startLon"]
  ys<- mcc_tab_this[i,"startLat"]
  xe<- jitter(mcc_tab_this[i,"endLon"],pitjit)
  ye<- jitter(mcc_tab_this[i,"endLat"],pitjit)
  if (i == 1)
  {
    points(xs, ys, pch=16, col=colour_scale[1], cex=ptsize)
    points(xs, ys, pch=1, col="gray10", cex=ptsize)
  }
  points(xe, ye, pch=16, col=endYears_colours[i], cex=ptsize)
  points(xe, ye, pch=1, col="gray10", cex=ptsize)
}

for (i in dim(mcc_tab)[1]:1)
{
  if (i == 1)
  {
    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"], pch=16, col=colour_scale[1], cex=1.35)
    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"], pch=1, col="gray10", cex=1.3)
  }
  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"], pch=16, col=endYears_colours[i], cex=1.3)
  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"], pch=1, col="gray10", cex=1.3)
}


xrange<- c(xmin(template_raster), xmax(template_raster))
yrange<- c(ymin(template_raster), ymax(template_raster))
rect(xrange[1], yrange[1], xrange[2], yrange[2], xpd=T, lwd=0.2)
axis(1, c(ceiling(xmin(template_raster)), floor(xmax(template_raster))), pos=ymin(template_raster), mgp=c(0,0.2,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=-0.8, tck=-0.01, col.axis="gray30")
axis(2, c(ceiling(ymin(template_raster)), floor(ymax(template_raster))), pos=xmin(template_raster), mgp=c(0,0.5,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=1, tck=-0.01, col.axis="gray30")
rast = raster(matrix(nrow=1, ncol=2)); rast[1] = min(mcc_tab_this[,"startYear"]); rast[2] = max(mcc_tab_this[,"endYear"])
plot(rast, legend.only=T, add=T, col=colour_scale, legend.width=0.5, legend.shrink=0.3, smallplot=c(0.40,0.80,0.14,0.155),
     legend.args=list(text="", cex=0.7, line=0.3, col="black"), horizontal=T,
     axis.args=list(cex.axis=0.6, lwd=0, lwd.tick=0.2, tck=-0.5, col.axis="black", line=0, mgp=c(0,-0.02,0)))


### Delta Brazil ###

# 1. Extracting the spatio-temporal information contained in posterior trees

treefile <- "F:/analysis/Delta_Brazil.trees"

localTreesDirectory = "F:/analysis/Delta_Brazil_Tree_extractions/"
allTrees = scan(file=treefile, what="", sep="\n", quiet=T)
burnIn = 0
randomSampling = FALSE
nberOfTreesToSample = 100
mostRecentSamplingDatum = 2022.14
coordinateAttributeName = "location"

treeExtractions(localTreesDirectory, allTrees, burnIn, randomSampling, nberOfTreesToSample, mostRecentSamplingDatum, coordinateAttributeName)

# 2. Extracting the spatio-temporal information embedded in the MCC tree

treefile<- "F:/analysis/Delta_Brazil_mcc.tree"
source("F:/analysis/mccExtractions.r")
mcc_tre = readAnnotatedNexus(treefile)
mcc_tab = mccExtractions(mcc_tre, mostRecentSamplingDatum)
write.csv(mcc_tab, "F:/analysis/Delta_Brazil_mcc.csv", row.names=F, quote=F)


# 3. Estimating the HPD region for each time slice
nberOfExtractionFiles = nberOfTreesToSample
prob = 0.95; precision = 0.025
startDatum = min(mcc_tab[,"startYear"])

polygons = suppressWarnings(spreadGraphic2(localTreesDirectory, nberOfExtractionFiles, prob, startDatum, precision))


# 4.1 spatial boundaries
my_spdf <- readOGR(
  dsn= paste0("F:/analysis/TM_WORLD_BORDERS_SIMPL-0.3/") ,
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

which(my_spdf@data$NAME=="Brazil")
BRAZILBD <- my_spdf[my_spdf@data$NAME=="Brazil" , ]
template_raster=BRAZILBD
borders = crop(getData("GADM", country="BRA", level=1), extent(template_raster))
plot(template_raster, lwd=1, border="gray50")
plot(borders, add=T, lwd=1, border="gray50")


minYear = min(mcc_tab[,"startYear"]); maxYear = max(mcc_tab[,"endYear"])
endYears_indices = (((mcc_tab[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
mcc_tab_this<- mcc_tab %>% data.frame()
mcc_tab_this<- mcc_tab_this %>% dplyr::filter(startYear>=minYear) %>% dplyr::filter(endYear<=maxYear)
endYears_indices = (((mcc_tab[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
min(mcc_tab_this[,"startYear"])
max(mcc_tab_this[,"endYear"])

n_number_colours_needed<- max(round(endYears_indices))
n_repeats_discrete<- 10
c1<- rev(brewer.pal(6,"Spectral"))
c2<- (brewer.pal(3,"PuRd"))
colours<- rev(rep(c(c1,c2), each=n_repeats_discrete))
colour_scale<- colorRampPalette(colours)(n_number_colours_needed)
endYears_colours = colour_scale[endYears_indices]
polygons_colours = rep(NA, length(polygons))

for (i in 1:length(polygons))
{
  date = as.numeric(names(polygons[[i]]))
  polygon_index = round((((date-minYear)/(maxYear-minYear))*100)+1)
  polygons_colours[i] = paste0(colour_scale[polygon_index],"55")
}

# 5. Generating the dispersal history plot

ptsize<- 1
pitjit<- 0.08
for (i in length(polygons):1)
{
  plot(polygons[[i]], axes=F, col=polygons_colours[i], add=T, border=NA)
}
for (i in 1:dim(mcc_tab_this)[1])
{
  curvedarrow(cbind(mcc_tab_this[i,"startLon"],mcc_tab_this[i,"startLat"]), cbind(mcc_tab_this[i,"endLon"],mcc_tab_this[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=2.5, lty=1, lcol="grey22", arr.pos=FALSE, curve=0.3, dr=NA, endhead=F,  arr.type = "triangle", arr.col = "blue")
  curvedarrow(cbind(mcc_tab_this[i,"startLon"],mcc_tab_this[i,"startLat"]), cbind(mcc_tab_this[i,"endLon"],mcc_tab_this[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=2, lty=1, lcol=endYears_colours[i], arr.pos=FALSE, curve=0.3, dr=NA, endhead=F,  arr.type = "triangle", arr.col = "blue")
}


for (i in dim(mcc_tab_this)[1]:1)
{
  xs<- mcc_tab_this[i,"startLon"]
  ys<- mcc_tab_this[i,"startLat"]
  xe<- jitter(mcc_tab_this[i,"endLon"],pitjit)
  ye<- jitter(mcc_tab_this[i,"endLat"],pitjit)
  if (i == 1)
  {
    points(xs, ys, pch=16, col=colour_scale[1], cex=ptsize)
    points(xs, ys, pch=1, col="gray10", cex=ptsize)
  }
  points(xe, ye, pch=16, col=endYears_colours[i], cex=ptsize)
  points(xe, ye, pch=1, col="gray10", cex=ptsize)
}

for (i in dim(mcc_tab)[1]:1)
{
  if (i == 1)
  {
    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"], pch=16, col=colour_scale[1], cex=1.35)
    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"], pch=1, col="gray10", cex=1.3)
  }
  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"], pch=16, col=endYears_colours[i], cex=1.3)
  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"], pch=1, col="gray10", cex=1.3)
}


xrange<- c(xmin(template_raster), xmax(template_raster))
yrange<- c(ymin(template_raster), ymax(template_raster))
rect(xrange[1], yrange[1], xrange[2], yrange[2], xpd=T, lwd=0.2)
axis(1, c(ceiling(xmin(template_raster)), floor(xmax(template_raster))), pos=ymin(template_raster), mgp=c(0,0.2,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=-0.8, tck=-0.01, col.axis="gray30")
axis(2, c(ceiling(ymin(template_raster)), floor(ymax(template_raster))), pos=xmin(template_raster), mgp=c(0,0.5,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=1, tck=-0.01, col.axis="gray30")
rast = raster(matrix(nrow=1, ncol=2)); rast[1] = min(mcc_tab_this[,"startYear"]); rast[2] = max(mcc_tab_this[,"endYear"])
plot(rast, legend.only=T, add=T, col=colour_scale, legend.width=0.5, legend.shrink=0.3, smallplot=c(0.40,0.80,0.14,0.155),
     legend.args=list(text="", cex=0.7, line=0.3, col="black"), horizontal=T,
     axis.args=list(cex.axis=0.6, lwd=0, lwd.tick=0.2, tck=-0.5, col.axis="black", line=0, mgp=c(0,-0.02,0)))


### Delta RHD XV ###

# 1. Extracting the spatio-temporal information contained in posterior trees

treefile <- "F:/analysis/Delta_RHDXV.trees"

localTreesDirectory = "F:/analysis/Delta_RHDXV_Tree_extractions/"
allTrees = scan(file=treefile, what="", sep="\n", quiet=T)
burnIn = 0
randomSampling = FALSE
nberOfTreesToSample = 100
mostRecentSamplingDatum = 2022.00
coordinateAttributeName = "location"

treeExtractions(localTreesDirectory, allTrees, burnIn, randomSampling, nberOfTreesToSample, mostRecentSamplingDatum, coordinateAttributeName)

# 2. Extracting the spatio-temporal information embedded in the MCC tree

treefile<- "F:/analysis/Delta_RHDXV_mcc.tree"
source("F:/analysis/mccExtractions.r")
mcc_tre = readAnnotatedNexus(treefile)
mcc_tab = mccExtractions(mcc_tre, mostRecentSamplingDatum)
write.csv(mcc_tab, "F:/analysis/Delta_RHDXV_mcc.csv", row.names=F, quote=F)

# 3. Estimating the HPD region for each time slice
nberOfExtractionFiles = nberOfTreesToSample
prob = 0.95; precision = 0.025
startDatum = min(mcc_tab[,"startYear"])

polygons = suppressWarnings(spreadGraphic2(localTreesDirectory, nberOfExtractionFiles, prob, startDatum, precision))

# 4.1 spatial boundaries

RHDXV <- readOGR(
  dsn= paste0("F:/analysis/RHDXV") ,
  layer="RHDXV",
  verbose=FALSE
)

template_raster=RHDXV
plot(template_raster, lwd=1, border="gray50")
plot(borders, add=T, lwd=0.1, border="gray57")

minYear = min(mcc_tab[,"startYear"]); maxYear = max(mcc_tab[,"endYear"])
endYears_indices = (((mcc_tab[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
mcc_tab_this<- mcc_tab %>% data.frame()
mcc_tab_this<- mcc_tab_this %>% dplyr::filter(startYear>=minYear) %>% dplyr::filter(endYear<=maxYear)
endYears_indices = (((mcc_tab[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
min(mcc_tab_this[,"startYear"])
max(mcc_tab_this[,"endYear"])

n_number_colours_needed<- max(round(endYears_indices))
n_repeats_discrete<- 10
c1<- rev(brewer.pal(6,"Spectral"))
c2<- (brewer.pal(3,"PuRd"))
colours<- rev(rep(c(c1,c2), each=n_repeats_discrete))
colour_scale<- colorRampPalette(colours)(n_number_colours_needed)
endYears_colours = colour_scale[endYears_indices]
polygons_colours = rep(NA, length(polygons))

for (i in 1:length(polygons))
{
  date = as.numeric(names(polygons[[i]]))
  polygon_index = round((((date-minYear)/(maxYear-minYear))*100)+1)
  polygons_colours[i] = paste0(colour_scale[polygon_index],"55")
}

# 5. Generating the dispersal history plot

ptsize<- 1
pitjit<- 0.08
for (i in length(polygons):1)
{
  plot(polygons[[i]], axes=F, col=polygons_colours[i], add=T, border=NA)
}
for (i in 1:dim(mcc_tab_this)[1])
{
  curvedarrow(cbind(mcc_tab_this[i,"startLon"],mcc_tab_this[i,"startLat"]), cbind(mcc_tab_this[i,"endLon"],mcc_tab_this[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=2.5, lty=1, lcol="grey22", arr.pos=FALSE, curve=0.3, dr=NA, endhead=F,  arr.type = "triangle", arr.col = "blue")
  curvedarrow(cbind(mcc_tab_this[i,"startLon"],mcc_tab_this[i,"startLat"]), cbind(mcc_tab_this[i,"endLon"],mcc_tab_this[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=2, lty=1, lcol=endYears_colours[i], arr.pos=FALSE, curve=0.3, dr=NA, endhead=F,  arr.type = "triangle", arr.col = "blue")
}


for (i in dim(mcc_tab_this)[1]:1)
{
  xs<- mcc_tab_this[i,"startLon"]
  ys<- mcc_tab_this[i,"startLat"]
  xe<- jitter(mcc_tab_this[i,"endLon"],pitjit)
  ye<- jitter(mcc_tab_this[i,"endLat"],pitjit)
  if (i == 1)
  {
    points(xs, ys, pch=16, col=colour_scale[1], cex=ptsize)
    points(xs, ys, pch=1, col="gray10", cex=ptsize)
  }
  points(xe, ye, pch=16, col=endYears_colours[i], cex=ptsize)
  points(xe, ye, pch=1, col="gray10", cex=ptsize)
}

for (i in dim(mcc_tab)[1]:1)
{
  if (i == 1)
  {
    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"], pch=16, col=colour_scale[1], cex=1.35)
    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"], pch=1, col="gray10", cex=1.3)
  }
  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"], pch=16, col=endYears_colours[i], cex=1.3)
  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"], pch=1, col="gray10", cex=1.3)
}


xrange<- c(xmin(template_raster), xmax(template_raster))
yrange<- c(ymin(template_raster), ymax(template_raster))
rect(xrange[1], yrange[1], xrange[2], yrange[2], xpd=T, lwd=0.2)
axis(1, c(ceiling(xmin(template_raster)), floor(xmax(template_raster))), pos=ymin(template_raster), mgp=c(0,0.2,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=-0.8, tck=-0.01, col.axis="gray30")
axis(2, c(ceiling(ymin(template_raster)), floor(ymax(template_raster))), pos=xmin(template_raster), mgp=c(0,0.5,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=1, tck=-0.01, col.axis="gray30")
rast = raster(matrix(nrow=1, ncol=2)); rast[1] = min(mcc_tab_this[,"startYear"]); rast[2] = max(mcc_tab_this[,"endYear"])
plot(rast, legend.only=T, add=T, col=colour_scale, legend.width=0.5, legend.shrink=0.3, smallplot=c(0.40,0.80,0.14,0.155),
     legend.args=list(text="", cex=0.7, line=0.3, col="black"), horizontal=T,
     axis.args=list(cex.axis=0.6, lwd=0, lwd.tick=0.2, tck=-0.5, col.axis="black", line=0, mgp=c(0,-0.02,0)))


### Omicron Brazil ###

# 1. Extracting the spatio-temporal information contained in posterior trees

treefile <- "F:/analysis/Omicron_Brazil.trees"

localTreesDirectory = "F:/analysis/Omicron_Brazil_Tree_extractions/"
allTrees = scan(file=treefile, what="", sep="\n", quiet=T)
burnIn = 0
randomSampling = FALSE
nberOfTreesToSample = 100
mostRecentSamplingDatum = 2022.23
coordinateAttributeName = "location"

treeExtractions(localTreesDirectory, allTrees, burnIn, randomSampling, nberOfTreesToSample, mostRecentSamplingDatum, coordinateAttributeName)

# 2. Extracting the spatio-temporal information embedded in the MCC tree

treefile<- "F:/analysis/Omicron_Brazil_mcc.tree"
source("F:/analysis/mccExtractions.r")
mcc_tre = readAnnotatedNexus(treefile)
mcc_tab = mccExtractions(mcc_tre, mostRecentSamplingDatum)
write.csv(mcc_tab, "F:/analysis/Omicron_Brazil_mcc.csv", row.names=F, quote=F)


# 3. Estimating the HPD region for each time slice
nberOfExtractionFiles = nberOfTreesToSample
prob = 0.95; precision = 0.025
startDatum = min(mcc_tab[,"startYear"])

polygons = suppressWarnings(spreadGraphic2(localTreesDirectory, nberOfExtractionFiles, prob, startDatum, precision))

# 4.1 spatial boundaries
my_spdf <- readOGR(
  dsn= paste0("F:/analysis/TM_WORLD_BORDERS_SIMPL-0.3/") ,
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

which(my_spdf@data$NAME=="Brazil")
BRAZILBD <- my_spdf[my_spdf@data$NAME=="Brazil" , ]
template_raster=BRAZILBD
borders = crop(getData("GADM", country="BRA", level=1), extent(template_raster))
plot(template_raster, lwd=1, border="gray50")
plot(borders, add=T, lwd=1, border="gray50")


minYear = min(mcc_tab[,"startYear"]); maxYear = max(mcc_tab[,"endYear"])
endYears_indices = (((mcc_tab[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
mcc_tab_this<- mcc_tab %>% data.frame()
mcc_tab_this<- mcc_tab_this %>% dplyr::filter(startYear>=minYear) %>% dplyr::filter(endYear<=maxYear)
endYears_indices = (((mcc_tab[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
min(mcc_tab_this[,"startYear"])
max(mcc_tab_this[,"endYear"])

n_number_colours_needed<- max(round(endYears_indices))
n_repeats_discrete<- 10
c1<- rev(brewer.pal(6,"Spectral"))
c2<- (brewer.pal(3,"PuRd"))
colours<- rev(rep(c(c1,c2), each=n_repeats_discrete))
colour_scale<- colorRampPalette(colours)(n_number_colours_needed)
endYears_colours = colour_scale[endYears_indices]
polygons_colours = rep(NA, length(polygons))

for (i in 1:length(polygons))
{
  date = as.numeric(names(polygons[[i]]))
  polygon_index = round((((date-minYear)/(maxYear-minYear))*100)+1)
  polygons_colours[i] = paste0(colour_scale[polygon_index],"55")
}

# 5. Generating the dispersal history plot

ptsize<- 1
pitjit<- 0.08
for (i in length(polygons):1)
{
  plot(polygons[[i]], axes=F, col=polygons_colours[i], add=T, border=NA)
}
for (i in 1:dim(mcc_tab_this)[1])
{
  curvedarrow(cbind(mcc_tab_this[i,"startLon"],mcc_tab_this[i,"startLat"]), cbind(mcc_tab_this[i,"endLon"],mcc_tab_this[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=2.5, lty=1, lcol="grey22", arr.pos=FALSE, curve=0.3, dr=NA, endhead=F,  arr.type = "triangle", arr.col = "blue")
  curvedarrow(cbind(mcc_tab_this[i,"startLon"],mcc_tab_this[i,"startLat"]), cbind(mcc_tab_this[i,"endLon"],mcc_tab_this[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=2, lty=1, lcol=endYears_colours[i], arr.pos=FALSE, curve=0.3, dr=NA, endhead=F,  arr.type = "triangle", arr.col = "blue")
}


for (i in dim(mcc_tab_this)[1]:1)
{
  xs<- mcc_tab_this[i,"startLon"]
  ys<- mcc_tab_this[i,"startLat"]
  xe<- jitter(mcc_tab_this[i,"endLon"],pitjit)
  ye<- jitter(mcc_tab_this[i,"endLat"],pitjit)
  if (i == 1)
  {
    points(xs, ys, pch=16, col=colour_scale[1], cex=ptsize)
    points(xs, ys, pch=1, col="gray10", cex=ptsize)
  }
  points(xe, ye, pch=16, col=endYears_colours[i], cex=ptsize)
  points(xe, ye, pch=1, col="gray10", cex=ptsize)
}

for (i in dim(mcc_tab)[1]:1)
{
  if (i == 1)
  {
    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"], pch=16, col=colour_scale[1], cex=1.35)
    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"], pch=1, col="gray10", cex=1.3)
  }
  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"], pch=16, col=endYears_colours[i], cex=1.3)
  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"], pch=1, col="gray10", cex=1.3)
}


xrange<- c(xmin(template_raster), xmax(template_raster))
yrange<- c(ymin(template_raster), ymax(template_raster))
rect(xrange[1], yrange[1], xrange[2], yrange[2], xpd=T, lwd=0.2)
axis(1, c(ceiling(xmin(template_raster)), floor(xmax(template_raster))), pos=ymin(template_raster), mgp=c(0,0.2,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=-0.8, tck=-0.01, col.axis="gray30")
axis(2, c(ceiling(ymin(template_raster)), floor(ymax(template_raster))), pos=xmin(template_raster), mgp=c(0,0.5,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=1, tck=-0.01, col.axis="gray30")
rast = raster(matrix(nrow=1, ncol=2)); rast[1] = min(mcc_tab_this[,"startYear"]); rast[2] = max(mcc_tab_this[,"endYear"])
plot(rast, legend.only=T, add=T, col=colour_scale, legend.width=0.5, legend.shrink=0.3, smallplot=c(0.40,0.80,0.14,0.155),
     legend.args=list(text="", cex=0.7, line=0.3, col="black"), horizontal=T,
     axis.args=list(cex.axis=0.6, lwd=0, lwd.tick=0.2, tck=-0.5, col.axis="black", line=0, mgp=c(0,-0.02,0)))

### Omicron RHD XV ###

# 1. Extracting the spatio-temporal information contained in posterior trees

treefile <- "F:/analysis/Omicron_RHDXV.trees"

localTreesDirectory = "F:/analysis/Omicron_RHDXV_Tree_extractions/"
allTrees = scan(file=treefile, what="", sep="\n", quiet=T)
burnIn = 0
randomSampling = FALSE
nberOfTreesToSample = 100
mostRecentSamplingDatum = 2022.23
coordinateAttributeName = "location"

treeExtractions(localTreesDirectory, allTrees, burnIn, randomSampling, nberOfTreesToSample, mostRecentSamplingDatum, coordinateAttributeName)

# 2. Extracting the spatio-temporal information embedded in the MCC tree

treefile<- "F:/analysis/Omicron_RHDXV_mcc.tree"
source("F:/analysis/mccExtractions.r")
mcc_tre = readAnnotatedNexus(treefile)
mcc_tab = mccExtractions(mcc_tre, mostRecentSamplingDatum)
write.csv(mcc_tab, "F:/analysis/Omicron_RHDXV_mcc.csv", row.names=F, quote=F)


# 3. Estimating the HPD region for each time slice
nberOfExtractionFiles = nberOfTreesToSample
prob = 0.95; precision = 0.025
startDatum = min(mcc_tab[,"startYear"])

polygons = suppressWarnings(spreadGraphic2(localTreesDirectory, nberOfExtractionFiles, prob, startDatum, precision))


# 4.1 spatial boundaries

RHDXV <- readOGR(
  dsn= paste0("F:/analysis/RHDXV") ,
  layer="RHDXV",
  verbose=FALSE
)

template_raster=RHDXV
plot(template_raster, lwd=1, border="gray50")


minYear = min(mcc_tab[,"startYear"]); maxYear = max(mcc_tab[,"endYear"])
endYears_indices = (((mcc_tab[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
mcc_tab_this<- mcc_tab %>% data.frame()
mcc_tab_this<- mcc_tab_this %>% dplyr::filter(startYear>=minYear) %>% dplyr::filter(endYear<=maxYear)
endYears_indices = (((mcc_tab[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
min(mcc_tab_this[,"startYear"])
max(mcc_tab_this[,"endYear"])

n_number_colours_needed<- max(round(endYears_indices))
n_repeats_discrete<- 10
c1<- rev(brewer.pal(6,"Spectral"))
c2<- (brewer.pal(3,"PuRd"))
colours<- rev(rep(c(c1,c2), each=n_repeats_discrete))
colour_scale<- colorRampPalette(colours)(n_number_colours_needed)
endYears_colours = colour_scale[endYears_indices]
polygons_colours = rep(NA, length(polygons))

for (i in 1:length(polygons))
{
  date = as.numeric(names(polygons[[i]]))
  polygon_index = round((((date-minYear)/(maxYear-minYear))*100)+1)
  polygons_colours[i] = paste0(colour_scale[polygon_index],"55")
}

# 5. Generating the dispersal history plot

ptsize<- 1
pitjit<- 0.08
for (i in length(polygons):1)
{
  plot(polygons[[i]], axes=F, col=polygons_colours[i], add=T, border=NA)
}
for (i in 1:dim(mcc_tab_this)[1])
{
  curvedarrow(cbind(mcc_tab_this[i,"startLon"],mcc_tab_this[i,"startLat"]), cbind(mcc_tab_this[i,"endLon"],mcc_tab_this[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=2.5, lty=1, lcol="grey22", arr.pos=FALSE, curve=0.3, dr=NA, endhead=F,  arr.type = "triangle", arr.col = "blue")
  curvedarrow(cbind(mcc_tab_this[i,"startLon"],mcc_tab_this[i,"startLat"]), cbind(mcc_tab_this[i,"endLon"],mcc_tab_this[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=2, lty=1, lcol=endYears_colours[i], arr.pos=FALSE, curve=0.3, dr=NA, endhead=F,  arr.type = "triangle", arr.col = "blue")
}


for (i in dim(mcc_tab_this)[1]:1)
{
  xs<- mcc_tab_this[i,"startLon"]
  ys<- mcc_tab_this[i,"startLat"]
  xe<- jitter(mcc_tab_this[i,"endLon"],pitjit)
  ye<- jitter(mcc_tab_this[i,"endLat"],pitjit)
  if (i == 1)
  {
    points(xs, ys, pch=16, col=colour_scale[1], cex=ptsize)
    points(xs, ys, pch=1, col="gray10", cex=ptsize)
  }
  points(xe, ye, pch=16, col=endYears_colours[i], cex=ptsize)
  points(xe, ye, pch=1, col="gray10", cex=ptsize)
}

for (i in dim(mcc_tab)[1]:1)
{
  if (i == 1)
  {
    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"], pch=16, col=colour_scale[1], cex=1.35)
    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"], pch=1, col="gray10", cex=1.3)
  }
  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"], pch=16, col=endYears_colours[i], cex=1.3)
  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"], pch=1, col="gray10", cex=1.3)
}


xrange<- c(xmin(template_raster), xmax(template_raster))
yrange<- c(ymin(template_raster), ymax(template_raster))
rect(xrange[1], yrange[1], xrange[2], yrange[2], xpd=T, lwd=0.2)
axis(1, c(ceiling(xmin(template_raster)), floor(xmax(template_raster))), pos=ymin(template_raster), mgp=c(0,0.2,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=-0.8, tck=-0.01, col.axis="gray30")
axis(2, c(ceiling(ymin(template_raster)), floor(ymax(template_raster))), pos=xmin(template_raster), mgp=c(0,0.5,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=1, tck=-0.01, col.axis="gray30")
rast = raster(matrix(nrow=1, ncol=2)); rast[1] = min(mcc_tab_this[,"startYear"]); rast[2] = max(mcc_tab_this[,"endYear"])
plot(rast, legend.only=T, add=T, col=colour_scale, legend.width=0.5, legend.shrink=0.3, smallplot=c(0.40,0.80,0.14,0.155),
     legend.args=list(text="", cex=0.7, line=0.3, col="black"), horizontal=T,
     axis.args=list(cex.axis=0.6, lwd=0, lwd.tick=0.2, tck=-0.5, col.axis="black", line=0, mgp=c(0,-0.02,0)))