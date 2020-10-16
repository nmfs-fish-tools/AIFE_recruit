data.dir <- "./recdata/data/"
petrale <- read.csv(file.path(data.dir,"Petrale_analyzed.data.csv"))
sable <- read.csv(file.path(data.dir,"Sablefish_Analyzed_Data_north.csv"))
sable_DFA <- read.csv(file.path(data.dir,"Sablefish_DFA_Data.csv"))

names(petrale)

#Plot recruits vs year and spawning stock biomass
plot(age.0~year,data=petrale, type="l", main="Recruits per year")
plot(age.0~sp.bio,data=petrale, main="Recruits vs SSB")

require(ggplot2)
require(dplyr)
require(ggcorrplot)

#Plot correlation
round(cor(petrale %>% 
            dplyr::select(names(petrale)[18:37]) %>% data.frame), 1) %>% 
ggcorrplot(tl.cex=6)

