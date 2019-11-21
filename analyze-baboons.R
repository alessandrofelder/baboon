#read data
baboon.data <- read.csv("/media/alessandro/Transcend/baboon-pores/Baboon-AnatSoc-Analysis.csv", row.names=1)

#add info
age.at.death <- c(2.93,1.03,2.45,1.97,1.86,2.41)
sex <- c("M","F","M","F","F","F")
baboon.data <- as.data.frame(t(baboon.data))

baboon.data <- cbind(baboon.data,age.at.death)
baboon.data <- cbind(baboon.data,sex)

#make data numeric
baboon.data$bone <- as.numeric(as.character(baboon.data$bone))

baboon.data$pores <- as.numeric(as.character(baboon.data$pores))
baboon.data$porosity <- as.numeric(as.character(baboon.data$porosity))

baboon.data$`vascular.pores (>8000 voxels)` <- as.numeric(as.character(baboon.data$`vascular.pores (>8000 voxels)`))
baboon.data$vascular.porosity <- as.numeric(as.character(baboon.data$vascular.porosity))

baboon.data$`oc.density(#/ccm bone)` <- as.numeric(as.character(baboon.data$`oc.density(#/ccm bone)`))

#plot data
x <- baboon.data$age.at.death
x.label <- "age at death [years]"

par(mfrow=c(4,1), mar=c(4.5,5,1,1))

plot(baboon.data$porosity*100~x,
     col=baboon.data$sex, 
     xlab=x.label, ylab = expression('porosity [%]'))

plot(baboon.data$vascular.porosity*100~x,
     col=baboon.data$sex, 
     xlab=x.label, ylab = expression('vascular porosity [%]'))

plot((baboon.data$porosity - baboon.data$vascular.porosity)*100~x,
     col=baboon.data$sex, 
     xlab=x.label, ylab = expression('osteocyte porosity [%]'))

plot(baboon.data$`oc.density(#/ccm bone)`~x, 
     col=baboon.data$sex, 
     xlab=x.label, ylab = expression('osteocyte density [cm'^-3*']'))



#analyse Oc.D. data
print("Shapiro-wilk test for normality of Oc.D")
print(shapiro.test(baboon.data$`oc.density(#/ccm bone)`))
print("correlation-test Oc.D. parametric")
print(cor.test(baboon.data$`oc.density(#/ccm bone)`,x, alternative="l"))
print("correlation-test Oc.D. non-parametric")
print(cor.test(baboon.data$`oc.density(#/ccm bone)`,x, alternative="l", method="s"))

#analyse vascular porosity data
print("Shapiro-wilk test for normality of vasc. por.")
print(shapiro.test(baboon.data$vascular.porosity))
print("correlation-test vasc. por. parametric")
print(cor.test(baboon.data$vascular.porosity,x, alternative="l"))
print("correlation-test vasc. por. non-parametric")
print(cor.test(baboon.data$vascular.porosity,x, alternative="l", method="s"))
