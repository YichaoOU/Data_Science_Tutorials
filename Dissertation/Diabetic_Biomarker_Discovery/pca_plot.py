

rangescale <- function(X) {

	Xmax <- apply(X, 2, max)
	Xscaled = scale(X, scale=Xmax, center=T)

	return(Xscaled)
}
library('scales')
library("ggplot2")

# Change the file name
# df=read.table("result.csv",sep=",",header=T)
df=read.table("all_48_48_replace.arff.csv",sep=",",header=T)

size_df = dim(df)

# Change the columns
data = data.matrix(df[,2:(size_df[2]-1)])
data = rangescale(data)
pca = prcomp(data)

sum_pca = summary(pca)

# Change the label if necessary
a=df[,size_df[2]]


pc_data_frame = data.frame(pc1 = pca$x[,1],pc2 = pca$x[,2],pc3 = pca$x[,3],class=a)
b=ggplot(pc_data_frame) +
geom_abline(intercept = 0.1,size=1,slope = 0.55)+
  # geom_point(aes(x=pc1, y=pc2, colour="black",fill="black", size=5, shape=factor(class))) +
  geom_point(aes(x=pc1, y=pc2, shape=factor(class)), colour="black",fill="black", size=2) +
  scale_shape(name="Class") + 
  # stat_ellipse(aes(x=pc1,y=pc2,fill=factor(class)),
               # geom="polygon", level=0.8, alpha=0.2)+
	xlab(paste("PC 1 (",percent(sum_pca$importance[2,1]),")",sep = ""))+ ylab(paste("PC 2 (",percent(sum_pca$importance[2,2]),")",sep = ""))+
	 theme_bw(base_size = 8) +
  theme(axis.line.x = element_line(size=1),axis.line.y = element_line(size=1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
	legend.position="none") 

# print (b)
ggsave(filename="15_features_PCA_plot_revised.tiff",width=5.6,height=5.6,unit="cm",dpi=1200)

