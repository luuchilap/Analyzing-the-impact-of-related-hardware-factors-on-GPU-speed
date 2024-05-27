library(dplyr)
data_GPU = read.csv("C:/Users/Admin/Downloads/All_GPUs.csv") 
head(data_GPU,5)

new_data <- data_GPU[,c("Max_Power", "Core_Speed", "L2_Cache", "Memory", "Memory_Bandwidth", "Memory_Bus", "Memory_Speed","Pixel_Rate")] 
head(new_data,10)

new_data <- new_data %>%
  mutate(
    Max_Power=as.numeric(gsub("[^0-9.]","",Max_Power)),
    Core_Speed=as.numeric(gsub("[^0-9.]","",Core_Speed)),
    L2_Cache=as.numeric(gsub("[^0-9.]","",L2_Cache)),
    Memory=as.numeric(gsub("[^0-9.]","",Memory)),
    Memory_Bandwidth=as.numeric(gsub("[^0-9.]","",Memory_Bandwidth)),
    Memory_Bus=as.numeric(gsub("[^0-9.]","",Memory_Bus)),
    Memory_Speed=as.numeric(gsub("[^0-9.]","",Memory_Speed)),
    Pixel_Rate=as.numeric(gsub("[^0-9.]","",Pixel_Rate)),
  )

head(new_data,10)

colSums(is.na(new_data))

mean_values <- sapply(new_data, function(x) mean(x,na.rm=TRUE))
mean_values
new_data <- new_data %>%
  mutate(
    Max_Power=ifelse(is.na(Max_Power),mean_values["Max_Power"],Max_Power),
    Core_Speed=ifelse(is.na(Core_Speed),mean_values["Core_Speed"],Core_Speed),
    L2_Cache=ifelse(is.na(L2_Cache),mean_values["L2_Cache"],L2_Cache),
    Memory=ifelse(is.na(Memory),mean_values["Memory"],Memory),
    Memory_Bandwidth=ifelse(is.na(Memory_Bandwidth),mean_values["Memory_Bandwidth"],Memory_Bandwidth),
    Memory_Bus=ifelse(is.na(Memory_Bus),mean_values["Memory_Bus"],Memory_Bus),
    Memory_Speed=ifelse(is.na(Memory_Speed),mean_values["Memory_Speed"],Memory_Speed),
    Pixel_Rate=ifelse(is.na(Pixel_Rate),mean_values["Pixel_Rate"],Pixel_Rate),
    )

head(new_data,10)

find_boxplot_boundaries <- function(col,whisker_coeff = 1.5){
  Q1 <- quantile(col, 0.25)
  Q3 <- quantile(col, 0.75)
  IQR <- Q3 - Q1
  lower <- Q1 - whisker_coeff*IQR
  upper <- Q3 + whisker_coeff*IQR
  return(list(lower = lower, upper = upper ))
}
BoxplotOutlierClipper <- function(whisker_coeff = 1.5, X){
  boundaries <- find_boxplot_boundaries(X, whisker_coeff)
  clipped_X <- pmax(pmin(X,boundaries$upper),boundaries$lower)
  return(clipped_X)
}

new_data$Max_Power <- BoxplotOutlierClipper(whisker_coeff = 1.5,X = new_data$Max_Power)
new_data$Core_Speed <- BoxplotOutlierClipper(whisker_coeff = 1.5,X = new_data$Core_Speed)
new_data$L2_Cache <- BoxplotOutlierClipper(whisker_coeff = 1.5,X = new_data$L2_Cache)
new_data$Memory <- BoxplotOutlierClipper(whisker_coeff = 1.5,X = new_data$Memory)
new_data$Memory_Bandwidth <- BoxplotOutlierClipper(whisker_coeff = 1.5,X = new_data$Memory_Bandwidth)
new_data$Memory_Bus<- BoxplotOutlierClipper(whisker_coeff = 1.5,X = new_data$Memory_Bus)
new_data$Memory_Speed<- BoxplotOutlierClipper(whisker_coeff = 1.5,X = new_data$Memory_Speed)
new_data$Pixel_Rate<- BoxplotOutlierClipper(whisker_coeff = 1.5,X = new_data$Pixel_Rate)

head(new_data,10)

library(ggplot2)

hist(new_data$Core_Speed, main="Biểu đồ Histogram của Core Speed", xlab="Core speed",breaks=30,xlim = c(500,1500))

plot(new_data$Max_Power,new_data$Core_Speed, main = "Max_Power vs Core_Speed", xlab = "Max_Power", ylab = "Core_Speed", col = "black", pch = 20)
model <- lm(new_data$Core_Speed ~ new_data$Max_Power)
abline(model, col = "red")


plot(new_data$L2_Cache,new_data$Core_Speed, main = "L2_Cache vs Core_Speed", xlab = "L2_Cache", ylab = "Core_Speed", col = "black", pch = 20)
model <- lm(new_data$Core_Speed ~ new_data$L2_Cache)
abline(model, col = "red")

plot(new_data$Memory,new_data$Core_Speed, main = "Memory vs Core_Speed", xlab = "Memory", ylab = "Core_Speed", col = "black", pch = 20)
model <- lm(new_data$Core_Speed ~ new_data$Memory)
abline(model, col = "red")

plot(new_data$Memory_Bandwidth,new_data$Core_Speed, main = "Memory_Bandwidth vs Core_Speed", xlab = "Memory_Bandwidth", ylab = "Core_Speed", col = "black", pch = 20)
model <- lm(new_data$Core_Speed ~ new_data$Memory_Bandwidth)
abline(model, col = "red")

plot(new_data$Memory_Bus,new_data$Core_Speed, main = "Memory_Bus vs Core_Speed", xlab = "Memory_Bus", ylab = "Core_Speed", col = "black", pch = 20)
model <- lm(new_data$Core_Speed ~ new_data$Memory_Bus)
abline(model, col = "red")

plot(new_data$Memory_Speed,new_data$Core_Speed, main = "Memory_Speed vs Core_Speed", xlab = "Memory_Speed", ylab = "Core_Speed", col = "black", pch = 20)
model <- lm(new_data$Core_Speed ~ new_data$Memory_Speed)
abline(model, col = "red")

plot(new_data$Pixel_Rate,new_data$Core_Speed, main = "Pixel_Rate vs Core_Speed", xlab = "Pixel_Rate", ylab = "Core_Speed", col = "black", pch = 20)
model <- lm(new_data$Core_Speed ~ new_data$Pixel_Rate)
abline(model, col = "red")

mean <- apply(new_data,2,mean)
sd <- apply(new_data,2,sd)
median <- apply(new_data,2,median)
Q1 <- apply(new_data,2,quantile,probs=0.25)
Q3 <- apply(new_data,2,quantile,probs=0.75)
min <- apply(new_data,2,min)
max <- apply(new_data,2,max)
t(data.frame(mean,sd,median,Q1,Q3,min,max))

model1=lm(Core_Speed ~ Max_Power+ L2_Cache + Memory + Memory_Bandwidth + Memory_Bus + Memory_Speed + Pixel_Rate, data = new_data)
summary(model1)

model2=lm(Core_Speed ~ Max_Power + Memory + Memory_Bandwidth + Memory_Bus + Memory_Speed + Pixel_Rate, data = new_data)
summary(model2)

anova(model1,model2)

plot(model2)

X <- data.frame("Max_Power" = 285, "L2_Cache" = 768, "Memory" = 1792, "Memory_Bandwidth" = 225.8,"Memory_Bus"=448, "Memory_Speed" = 1008, "Pixel_Rate" = 32)
predictX <- predict(model1,X,interval="confidence", level=0.95)
head(predictX)

data_anova <- new_data
data_anova$Manufacturer <-  data_GPU$Manufacturer
data_anova$SLI_Crossfire <- data_GPU$SLI_Crossfire
anova_model <- aov(Core_Speed ~ Manufacturer * SLI_Crossfire, data = data_anova)
summary(anova_model)

library(ggplot2)

ggplot(data = data_anova, aes(x = Manufacturer, y = Core_Speed, group = as.factor(SLI_Crossfire), color = SLI_Crossfire)) + 
  geom_line() +
  xlab("Manufacturer") +
  ylab("Core_Speed")

TukeyHSD(anova_model)

# Lọc dữ liệu theo nhà sản xuất 
data_manufacturer_ATI <- subset(data_anova, Manufacturer == "ATI")
# Vẽ histogram
ggplot(data_manufacturer_ATI, aes(x = Core_Speed)) +
  geom_histogram(fill = "pink", color = "black") +
  labs(title = "ATI Manufacturer",
       x = "Core Speed", y = "Frequency")

data_manufacturer_AMD <- subset(data_anova, Manufacturer == "AMD")
ggplot(data_manufacturer_AMD, aes(x = Core_Speed)) +
  geom_histogram(fill = "pink", color = "black") +
  labs(title = "AMD Manufacturer",
       x = "Core Speed", y = "Frequency")

data_manufacturer_Nvidia <- subset(data_anova, Manufacturer == "Nvidia")
ggplot(data_manufacturer_Nvidia, aes(x = Core_Speed)) +
  geom_histogram(fill = "pink", color = "black") +
  labs(title = "Nvidia Manufacturer",
       x = "Core Speed", y = "Frequency")

data_manufacturer_Intel <- subset(data_anova, Manufacturer == "Intel")
ggplot(data_manufacturer_Intel, aes(x = Core_Speed)) +
  geom_histogram(fill = "pink", color = "black") +
  labs(title = "Intel Manufacturer",
       x = "Core Speed", y = "Frequency")

data_SLICrossfire_Integrated <- subset(data_anova, SLI_Crossfire == "Yes")
ggplot(data_SLICrossfire_Integrated, aes(x = Core_Speed)) +
  geom_histogram(fill = "pink", color = "black") +
  labs(title = "SLI Crossfire Integrated",
       x = "Core Speed", y = "Frequency")

data_No_SLICrossfire_Integrated <- subset(data_anova, SLI_Crossfire == "No")
ggplot(data_No_SLICrossfire_Integrated, aes(x = Core_Speed)) +
  geom_histogram(fill = "pink", color = "black") +
  labs(title = "No SLI Crossfire Integrated",
       x = "Core Speed", y = "Frequency")

install.packages("car")
library(car)

data_anova$Manufacturer <- factor(data_anova$Manufacturer)
leveneTest(Core_Speed ~ Manufacturer, data=data_anova)

data_anova$SLI_Crossfire <- factor(data_anova$SLI_Crossfire)
leveneTest(Core_Speed ~ SLI_Crossfire, data=data_anova)
