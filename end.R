load("NEM.RData")
NEM=result
rm(result)
load("MMZ.RData")
MMZ=result
rm(result)
MMO=load("MMO.RData")
MMO=result
rm(result)
MMM=load("MMM.RData")
MMM=result
rm(result)
result = cbind(MMM,MMO,MMZ,NEM)

ggplot(result) +
  geom_histogram(aes(x = MZNU, fill = "EM(0)"), binwidth = 0.5,alpha=0.5) +
  xlim(0,30)+
  geom_histogram(aes(x = NNU, fill = "N-EM"), binwidth = 0.5,alpha=0.5) +
  xlim(0,30)+
  scale_fill_manual(values = c("red", "blue")) +
  theme(legend.position = c(0.9, 0.9),
        legend.justification = c(1, 1))+
  #ggtitle(TeX("Histogram of estimation of $\\nu$ from EM(0) algorithm and N-EM algorithm"))+
  labs(x=NULL, fill = "Estimation from \n two algorithms")

ggplot(result) +
  geom_histogram(aes(x = MONU, fill = "EM(1)"), binwidth = 0.5,alpha=0.5) +
  xlim(0,30)+
  geom_histogram(aes(x = NNU, fill = "N-EM"), binwidth = 0.5,alpha=0.5) +
  xlim(0,30)+
  scale_fill_manual(values = c("red", "blue")) +
  theme(legend.position = c(0.9, 0.9),
        legend.justification = c(1, 1))+
  #ggtitle(TeX("Histogram of estimation of $\\nu$ from EM(0) algorithm and N-EM algorithm"))+
  labs(x=NULL, fill = "Estimation from \n two algorithms")


ggplot(result) +
  geom_histogram(aes(x = MMNU, fill = "EM(2)"), binwidth = 0.5,alpha=0.5) +
  xlim(0,30)+
  geom_histogram(aes(x = NNU, fill = "N-EM"), binwidth = 0.5,alpha=0.5) +
  xlim(0,30)+
  scale_fill_manual(values = c("red", "blue")) +
  theme(legend.position = c(0.9, 0.9),
        legend.justification = c(1, 1))+
  #ggtitle(TeX("Histogram of estimation of $\\nu$ from EM(0) algorithm and N-EM algorithm"))+
  labs(x=NULL, fill = "Estimation from \n two algorithms")

ggplot(result) +
  geom_histogram(aes(x = MZMU, fill = "EM(0)"), binwidth = 0.1,alpha=0.5) +
  xlim(0,2)+
  geom_histogram(aes(x = NMU, fill = "N-EM"), binwidth = 0.1,alpha=0.5) +
  xlim(0,2)+
  scale_fill_manual(values = c("red", "blue")) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1))+
  #ggtitle(TeX("Histogram of estimation of $\\mu$ from EM(0) algorithm and N-EM algorithm"))+
  labs(x = NULL, fill = "Estimation from \n two algorithms")

ggplot(result) +
  geom_histogram(aes(x = MOMU, fill = "EM(1)"), binwidth = 0.1,alpha=0.5) +
  xlim(0,2)+
  geom_histogram(aes(x = NMU, fill = "N-EM"), binwidth = 0.1,alpha=0.5) +
  xlim(0,2)+
  scale_fill_manual(values = c("red", "blue")) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1))+
  #ggtitle(TeX("Histogram of estimation of $\\mu$ from EM(0) algorithm and N-EM algorithm"))+
  labs(x = NULL, fill = "Estimation from \n two algorithms")

ggplot(result) +
  geom_histogram(aes(x = MMMU, fill = "EM(2)"), binwidth = 0.1,alpha=0.5) +
  xlim(0,2)+
  geom_histogram(aes(x = NMU, fill = "N-EM"), binwidth = 0.1,alpha=0.5) +
  xlim(0,2)+
  scale_fill_manual(values = c("red", "blue")) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1))+
  #ggtitle(TeX("Histogram of estimation of $\\mu$ from EM(0) algorithm and N-EM algorithm"))+
  labs(x = NULL, fill = "Estimation from \n two algorithms")

ggplot(result) +
  geom_histogram(aes(x = MZSI, fill = "EM(0)"), binwidth = 0.1,alpha=0.5) +
  xlim(1,3)+
  geom_histogram(aes(x = NSI, fill = "N-EM"), binwidth = 0.1,alpha=0.5) +
  xlim(1,3)+
  scale_fill_manual(values = c("red", "blue")) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1))+
  #ggtitle(TeX("Histogram of estimation of $\\sigma$ from EM(0) algorithm and N-EM algorithm"))+
  labs(x = NULL, fill = "Estimation from \n two algorithms")

ggplot(result) +
  geom_histogram(aes(x = MOSI, fill = "EM(1)"), binwidth = 0.1,alpha=0.5) +
  xlim(1,3)+
  geom_histogram(aes(x = NSI, fill = "N-EM"), binwidth = 0.1,alpha=0.5) +
  xlim(1,3)+
  scale_fill_manual(values = c("red", "blue")) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1))+
  #ggtitle(TeX("Histogram of estimation of $\\sigma$ from EM(0) algorithm and N-EM algorithm"))+
  labs(x = NULL, fill = "Estimation from \n two algorithms")

ggplot(result) +
  geom_histogram(aes(x = MMSI, fill = "EM(2)"), binwidth = 0.1,alpha=0.5) +
  xlim(1,3)+
  geom_histogram(aes(x = NSI, fill = "N-EM"), binwidth = 0.1,alpha=0.5) +
  xlim(1,3)+
  scale_fill_manual(values = c("red", "blue")) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1))+
  #ggtitle(TeX("Histogram of estimation of $\\sigma$ from EM(0) algorithm and N-EM algorithm"))+
  labs(x = NULL, fill = "Estimation from \n two algorithms")

ggplot(result) +
  geom_histogram(aes(x = MZLA, fill = "EM(0)"), binwidth = 0.5,alpha=0.5) +
  xlim(0,10)+
  geom_histogram(aes(x = NLA, fill = "N-EM"), binwidth = 0.5,alpha=0.5) +
  xlim(0,10)+
  scale_fill_manual(values = c("red", "blue")) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1))+
  #ggtitle(TeX("Histogram of estimation of $\\lambda$ from EM(0) algorithm and N-EM algorithm"))+
  labs(x = NULL, fill = "Estimation from \n two algorithms")

ggplot(result) +
  geom_histogram(aes(x = MOLA, fill = "EM(1)"), binwidth = 0.5,alpha=0.5) +
  xlim(0,10)+
  geom_histogram(aes(x = NLA, fill = "N-EM"), binwidth = 0.5,alpha=0.5) +
  xlim(0,10)+
  scale_fill_manual(values = c("red", "blue")) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1))+
  #ggtitle(TeX("Histogram of estimation of $\\lambda$ from EM(0) algorithm and N-EM algorithm"))+
  labs(x = NULL, fill = "Estimation from \n two algorithms")

ggplot(result) +
  geom_histogram(aes(x = MMLA, fill = "EM(2)"), binwidth = 0.5,alpha=0.5) +
  xlim(0,10)+
  geom_histogram(aes(x = NLA, fill = "N-EM"), binwidth = 0.5,alpha=0.5) +
  xlim(0,10)+
  scale_fill_manual(values = c("red", "blue")) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1))+
  #ggtitle(TeX("Histogram of estimation of $\\lambda$ from EM(0) algorithm and N-EM algorithm"))+
  labs(x = NULL, fill = "Estimation from \n two algorithms")

result$NSUB = result$NLLF-result$NLL
result$MZSUB = result$MZLLF-result$MZLL
result$MOSUB = result$MOLLF-result$MOLL
result$MMSUB = result$MMLLF-result$MMLL

ggplot(result) +
  geom_histogram(aes(x = MZSUB, fill = "EM(0)"), binwidth = 0.5,alpha=0.5) +
  xlim(-10,2)+
  geom_histogram(aes(x = NSUB, fill = "N-EM"), binwidth = 0.5,alpha=0.5) +
  xlim(-10,2)+
  scale_fill_manual(values = c("red", "blue")) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1))+
  labs(x = NULL, fill = NULL)

ggplot(result) +
  geom_histogram(aes(x = MOSUB, fill = "EM(1)"), binwidth = 0.5,alpha=0.5) +
  xlim(-10,2)+
  geom_histogram(aes(x = NSUB, fill = "N-EM"), binwidth = 0.5,alpha=0.5) +
  xlim(-10,2)+
  scale_fill_manual(values = c("red", "blue")) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1))+
  labs(x = NULL, fill = NULL)

ggplot(result) +
  geom_histogram(aes(x = MMSUB, fill = "EM(2)"), binwidth = 0.5,alpha=0.5) +
  xlim(-10,2)+
  geom_histogram(aes(x = NSUB, fill = "N-EM"), binwidth = 0.5,alpha=0.5) +
  xlim(-10,2)+
  scale_fill_manual(values = c("red", "blue")) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1))+
  labs(x = NULL, fill = NULL)
