library("lfe")
library("stargazer")
#library("plyr")
library("dplyr")
library("Hmisc")
library("xtable")
library("ggplot2")

##################################
###PCC transformation
##############################
##final coding file, decimals with commas
direct<-read.csv2(file="J://Daten//TaxIncentives_meta//revisionRP_oct2020//replication_direct_china.csv", header=TRUE, sep=";", dec=",")
names(direct)[1]<-"estID"

#t based on the coeff and SE
direct$t <- direct$Coef/direct$SE
tstatmissing<-subset(direct, is.na(direct$t))
#replaces the NA t by those reported in the literature -- several 0
direct$t[is.na(direct$t)]<-tstatmissing$tstat
direct$tsq<-direct$t^2
summary(direct$t)

##pcc
direct$r<-direct$t/sqrt(direct$tsq+direct$df)
summary(direct$r)
#Taiwan with inverse measured
direct$r[direct$estID==101]<-0.0486
direct$r[direct$estID==102]<-0.0835
direct$r[direct$estID==103]<-0.0293
#tai<-subset(direct, Country=="Taiwan")
summary(direct$r)

##SE for PCC
direct$rsq<-direct$r^2
direct$SEr<-sqrt((1-direct$rsq)/direct$df)
summary(direct$SEr)

#basic plot for funnel plot
ggplot(direct, aes(x=r, y=1/SEr), group=factor(Typech))+geom_point(aes(shape=factor(Tax.Incentive.Design)), size=2)+labs(shape = "Base", x="PCC(coef)", y="Precision (1/SE)")
plot(direct$r, 1/direct$SEr)

#recoding key tax var
direct$Typech[direct$Tax.Incentive.Design==" incremental" | direct$Tax.Incentive.Design=="incremental"]<-"incremental"
direct$Typech[direct$Tax.Incentive.Design=="volume"]<-"volume"
direct$Typech[direct$Tax.Incentive.Design=="hybrid to volume" | direct$Tax.Incentive.Design=="incremental to volume"]<-"base change"
direct$Typech[direct$Tax.Incentive.Design=="hybrid"]<-"hybrid"
#typo to check
#table(direct$taxCredit)
summary(is.na(direct$taxCredit))
table(is.na(direct$Typech))

#base def
#table(direct$Tax.Incentive.Design)
#harmonizes
direct$Tax.Incentive.Design[direct$Tax.Incentive.Design==" incremental" | direct$Tax.Incentive.Design=="incremental"]<-"incremental"
direct$Tax.Incentive.Design[direct$Tax.Incentive.Design=="hybrid to volume" | direct$Tax.Incentive.Design=="incremental to volume"]<-"base change"


########FAT PET ANALYSIS variables
##precision weights
direct$vi<-direct$SEr^2
direct$prec_sq<-1/direct$vi
direct$prec<-1/direct$SEr
#PCC -- tstats for regressions
direct$tstat_pcc<-direct$r/direct$SEr
direct$tvi<-direct$r/direct$vi
summary(direct$tstat_pcc)
summary(direct$prec)

#######################################################################
#### comparing similar phenomena -- defining design variables
########################################################################
#modification vs intro scheme
direct$base<-0
direct$base[direct$Tax.Incentive.Design=="base change" | direct$Tax.Incentive.Design=="base change"]<-1
table(direct$base)
direct$baseSE<-direct$base/direct$SEr
#modif of rates or additional features than base
direct$shift<-0
direct$shift[direct$Country=="UK" | direct$Country=="Australia" |
               direct$Country=="Canada" | direct$Country=="Ireland" | direct$Country=="Spain" | direct$Country=="Slovenia" | direct$Country=="France" | 
               direct$Country=="Japan" | (direct$Country=="Taiwan" & direct$Period=="2006-2014")]<-1
direct$shift[direct$pubid==16]<-0 # corrects for the 2002 shock in the UK
############################################################################
####specific samples
#############################################################################
#rough control on the period beyond country
direct$later<-as.integer(!(direct$Period=="1975-1988" |direct$Period=="1975-1989" | direct$Period=="1975-1990" | direct$Period=="1975-1991" |
                             direct$Period=="1975-1992" | direct$Period=="1975-1993" | direct$Period=="1981-2013" | direct$Period=="1992-98" |
                             direct$Period=="1994" | direct$Period=="1994-1999"))
#all recent estimates 
post2000<-subset(direct, later==1)

colSums(is.na(direct)) 
################################################################################
##weights for the overall database
################################################################################
direct$volSE<-direct$volume/direct$SEr
direct$dedSE<-direct$deduction/direct$SEr
direct$carrySE<-direct$carryforward/direct$SEr
direct$appSE<-direct$approval/direct$SEr
direct$refSE<-direct$refund/direct$SEr
summary(direct$refSE)
direct$capSE<-direct$cap/direct$SEr
direct$targetedSE<-direct$targeted/direct$SEr
direct$US<-0
direct$US[direct$Country=="USA (California)" | direct$Country=="USA"]<-1
direct$USSE<-direct$US/direct$SEr
summary(direct$Year)
direct$Year[is.na(direct$Year)]<-2014
#stargazer(direct)

###harmonizes the main variables of interest
direct$variable[direct$Interest=="dummy 2002*eligib"|  direct$Interest=="post*group"|  direct$Interest=="zero*post"|  direct$Interest=="eligib*post*young"|  
                  direct$Interest=="eligib*post*loss"|  direct$Interest=="eligib*post*consistent"| direct$Interest== "eligib*post"|  direct$Interest=="eligib*year"| 
                  direct$Interest=="dummy 2002*eligib"|  direct$Interest=="claimed*post"|  direct$Interest=="CA*post"|  direct$Interest=="applied*elig" | direct$Interest=="elig*year"]<-"DiD"
direct$variable[direct$Interest=="receives tax"| 
                  direct$Interest=="dummies"| 
                  direct$Interest=="dummy claimed"]<-"treatment dummy"

direct$variable[direct$pubid==25|direct$pubid==21 | direct$pubid==12]<-"amount of eligible RD"
direct$variable[direct$Interest=="log RD TC" | direct$Interest=="log RD TC (lag)"| direct$Interest=="log RD TC (lag2)" | direct$Interest=="rate of eligible R&D tax credit (own calculation)"
                | direct$Interest=="rate of eligible R&D tax credit (own calculation, with cap)"]<-"amount of eligible RD"

table(direct$variable)
direct$variable[is.na(direct$variable)]<-"eligible dummy"



#selects UK estimates only
UK<-subset(direct, Country=="UK")
UK<-subset(UK, pubid!=16) #kicks Guceri 2013
UK$Authors[UK$pubid==15 | UK$pubid==14]<-"Guceri"
UK$Authors[UK$Authors=="Guceri, Liu"]<-"aGuceri, Li"
UK$pubid<-factor(UK$pubid, c("22", "15", "14", "17", "23", "26","27"))
#sample outside UK
noUK<-subset(direct, Country!="UK")

##reduces the sample to 1 paper per authors for the UK to reduce the bias
direct_corr<-subset(direct, !(pubid==22 | pubid==15 | pubid==17 | pubid==27 | pubid==14))
direct_corr$dummy[direct_corr$variable=="treatment dummy" | direct_corr$variable=="eligible dummy"]<-1
direct_corr$dummy[direct_corr$variable!="treatment dummy" | direct_corr$variable!="eligible dummy"]<-0

direct_corr$DiD[direct_corr$variable=="DiD"]<-1
direct_corr$DiD[direct_corr$variable!="DiD"]<-0


##all together to test the shift vs intro
post2000_cor<-subset(direct_corr, later==1)

##shifts correlated with more recent period
modif<-subset(direct_corr,(shift==1 & later==1))

##shifts correlated with more recent period: shift vs intro of volume
introall<-subset(direct_corr, (shift==0 & later==1 & volume==1)) #only recent estimates probably kicks Taiwan, 6 estimates.
##########################################################################################################################################
#### weights the tax designs variables
###############################################

#UK
direct$shiftSE<-direct$shift / direct$SEr
direct$UK<-0
direct$UK[direct$Country=="UK"]<-1
direct$UKSE<-direct$UK / direct$SEr
#recent periods
post2000_cor$shiftSE<-post2000_cor$shift/post2000_cor$SEr
post2000_cor$volSE<-post2000_cor$volume/post2000_cor$SEr
# intro vs shift to volume
modif$baseSE<-modif$base/modif$SEr

#ctrl
post2000_cor$Belg<-0
post2000_cor$Belg[post2000_cor$Country=="Belgium"]<-1
post2000_cor$BelgSE<-post2000_cor$Belg*post2000_cor$SEr


#Belgium bias in introducing volume
introall$volSE<-introall$volume/introall$SEr
introall$volbin<-0
introall$volbin[introall$base==1 | introall$volume==1]<-1
introall$volbinSE<-introall$volbin/introall$SEr
introall$Belg<-0
introall$Belg[introall$Country=="Belgium"]<-1

####################################################
##FAT PET  PEESE and country bias
#######################################################

#FAT PET
pcc<-felm(tstat_pcc ~ prec  | 0 | 0 |pubid, data=direct)
#PEESE
pccvar<-felm(tvi ~ prec_sq  | 0 | 0 |pubid, data=direct)
#model  UK
ppcnoUK<-felm(tstat_pcc ~ prec + UK | 0 | 0 |pubid, data=direct_corr)
#recent estimations
pcclater<-felm(tstat_pcc ~ prec   |0|0|pubid, data = post2000_cor)
#volume related
pccvol<-felm(tstat_pcc ~ prec + volSE   |0|0|pubid, data = post2000_cor)
#shift to vol
##here methods heavily corresponding to the nature of the shock
pccshift<-felm(tstat_pcc ~ prec +  baseSE |0|0|pubid, data = modif)

#intro of vol
pccintro<-felm(tstat_pcc ~ prec + Belg |0|0|pubid, data = introall) #most of the intro post 2000, here later synonymous of introducing volume

#DiD
post2000_cor$DiD[post2000_cor$variable=="DiD"]<-1
post2000_cor$DiD[post2000_cor$variable!="DiD"]<-0
#dummy
post2000_cor$dummy[post2000_cor$variable=="treatment dummy"]<-1
post2000_cor$dummy[post2000_cor$variable=="eligible dummy"]<-1
post2000_cor$dummy[post2000_cor$variable=="amount of eligible RD"]<-0
post2000_cor$dummy[post2000_cor$variable=="DiD"]<-0

#evolution of the methodology
pccmeth<-felm(tstat_pcc ~ prec + dummy + DiD |0|0|pubid, data =post2000_cor)



######################################################
# exports
#####################################################

stargazer(pcc, pccvar, ppcnoUK, pcclater, pccmeth, pccvol, pccshift, pccintro,
          title = "FAT-PET estimations",
          notes = "Robust standard errors and clustered at study level",
          notes.align = "r", 
          dep.var.labels = "PCC",
          column.labels = c("FATPET","PEESE", "UK", "post2000", "method", "volume", "shift to vol", "intro vol"),
          model.numbers = TRUE,
          column.sep.width = "1pt",
          no.space = TRUE,
          font.size = "footnotesize",
          type = "latex")

########################################################################################################################################
##ROBUSTNESS CHECKS -- Direct approaches with DiD
#########################################################################################################################################
###designs in extended analysis
DiD_db<-subset(direct_corr, DiD==1)
pccRC<-felm(tstat_pcc ~ prec  | 0 | 0 |pubid, data=DiD_db)

#intro of vol
pccDiDvol<-felm(tstat_pcc ~ prec + volSE |0|0|pubid, data = DiD_db) #

#deduction vs tax credits
DiDpccded<-felm(tstat_pcc ~ prec   + dedSE | 0 | 0 | pubid, data=DiD_db)

#avg effect for targeted
DiDpcctar2<-felm(tstat_pcc ~ prec + targetedSE  | 0 | 0 | pubid, data=DiD_db)

#carry
DiDpcccar<-felm(tstat_pcc ~ prec   + carrySE |  0 | 0 | pubid, data=DiD_db)

#approval
DiDpccapp<-felm(tstat_pcc ~ prec   + appSE | 0 | 0 | pubid, data=DiD_db)

#cap
DiDpcc_cap<-felm(tstat_pcc ~ prec   + capSE  | 0 | 0 | pubid, data=DiD_db)


stargazer(pccRC, DiDpccded, pccDiDvol, DiDpcctar2, DiDpcccar, DiDpccapp, DiDpcc_cap,
          title = "robustness checks among direct approaches",
          notes = "Robust standard errors and clustered at study level",
          notes.align = "r", 
           dep.var.labels = "PCC",
          column.labels = c("FATPET","type", "vol", "targeted", "carry", "approval", "cap"),
          model.numbers = TRUE,
          column.sep.width = "1pt",
          no.space = TRUE,
          font.size = "footnotesize",
          label= "RCdirect",
          type = "latex")

########################################################################################################################################
###appendix?
pccRC<-felm(tstat_pcc ~ prec + dummy |0|0|pubid, data = direct_corr) 
pccDiDvol<-felm(tstat_pcc ~ prec + volSE + dummy + DiD |0|0|pubid, data = direct_corr) #


DiDpccded<-felm(tstat_pcc ~ prec   + dedSE + dummy + DiD | 0 | 0 | pubid, data=direct_corr)


DiDpcctar2<-felm(tstat_pcc ~ prec + targetedSE + dummy + DiD | 0 | 0 | pubid, data=direct_corr)

#carry
table(DiD_db$Country, DiD_db$carryforward)
DiDpcccar<-felm(tstat_pcc ~ prec   + carrySE + dummy + DiD |  0 | 0 | pubid, data=direct_corr)

#approval is only Nowray
DiDpccapp<-felm(tstat_pcc ~ prec   + appSE + dummy  + DiD | 0 | 0 | pubid, data=direct_corr)
table(DiD_db$Country, DiD_db$approval)

#removes belgium
table(DiD_db$cap, DiD_db$Country)
DiDpcc_cap<-felm(tstat_pcc ~ prec   + capSE + dummy + DiD | 0 | 0 | pubid, data=direct_corr)


stargazer(pccRC, pcctax, DiDpccded, pccDiDvol, DiDpcctar2, DiDpccapp, DiDpcccar, DiDpcc_cap,
          title = "robustness checks among direct approaches",
          notes = "Robust standard errors and clustered at study level",
          notes.align = "r", 
          dep.var.labels = "PCC",
          column.labels = c("FATPET", "tax credit","type", "vol", "targeted", "approval", "carry", "cap"),
          model.numbers = TRUE,
          column.sep.width = "1pt",
          no.space = TRUE,
          font.size = "footnotesize",
          label= "RCdirect",
          type = "text")
###################################################################
#################################################################
#main country overview
direct$Country2<-direct$Country
direct$Country2[direct$Country2=="USA (California)"]<-"USA"
#descriptives at the country level - takes the sample corrected for the UK??
descr_country<-direct %>%  group_by(Country2) %>%   summarise(#avg_tstat=mean(t.stat),
  avg_old=mean(later),
  avg_inc=mean(incremental),
  avg_hyb=mean(hybrid),
  avg_vol=mean(volume),
  avg_deduc = mean(deduction),
  avg_carry=mean(carryforward),
  avg_refund=mean(refund),
  avg_target=mean(targeted),
  avg_cap=mean(cap),
  avg_approv=mean(approval),
  obs=n()) 
#exports table
print(xtable(descr_country), include.rownames=FALSE)

#descriptives at the study level
descr_study_dir<-direct %>%  group_by(pubid, Country, Period) %>%   summarise(
  Paper=Authors,
  Published=Year,
  avg_PCCtstat=mean(tstat),
  #    Country=Country,
  #     Period=Period,
  #   avg_old=mean(later),
  #    avg_inc=mean(incremental),
  #     avg_hyb=mean(hybrid),
  #     avg_vol=mean(volume),
  #     avg_deduc = mean(deduction),
  #     avg_carry=mean(carryforward),
  #    avg_refund=mean(refund),
  #   avg_target=mean(targeted),
  #   avg_cap=mean(cap),
  #   avg_approv=mean(approval),
  obs=n()) 
descr_study_dir<-unique(descr_study_dir)
descr_study_exp<-descr_study[,c("Paper", "Published", "Country2", "Period", "avg_PCCtstat", "avg_old", "avg_inc", "avg_hyb", "avg_vol", "avg_deduc",
                                "avg_carry", "avg_refund", "avg_target", "avg_cap", "avg_approv", "obs")]

descr_study_exp_dir<-descr_study_dir[,c("Paper", "Published", "Country", "Period", "avg_PCCtstat", "obs")]

print(xtable(descr_study_exp_dir), include.rownames=FALSE)

#descriptives per bases
descr_base<-strd %>% group_by(Typesch) %>% summarise (avg_tstat=mean(tstat),
                                                      sd_tstat=sd(tstat),
                                                      obs=n())
print(xtable(descr_base), include.rownames=FALSE)

quickstat<-direct %>% group_by(pubid) %>% summarise (avgtstat=mean(tstat_pcc))
summary(direct$tstat)
#general stats to add to Daniela's
stargazer(strd)
table(strd$approval, strd$Country)
table(strd$ctx)
###########################################################################################
#######correlation --- wo the UK bias
##########################################################################################
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .0001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

direct_corr$incSE<-direct_corr$incremental/direct_corr$SEr
direct_corr$hybSE<-direct_corr$hybrid/direct_corr$SEr

m<-direct_corr[,c("targetedSE", "incSE", "volSE", "dedSE", "carrySE", "appSE", "capSE", "hybSE", "baseSE")]


corstars(m, result="latex")
corr_dirc<-rcorr(as.matrix(m))
corr_dircP<-as.data.frame(corr_dirc$P)
print(corr_dirc)
xtable(corr_dircP, type="latex")

matrstruc<-subset(woRaoWP, select=c("incSE","targetedSE", "volSE", "dedSE", "carrySE", "appSE", "cap2SE", "hybSE", "SpainpostSE"))
corstars(matrstruc, result="latex")
corr_s<-rcorr(as.matrix(matrstruc))
corr_s<-as.data.frame(corr_dirc$P)
print(corr_s)
xtable(corr_s, type="latex")

                
                #################################################################################################################################
                #####DONUT GRAPH --- heterogeneity coming from methodological variations
                #################################################################################################################################

                direct$variable[direct$Interest=="dummy 2002*eligib"|  direct$Interest=="post*group"|  direct$Interest=="zero*post"|  direct$Interest=="eligib*post*young"|  
                                  direct$Interest=="eligib*post*loss"|  direct$Interest=="eligib*post*consistent"| direct$Interest== "eligib*post"|  direct$Interest=="eligib*year"| 
                                  direct$Interest=="dummy 2002*eligib"|  direct$Interest=="claimed*post"|  direct$Interest=="CA*post"|  direct$Interest=="applied*elig"]<-"DiD"
                direct$variable[direct$Interest=="receives tax"| 
                                  direct$Interest=="dummies"| 
                                  direct$Interest=="dummy claimed"]<-"treatment dummy"
                direct$variable[direct$pubid==12]<-"tax benefits"
                direct$variable[direct$pubid==25|direct$pubid==21]<-"amount of eligible RD"
                direct$variable[is.na(direct$variable)]<-"eligible dummy"
                table(direct$variable)
                DiD<-subset(direct, variable=="DiD")
                
                output<-as.data.frame(c("amount of eligible RD",  "DiD","eligible dummy", "tax benefits", "treatment dummy"))
                names(output)[1]<-"variable"
                output$numb<-c(35, 295, 137, 20, 15)
                output$share<-output$numb/502
                colors=c("darkgrey", "blue", "beige", "black", "lightblue")
                
                p <- plot_ly(output) %>% add_pie(labels = ~variable, values = ~numb, type = 'pie', hole = 0.7, marker = list(colors = colors))
                p
                
                donut<-direct %>% group_by(class1) %>% summarise(obs=n())
                donut$share<-donut$obs/518
                donut$section<-substr(donut$class1, 1, 1)
                p <- plot_ly(donut) %>% add_pie(labels = ~class1, values = ~share, type = 'pie', hole = 0.7, sort = F, list(
                  type = 'groupby',
                  groups = donut$section,
                  marker=list(color = c("silver","blue", "yellow", "green", "red", "black", "purple", "orange", "pink"))))
                p
                table(donut$section)
                
                
                donut2<-donut %>% group_by(section) %>% summarise(obs=sum(share))
                p <- plot_ly(donut2) %>% add_pie(labels = ~section, values = ~obs, type = 'pie', hole = 0.7, sort = T)
                p
                