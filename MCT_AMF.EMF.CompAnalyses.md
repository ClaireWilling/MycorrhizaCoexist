MCT\_EMAMCompetition
================
Claire Willing
2/9/2021

\#Data clean-up \#\#Calculating total biomass

``` r
setwd("~/Dropbox/Research/Postdoc/MCT_BaBi/DataAnalysis/")
MCTdataRAW=read.csv("MCT_BaBi_FullDataSheet.csv", header = T)
attach(MCTdataRAW)

#linear model
rootdrywetmassmodel<-lm(formula = Rootmass_g~Rootmass_focal_wet_g_minuspercol, data = MCTdataRAW)

library(car)
qqp(residuals(rootdrywetmassmodel), col=2)
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

    ## 102 347 
    ##   1  33

``` r
summary(rootdrywetmassmodel)
```

    ## 
    ## Call:
    ## lm(formula = Rootmass_g ~ Rootmass_focal_wet_g_minuspercol, data = MCTdataRAW)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.39938 -0.14137 -0.00662  0.27694  0.72733 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                       0.18339    0.11062   1.658    0.106    
    ## Rootmass_focal_wet_g_minuspercol  0.12684    0.02051   6.185 4.41e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3899 on 35 degrees of freedom
    ##   (967 observations deleted due to missingness)
    ## Multiple R-squared:  0.5222, Adjusted R-squared:  0.5086 
    ## F-statistic: 38.25 on 1 and 35 DF,  p-value: 4.409e-07

``` r
MCTdataRAWr<-dplyr::filter(MCTdataRAW, Rootmass_focal_wet_g_minuspercol<11)

rootdrywetmassmodel<-glm(formula = Rootmass_g~Rootmass_focal_wet_g_minuspercol, data = MCTdataRAWr)

qqp(residuals(rootdrywetmassmodel), col=2)
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

    ## [1] 15  8

``` r
summary(rootdrywetmassmodel)
```

    ## 
    ## Call:
    ## glm(formula = Rootmass_g ~ Rootmass_focal_wet_g_minuspercol, 
    ##     data = MCTdataRAWr)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -0.85117  -0.10681   0.01321   0.21708   0.58201  
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                       0.07287    0.08576   0.850    0.401    
    ## Rootmass_focal_wet_g_minuspercol  0.16253    0.01683   9.654 2.85e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.08594897)
    ## 
    ##     Null deviance: 10.9334  on 35  degrees of freedom
    ## Residual deviance:  2.9223  on 34  degrees of freedom
    ## AIC: 17.762
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
##Row 30 is a big outlier; will try to remove and see if better fit

coefficient<-summary(rootdrywetmassmodel)

slope=coefficient$coefficients[2,1]
intercept=coefficient$coefficients[1,1]

##checking that values look correct with model fit
Rootdrywetmass<-ggplot(data=MCTdataRAWr, aes(x=Rootmass_focal_wet_g_minuspercol, y=Rootmass_g))+
  geom_point()+
  labs(title="Wet mass vs dry mass", 
       x="Wet biomass (g)",
       y="Dry biomass (g)")+
  ylim(0,3)+
  geom_abline(aes(intercept=intercept,
                  slope=slope))

Rootdrywetmass+theme_classic()
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

``` r
##checks out

MCTdataroot=MCTdataRAW %>%
  rowwise()%>% 
  mutate(
    ##adjustingrootsmassfromstaining
    Rootcolldry=
           (slope*Percol_wetmass_g)+intercept,
    ##roots trimmed/number of plants in pot +focal plant
    Roottrimdry=Rootmasstrimmed_g/(Competitornum+1))

##adjusted values fit on regression line?
Rootdrywetmass<-ggplot()+
  geom_point(data=MCTdataroot, aes(x=Rootmass_focal_wet_g_minuspercol, y=Rootmass_g))+
  geom_point(data=MCTdataroot, aes(x=Percol_wetmass_g, y=Rootcolldry), col="red")+
  labs(title="Wet mass vs dry mass", 
       x="Wet biomass (g)",
       y="Dry biomass (g)")+
  ylim(0,3)+
  geom_abline(aes(intercept=intercept,
                  slope=slope))
Rootdrywetmass+theme_classic()
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-2-4.png)<!-- -->

``` r
##Clean up 
##There is one plant that was labeled as Bi, but was actually planted as Ba (Sample ID:30_EMF_BiBa0_Rep6)

MCTdatarootc<-MCTdataroot%>%
  filter(SampleIDnum==30) 

 MCTdataroot[210,]$FocalSpecies="Ba"
 MCTdataroot[210,]$SampleID="30_EMF_Ba1Ba0_Rep6"
 MCTdataroot[210,]$CompetitionTreatment="Conspecific"

 ##check to see if cleaned; good
 MCTdatarootc<-MCTdataroot%>%
  filter(SampleIDnum==30) 

#Calculating total biomass
MCTdatafullc=MCTdataroot %>%
  rowwise()%>% 
  mutate_at(vars(Rootcolldry, Roottrimdry), ~replace_na(., 0))%>%
  mutate(Belowgroundbiomass_focal=
           Rootmass_g+Rootcolldry+Roottrimdry)%>%
  mutate(Abovegroundbiomass_focal=
           Shoot_leaf_mass_g+as.numeric(Amax_mass_g)
         +LMA_leafmass_g)%>% 
  mutate(Totalbiomass_focal=
  Belowgroundbiomass_focal+Abovegroundbiomass_focal)
```

\#\#Recoding competition

``` r
MCTdatafull2<-MCTdatafullc%>%
  dplyr::filter(Competitornum==0)%>%
  dplyr::mutate(CompetitionTreatment=
                  dplyr::recode(CompetitionTreatment,
                  Heterospecific="No Competitors",
                  Conspecific="No Competitors"))

MCTdatafull3<-MCTdatafullc%>%
  dplyr::filter(Competitornum>0)

MCTdatafull<-full_join(MCTdatafull2,MCTdatafull3)
```

    ## Joining, by = c("SampleID", "SampleIDnum", "Mycotreatment", "DensityTreatment", "FocalSpecies", "Competitornum", "CompetitionTreatment", "Replicate", "Dead.", "Roottrim_28.08.2020", "Rootmasstrimmed_g", "Position", "TrayNum", "HarvestDate", "Amax_leafarea_cm3", "Amax_leafmass_g", "Percol_root", "AMFPercol_rootmass", "Percol_wetmass_g", "Rootmass_focal_wet_g_minuspercol", "Rootmass_g", "Stemheight_mm", "Shoot_leaf_mass_g", "LMA_leafarea_cm3", "Amax_mass_g", "LMA_leafmass_g", "Notes", "Competitor.biomass", "Hyphae", "Arbuscules", "Vesicles", "No.Colonization", "Total", "X", "X.1", "X.2", "X.3", "Rootcolldry", "Roottrimdry", "Belowgroundbiomass_focal", "Abovegroundbiomass_focal", "Totalbiomass_focal")

``` r
level_order <- c("No Competitors", "Conspecific", "Heterospecific")
level_order2<-c("Sterile", "EMF", "AMF", "AMFEMF")
MCTdatafull$CompetitionTreatment <- factor(MCTdatafull$CompetitionTreatment, levels=level_order)
MCTdatafull$Mycotreatment <- factor(MCTdatafull$Mycotreatment, levels=level_order2)

##Reading in values as numeric vs factors
MCTdatafull$Percol_root<-as.numeric(MCTdatafull$Percol_root)
```

    ## Warning: NAs introduced by coercion

``` r
MCTdatafull$Arbuscules<-as.numeric(MCTdatafull$Arbuscules)
```

    ## Warning: NAs introduced by coercion

``` r
MCTdatafull$Vesicles<-as.numeric(MCTdatafull$Vesicles)
MCTdatafull$Hyphae<-as.numeric(MCTdatafull$Hyphae)
```

    ## Warning: NAs introduced by coercion

``` r
MCTdatafull$Total<-as.numeric(MCTdatafull$Total)
```

\#\#AMF colonization calculations

``` r
MCTdatafull=MCTdatafull%>%
  rowwise()%>% 
  mutate(AMFPerRootLenCol=(Hyphae+Arbuscules+Vesicles)/Total)
```

\#\#Combine with nutrient data

``` r
library(readxl)
setwd("~/Dropbox/Research/Postdoc/MCT_BaBi/DataAnalysis/")
Iso1=read_excel("IsotopeData_CSIB_UCB_Willing_15N13C_MCTSamples.xls", skip=12,sheet=1)
```

    ## New names:
    ## * `Sample ID` -> `Sample ID...3`
    ## * `` -> ...6
    ## * `Sample ID` -> `Sample ID...9`

``` r
Iso2=read_excel("IsotopeData_CSIB_UCB_Willing_15N13C_MCTSamples.xls", skip=12,sheet=2)
```

    ## New names:
    ## * `Sample ID` -> `Sample ID...3`
    ## * `` -> ...6
    ## * `Sample ID` -> `Sample ID...9`

``` r
Iso=full_join(Iso1,Iso2)
```

    ## Joining, by = c("#", "Cell address", "Sample ID...3", "Sample wt. (mg)", "Notes*", "...6", "* Please indicate if samples are not replaceable", "Batch", "Sample ID...9", "% N", "mg N in capsule", "d 15N (‰)", "% C", "mg C in capsule", "d 13C (‰)")

``` r
Isoc<-rename(Iso, SampleID="Sample ID...3")

#Combine with other data for metadata info
MCTdatafull<-dplyr::full_join(MCTdatafull,Isoc)
```

    ## Joining, by = "SampleID"

``` r
MCTdatafull<-MCTdatafull%>%
  mutate(PerN_Std=Abovegroundbiomass_focal*`% N`)
```

\#H1 Alone \#\#Biomass

``` r
Alone = MCTdatafull%>%
            filter(Competitornum==0)

Alone_Bi = Alone%>%
            filter(FocalSpecies=="Bi")

BiomassaovBi<-(aov(Alone_Bi$Totalbiomass_focal~
                Alone_Bi$Mycotreatment))

summary(BiomassaovBi)
```

    ##                        Df Sum Sq Mean Sq F value Pr(>F)  
    ## Alone_Bi$Mycotreatment  3  1.380  0.4599   3.107 0.0362 *
    ## Residuals              43  6.365  0.1480                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
TukeyHSD(BiomassaovBi)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Alone_Bi$Totalbiomass_focal ~ Alone_Bi$Mycotreatment)
    ## 
    ## $`Alone_Bi$Mycotreatment`
    ##                       diff        lwr         upr     p adj
    ## EMF-Sterile     0.23339773 -0.1957824  0.66257781 0.4740121
    ## AMF-Sterile    -0.25499167 -0.6747381  0.16475473 0.3765459
    ## AMFEMF-Sterile -0.04232667 -0.4620731  0.37741973 0.9930536
    ## AMF-EMF        -0.48838939 -0.9175695 -0.05920931 0.0201336
    ## AMFEMF-EMF     -0.27572439 -0.7049045  0.15345569 0.3276280
    ## AMFEMF-AMF      0.21266500 -0.2070814  0.63241140 0.5344890

``` r
Alone_Ba = Alone%>%
            filter(FocalSpecies=="Ba")

BiomassaovBa<-(aov(Alone_Ba$Totalbiomass_focal~Alone_Ba$Mycotreatment))

summary(BiomassaovBa)
```

    ##                        Df Sum Sq Mean Sq F value Pr(>F)   
    ## Alone_Ba$Mycotreatment  3  25.06   8.353   5.375  0.003 **
    ## Residuals              45  69.94   1.554                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
TukeyHSD(BiomassaovBa)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Alone_Ba$Totalbiomass_focal ~ Alone_Ba$Mycotreatment)
    ## 
    ## $`Alone_Ba$Mycotreatment`
    ##                      diff         lwr      upr     p adj
    ## EMF-Sterile    -0.3022301 -1.63356814 1.029108 0.9297716
    ## AMF-Sterile     0.9807235 -0.37698021 2.338427 0.2314545
    ## AMFEMF-Sterile  1.4544543  0.09675051 2.812158 0.0315320
    ## AMF-EMF         1.2829536 -0.04838444 2.614292 0.0626427
    ## AMFEMF-EMF      1.7566843  0.42534628 3.088022 0.0053398
    ## AMFEMF-AMF      0.4737307 -0.88397303 1.831434 0.7885211

``` r
#Mycoplots (no competition)
Mycobaseline<-ggplot(data=Alone, 
         aes(x=Mycotreatment, Totalbiomass_focal,
             fill=Mycotreatment, color=Mycotreatment))+
  geom_boxplot()+
  facet_wrap(~FocalSpecies, scales = "free_y", 
             ncol = 2, labeller = labeller(FocalSpecies=plantlabels))+
  labs(x="Mycorrhizal Treatment", y="Plant biomass (g)")+
  scale_fill_manual(values=Mycocolors)+
  scale_color_manual(values=Mycocolors2)+
  scale_x_discrete(labels = c('None','EMF','AMF', "AMF+EMF"))+
  theme(
        text = element_text(),
        panel.background = element_rect(fill="transparent", colour = NA),
        plot.background = element_rect(fill="transparent",colour = NA),
        panel.border = element_rect(fill="transparent", colour = "black"),
        axis.title.y = element_text(angle=90, size = 12, margin = margin(r=3, unit = "mm")),
        axis.title.x = element_text(size=12, margin = margin(t=3, unit = "mm")),
        axis.text.x = element_text(size=12, margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")),
        axis.text.y = element_text(size=12, margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
        axis.line = element_blank(),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.y = element_line(colour = "black"),
        axis.ticks.length=unit(-1.4, "mm"),
        strip.text = element_text(face = "italic"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
```

\#\#Colonization

``` r
##Does EM colonization vary by mycotreatment?
AvgBiColAlone=Alone_Bi%>%
  group_by(Mycotreatment)%>%
  drop_na(Percol_root)%>%
  summarise(AvgEMFcolonization=mean(Percol_root),
  sdEMFcol=sd(Percol_root))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
##very low sample size; just spot checked
AvgBaColAlone=Alone_Ba%>%
  group_by(Mycotreatment)%>%
  drop_na(AMFPerRootLenCol)%>%
  summarise(AvgAMFcolonization=mean(AMFPerRootLenCol),
  sdAMFcol=sd(AMFPerRootLenCol))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
##take a closer look at contamination across treatments
TreatmentContam<-c("AMF","AMFEMF")

BaContamination=MCTdatafull%>%
  filter(FocalSpecies=="Ba"&
           Mycotreatment %in%TreatmentContam)%>%
  select(CompetitionTreatment,
         Mycotreatment,AMFPerRootLenCol)%>%
  drop_na(AMFPerRootLenCol)

View(BaContamination)


EMColxTreat<-aov(Alone_Bi$Percol_root~Alone_Bi$Mycotreatment)

summary(EMColxTreat)
```

    ##                        Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Alone_Bi$Mycotreatment  3 0.9430 0.31435   43.19 9.29e-13 ***
    ## Residuals              41 0.2984 0.00728                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 2 observations deleted due to missingness

``` r
TukeyHSD(EMColxTreat)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Alone_Bi$Percol_root ~ Alone_Bi$Mycotreatment)
    ## 
    ## $`Alone_Bi$Mycotreatment`
    ##                         diff         lwr         upr    p adj
    ## EMF-Sterile     3.125785e-01  0.21722257  0.40793436 0.000000
    ## AMF-Sterile     6.661338e-17 -0.09781181  0.09781181 1.000000
    ## AMFEMF-Sterile  2.646399e-01  0.17137998  0.35789979 0.000000
    ## AMF-EMF        -3.125785e-01 -0.41239072 -0.21276620 0.000000
    ## AMFEMF-EMF     -4.793858e-02 -0.14329447  0.04741731 0.539623
    ## AMFEMF-AMF      2.646399e-01  0.16682807  0.36245170 0.000000

``` r
##Does biomass improve with EM colonization for single pines; YES and significant difference for AMFEMF and EMF only (greater improveiment wiht just EMF)
MycoColonization<-c("AMFEMF","EMF")

Percol_biomass<-lmer(formula =sqrt(Totalbiomass_focal)~as.numeric(Percol_root)*Mycotreatment+(1|TrayNum), data=Alone_Bi %>%
                   filter(Mycotreatment %in%MycoColonization))

Percol_biomassRed<-lmer(formula =sqrt(Totalbiomass_focal)~as.numeric(Percol_root)+Mycotreatment+(1|TrayNum), data=Alone_Bi %>% filter(Mycotreatment %in%MycoColonization))

AIC(Percol_biomass,Percol_biomassRed)
```

    ##                   df        AIC
    ## Percol_biomass     6 -0.8993108
    ## Percol_biomassRed  5 -2.1430832

``` r
qqp(residuals(Percol_biomassRed))
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## [1]  7 17

``` r
BiomassxColonization<-ggpredict(Percol_biomassRed, terms=c("Percol_root[all]", "Mycotreatment"))
```

    ## Model has sqrt-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the sqrt-scale.

``` r
plot(BiomassxColonization)
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
##Colonization x biomass plot
AloneBiomassxColonization<-ggplot()+
  geom_point(data=Alone_Bi %>%
                filter(Mycotreatment %in% MycoColonization), 
    aes(x=as.numeric(Percol_root), 
        y=Totalbiomass_focal, color=Mycotreatment))+
  geom_line(data=BiomassxColonization, 
            aes(x = x, y = predicted,
                color=group))+
  geom_ribbon(data=BiomassxColonization,
              aes(x=x,ymin=conf.low, ymax=conf.high,
              fill=group), alpha=0.25) +
  scale_color_manual(values = Mycocolors, 
                     name="Mycorrhizal Treatment",
                     labels=c('EMF', "AMF+EMF"))+
  scale_fill_manual(values = Mycocolors2)+
  guides(fill = FALSE)+
  labs(x="EMF % colonization",
       y="Total pine biomass (g)")+
      theme(
        text = element_text(),
        panel.background = element_rect(fill="transparent", colour = NA),
        plot.background = element_rect(fill="transparent",colour = NA),
        panel.border = element_rect(fill="transparent", colour = "black"),
        axis.title.y = element_text(angle=90, size = 12, margin = margin(r=3, unit = "mm")),
        axis.title.x = element_text(size=12, margin = margin(t=3, unit = "mm")),
        axis.text.x = element_text(size=12, margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")),
        axis.text.y = element_text(size=12, margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
        axis.line = element_blank(),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.y = element_line(colour = "black"),
        axis.ticks.length=unit(-1.4, "mm"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_rect(fill="transparent", colour = NA),
            legend.box = "horizontal",
            legend.key.size= unit(4, "mm"),
            legend.text = element_text(size =11),
            legend.spacing = unit(0, "mm"),
            legend.background = element_rect(fill = "transparent", colour = NA),
            plot.margin=margin(t=2, r=2, b=2, l=2, unit = "mm"),
            strip.background=element_rect(fill="transparent", colour=NA),
            strip.text = element_text(face="bold"))
```

\#\#Nutrients

``` r
##Plant total leaf N
PerNmodel_Bi<-aov(PerN_Std~Mycotreatment, data=Alone_Bi)
  summary(PerNmodel_Bi)
```

    ##               Df Sum Sq Mean Sq F value  Pr(>F)   
    ## Mycotreatment  3  2.598  0.8659   5.105 0.00874 **
    ## Residuals     20  3.392  0.1696                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 23 observations deleted due to missingness

``` r
  TukeyHSD(PerNmodel_Bi)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = PerN_Std ~ Mycotreatment, data = Alone_Bi)
    ## 
    ## $Mycotreatment
    ##                      diff         lwr        upr     p adj
    ## EMF-Sterile     0.6344519 -0.03104135  1.2999451 0.0650798
    ## AMF-Sterile    -0.1968437 -0.86233687  0.4686495 0.8406137
    ## AMFEMF-Sterile  0.4178573 -0.24763586  1.0833506 0.3220337
    ## AMF-EMF        -0.8312955 -1.49678873 -0.1658023 0.0112251
    ## AMFEMF-EMF     -0.2165945 -0.88208772  0.4488987 0.7992639
    ## AMFEMF-AMF      0.6147010 -0.05079220  1.2801942 0.0766928

``` r
PerNmodel_Ba<-aov(PerN_Std~Mycotreatment, data=Alone_Ba)
  summary(PerNmodel_Ba)
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## Mycotreatment  3  0.449  0.1498   0.518  0.674
    ## Residuals     20  5.782  0.2891               
    ## 25 observations deleted due to missingness

``` r
  TukeyHSD(PerNmodel_Ba)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = PerN_Std ~ Mycotreatment, data = Alone_Ba)
    ## 
    ## $Mycotreatment
    ##                       diff        lwr       upr     p adj
    ## EMF-Sterile     0.33700828 -0.5318532 1.2058697 0.7020374
    ## AMF-Sterile     0.01615049 -0.8527110 0.8850119 0.9999476
    ## AMFEMF-Sterile  0.17719784 -0.6916636 1.0460593 0.9396201
    ## AMF-EMF        -0.32085779 -1.1897192 0.5480037 0.7321321
    ## AMFEMF-EMF     -0.15981043 -1.0286719 0.7090510 0.9545778
    ## AMFEMF-AMF      0.16104736 -0.7078141 1.0299088 0.9535944

``` r
##d15N 
d15Nmodel_Bi<-aov(`d 15N (‰)`~Mycotreatment, data=Alone_Bi)
  summary(d15Nmodel_Bi)
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)  
    ## Mycotreatment  3  50.66  16.887   4.532  0.014 *
    ## Residuals     20  74.52   3.726                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 23 observations deleted due to missingness

``` r
  TukeyHSD(d15Nmodel_Bi)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = `d 15N (‰)` ~ Mycotreatment, data = Alone_Bi)
    ## 
    ## $Mycotreatment
    ##                      diff       lwr         upr     p adj
    ## EMF-Sterile    -2.6297305 -5.748983  0.48952225 0.1179270
    ## AMF-Sterile    -3.0885055 -6.207758  0.03074731 0.0528924
    ## AMFEMF-Sterile -3.8685934 -6.987846 -0.74934057 0.0118631
    ## AMF-EMF        -0.4587749 -3.578028  2.66047786 0.9758274
    ## AMFEMF-EMF     -1.2388628 -4.358116  1.88038998 0.6867339
    ## AMFEMF-AMF     -0.7800879 -3.899341  2.33916492 0.8958698

``` r
d15Nmodel_Ba<-aov(`d 15N (‰)`~Mycotreatment, data=Alone_Ba)
  summary(d15Nmodel_Ba)
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## Mycotreatment  3   7.76   2.588   1.139  0.357
    ## Residuals     20  45.44   2.272               
    ## 25 observations deleted due to missingness

``` r
  TukeyHSD(d15Nmodel_Ba)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = `d 15N (‰)` ~ Mycotreatment, data = Alone_Ba)
    ## 
    ## $Mycotreatment
    ##                      diff        lwr      upr     p adj
    ## EMF-Sterile    -0.2536851 -2.6893423 2.181972 0.9911027
    ## AMF-Sterile     0.3920742 -2.0435830 2.827731 0.9687528
    ## AMFEMF-Sterile  1.2474973 -1.1881599 3.683155 0.4941312
    ## AMF-EMF         0.6457592 -1.7898980 3.081416 0.8789198
    ## AMFEMF-EMF      1.5011824 -0.9344748 3.936840 0.3374402
    ## AMFEMF-AMF      0.8554231 -1.5802341 3.291080 0.7605688

``` r
#d13C
d13Cmodel_Bi<-aov(`d 13C (‰)`~Mycotreatment, data=Alone_Bi)
  summary(d13Cmodel_Bi)
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## Mycotreatment  3  0.713  0.2377   0.303  0.823
    ## Residuals     20 15.693  0.7847               
    ## 23 observations deleted due to missingness

``` r
  TukeyHSD(d13Cmodel_Bi)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = `d 13C (‰)` ~ Mycotreatment, data = Alone_Bi)
    ## 
    ## $Mycotreatment
    ##                       diff       lwr       upr     p adj
    ## EMF-Sterile     0.36284068 -1.068610 1.7942918 0.8921608
    ## AMF-Sterile     0.03713227 -1.394319 1.4685834 0.9998578
    ## AMFEMF-Sterile -0.09506109 -1.526512 1.3363900 0.9976435
    ## AMF-EMF        -0.32570841 -1.757160 1.1057427 0.9188443
    ## AMFEMF-EMF     -0.45790177 -1.889353 0.9735493 0.8073233
    ## AMFEMF-AMF     -0.13219336 -1.563644 1.2992578 0.9937498

``` r
d13Cmodel_Ba<-aov(`d 13C (‰)`~Mycotreatment, data=Alone_Ba)
  summary(d13Cmodel_Ba)
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## Mycotreatment  3   3.02   1.008   0.631  0.604
    ## Residuals     20  31.97   1.598               
    ## 25 observations deleted due to missingness

``` r
  TukeyHSD(d13Cmodel_Ba)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = `d 13C (‰)` ~ Mycotreatment, data = Alone_Ba)
    ## 
    ## $Mycotreatment
    ##                       diff       lwr      upr     p adj
    ## EMF-Sterile    -0.54911005 -2.592118 1.493898 0.8746196
    ## AMF-Sterile    -0.56356560 -2.606574 1.479443 0.8660749
    ## AMFEMF-Sterile -1.00068480 -3.043693 1.042323 0.5309982
    ## AMF-EMF        -0.01445555 -2.057464 2.028553 0.9999971
    ## AMFEMF-EMF     -0.45157476 -2.494583 1.591433 0.9249077
    ## AMFEMF-AMF     -0.43711920 -2.480127 1.605889 0.9312143

``` r
##PLOTS
d15N<-ggplot(data=Alone,
             aes(x=Mycotreatment, `d 15N (‰)`, fill=Mycotreatment, color=Mycotreatment))+
  geom_boxplot(aes())+
  facet_wrap(~FocalSpecies,
             labeller = labeller(FocalSpecies=plantlabels))+
  labs(x="Mycorrhizal Treatment", 
       y=expression(delta~paste("" ^"15")~ "N (‰)"))+
  scale_fill_manual(values=Mycocolors)+
  scale_color_manual(values=Mycocolors2)+
  scale_x_discrete(labels = c('None','EMF','AMF', "AMF+EMF"))+
  theme(
        text = element_text(),
        panel.background = element_rect(fill="transparent", colour = NA),
        plot.background = element_rect(fill="transparent",colour = NA),
        panel.border = element_rect(fill="transparent", colour = "black"),
        axis.title.y = element_text(angle=90, size = 12, margin = margin(r=3, unit = "mm")),
        axis.title.x = element_text(size=12, margin = margin(t=3, unit = "mm")),
        axis.text.x = element_text(size=8, margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")),
        axis.text.y = element_text(size=10, margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
        axis.line = element_blank(),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.y = element_line(colour = "black"),
        axis.ticks.length=unit(-1.4, "mm"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face="italic"),
        legend.position = "none")

PerN_std<-ggplot(data=Alone,
                 aes(x=Mycotreatment,PerN_Std,
                     fill=Mycotreatment,
                     color=Mycotreatment))+
  geom_boxplot(aes())+
  facet_wrap(~FocalSpecies, scales = "free_y",
             labeller = labeller(FocalSpecies=plantlabels))+
  scale_fill_manual(values=Mycocolors)+
  scale_color_manual(values=Mycocolors2)+
  scale_x_discrete(labels =c('None','EMF','AMF',                                       "AMF+EMF"))+
  labs(x="Mycorrhizal Treatment", y="Leaf N (per plant)")+
  theme(
        text = element_text(),
        panel.background = element_rect(fill="transparent", colour = NA),
        plot.background = element_rect(fill="transparent",colour = NA),
        panel.border = element_rect(fill="transparent", colour = "black"),
        axis.title.y = element_text(angle=90, size = 12, margin = margin(r=3, unit = "mm")),
        axis.title.x = element_text(size=12, margin = margin(t=3, unit = "mm")),
        axis.text.x = element_text(size=8, margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")),
        axis.text.y = element_text(size=10, margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
        axis.line = element_blank(),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.y = element_line(colour = "black"),
        axis.ticks.length=unit(-1.4, "mm"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face="italic"),
        legend.position = "none")


d13C<-ggplot(data=Alone, 
             aes(x=Mycotreatment, `d 13C (‰)`))+
  geom_boxplot(aes(fill=Mycotreatment))+
  facet_wrap(~FocalSpecies)+
  scale_fill_manual(values=Mycocolors)+
  theme(
        text = element_text(),
        panel.background = element_rect(fill="transparent", colour = NA),
        plot.background = element_rect(fill="transparent",colour = NA),
        panel.border = element_rect(fill="transparent", colour = "black"),
        axis.title.y = element_text(angle=90, size = 12, margin = margin(r=3, unit = "mm")),
        axis.title.x = element_text(size=12, margin = margin(t=3, unit = "mm")),
        axis.text.x = element_text(size=12, margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")),
        axis.text.y = element_text(size=12, margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
        axis.line = element_blank(),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.y = element_line(colour = "black"),
        axis.ticks.length=unit(-1.4, "mm"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right")
```

\#H2 Competition \#\#Biomass \#\#\#Baccharis \#\#\#\#AMF

``` r
##data structure
baAMFhist<-MCTdatafull%>%
         filter(Mycotreatment=="AMF",
                FocalSpecies=="Ba")%>%
     pull(Totalbiomass_focal)

hist(baAMFhist)
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
shapiro.test((baAMFhist))
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  (baAMFhist)
    ## W = 0.95, p-value = 0.03692

``` r
##pretty right-skewed, so trying a sqrt transformation

hist(sqrt(baAMFhist))
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
shapiro.test(sqrt(baAMFhist))
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  sqrt(baAMFhist)
    ## W = 0.98726, p-value = 0.8696

``` r
#fixed

#models
BAAMF_1_COMPNUM<-lmer(formula =sqrt(Totalbiomass_focal)~
         Competitornum:CompetitionTreatment+(1|TrayNum), 
  data = MCTdatafull%>%
    filter(Mycotreatment=="AMF",
           FocalSpecies=="Ba"))
```

    ## fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

``` r
BAAMF_2_COMPNUM<-lmer(formula =sqrt(Totalbiomass_focal)~
         Competitornum+(1|TrayNum), 
  data = MCTdatafull%>%
    filter(Mycotreatment=="AMF",
           FocalSpecies=="Ba"))

AIC(BAAMF_1_COMPNUM,BAAMF_2_COMPNUM)
```

    ##                 df      AIC
    ## BAAMF_1_COMPNUM  5 55.44302
    ## BAAMF_2_COMPNUM  4 59.29004

``` r
qqp(residuals(BAAMF_1_COMPNUM))
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

    ## [1] 11 27

``` r
BaAMF_model_compnum<-summary(BAAMF_1_COMPNUM)

BaAMF_aAB_compnum<-BaAMF_model_compnum$coefficients[3,1]/BaAMF_model_compnum$coefficients[1,1]
BaAMF_aAA_compnum<-BaAMF_model_compnum$coefficients[2,1]/BaAMF_model_compnum$coefficients[1,1]

Mycotreatment=c("AMF","AMFEMF","EMF", "Sterile")
alphatableCompNum<-data.frame(Mycotreatment)
alphatableCompNum$aAB[alphatableCompNum$Mycotreatment=="AMF"]<-BaAMF_aAB_compnum
alphatableCompNum$aAA[alphatableCompNum$Mycotreatment=="AMF"]<-BaAMF_aAA_compnum

BAAMF_predictions_CompNum<-ggemmeans(BAAMF_1_COMPNUM,
        terms=c("Competitornum[all]", 
        "CompetitionTreatment[Heterospecific, Conspecific]"),
        back.transform=T, interval="confidence")
```

    ## Loading required namespace: emmeans

    ## NOTE: A nesting structure was detected in the fitted model:
    ##     Competitornum %in% (Competitornum*CompetitionTreatment), CompetitionTreatment %in% (Competitornum*CompetitionTreatment)

    ## Model has sqrt-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the sqrt-scale.

``` r
legend.plot<-ggplot()+
  geom_jitter(MCTdatafull%>%
         filter(Mycotreatment=="AMF",
                FocalSpecies=="Ba"),
             mapping =aes(x=Competitornum, 
                 y=Totalbiomass_focal, 
         color=CompetitionTreatment), width = 0.1)+
  geom_line(data=BAAMF_predictions_CompNum, 
            aes(x = x, y = predicted, 
                colour = group))+
  geom_hline(yintercept = 0, linetype="dashed", alpha=0.7)+
  scale_color_manual(values=CompTreatColors)+
  theme(panel.background = element_rect(fill="transparent", colour = NA),
            plot.background = element_rect(fill="transparent",colour = NA),
            panel.border = element_rect(fill="transparent", colour = "black"),
        legend.key = element_rect(fill = NA),
      legend.margin=margin(c(1,5,5,5)))

CompPlotLegend<-get_legend(legend.plot)


BA_AMF_compnum<-ggplot()+
  geom_jitter(MCTdatafull%>%
         filter(Mycotreatment=="AMF",
                FocalSpecies=="Ba"),
             mapping =aes(x=Competitornum, 
                 y=Totalbiomass_focal, 
         color=CompetitionTreatment), width = 0.1)+
  geom_line(data=BAAMF_predictions_CompNum, 
            aes(x = x, y = predicted, 
                colour = group))+
  geom_ribbon(data=BAAMF_predictions_CompNum,
              aes(x=x,
                  ymin=conf.low, ymax=conf.high,
              fill=group), alpha=0.25)+
  geom_hline(yintercept = 0, linetype="dashed", alpha=0.7)+
  scale_color_manual(values=CompTreatColors)+
  scale_fill_manual(values=CompTreatColors)+
  labs(title="AMF", 
       x="Number of competitors",
       y="")+
  ylim(0,6)+
  theme(panel.background = element_rect(fill="transparent", colour = NA),
            plot.background = element_rect(fill="transparent",colour = NA),
            panel.border = element_rect(fill="transparent", colour = "black"),
        legend.position = "none",
        legend.background = element_rect(fill="transparent",colour = NA), 
        legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        axis.text.y=element_blank())
```

\#\#\#\#EMF

``` r
baEMFhist<-MCTdatafull%>%
         filter(Mycotreatment=="EMF",
                FocalSpecies=="Ba")%>%
     pull(Totalbiomass_focal)
hist(sqrt(baEMFhist))
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
shapiro.test(sqrt(baEMFhist))
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  sqrt(baEMFhist)
    ## W = 0.97131, p-value = 0.2728

``` r
##sqrt transformation =normal data

BAEMF_1_CompNum<-lmer(formula =sqrt(Totalbiomass_focal)~
   Competitornum:CompetitionTreatment + (1|TrayNum), 
   data = MCTdatafull%>%
     filter(Mycotreatment=="EMF",
            FocalSpecies=="Ba"))
```

    ## fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

``` r
qqp(residuals(BAEMF_1_CompNum))
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

    ## [1] 7 2

``` r
#tab_model(BAEMF_3_CompNum)


BAEMF_model_compnum<-summary(BAEMF_1_CompNum)

BAEMF_aAB_compnum<-BAEMF_model_compnum$coefficients[3,1]/BAEMF_model_compnum$coefficients[1,1]
BAEMF_aAA_compnum<-BAEMF_model_compnum$coefficients[2,1]/BAEMF_model_compnum$coefficients[1,1]

alphatableCompNum$aAB[alphatableCompNum$Mycotreatment=="EMF"]<-BAEMF_aAB_compnum
alphatableCompNum$aAA[alphatableCompNum$Mycotreatment=="EMF"]<-BAEMF_aAA_compnum


BAEMF_predictions_compnum<-ggemmeans(BAEMF_1_CompNum,
        terms=c("Competitornum[all]", 
        "CompetitionTreatment[Heterospecific, Conspecific]"),
        back.transform=T, interval="confidence")
```

    ## NOTE: A nesting structure was detected in the fitted model:
    ##     Competitornum %in% (Competitornum*CompetitionTreatment), CompetitionTreatment %in% (Competitornum*CompetitionTreatment)

    ## Model has sqrt-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the sqrt-scale.

``` r
BA_EMF_compnum<-ggplot()+
  geom_jitter(MCTdatafull%>%
         filter(Mycotreatment=="EMF",
                FocalSpecies=="Ba"),
             mapping =aes(x=Competitornum, 
                 y=Totalbiomass_focal, 
         color=CompetitionTreatment), width=0.1)+
   geom_line(data=BAEMF_predictions_compnum, 
            aes(x = x, y = predicted,
                color=group))+
  geom_ribbon(data=BAEMF_predictions_compnum,
              aes(x=x,
                  ymin=conf.low, ymax=conf.high,
              fill=group), alpha=0.25)+
  geom_hline(yintercept = 0, linetype="dashed", alpha=0.7)+
  scale_color_manual(values=CompTreatColors)+
  scale_fill_manual(values=CompTreatColors)+
  labs(title="EMF", 
       x="Number of competitors",
       y="")+
  ylim(0,6)+
  theme(panel.background = element_rect(fill="transparent", colour = NA),
            plot.background = element_rect(fill="transparent",colour = NA),
            panel.border = element_rect(fill="transparent", colour = "black"),
            legend.position = "none", axis.text.y=element_blank(), 
            plot.title =element_text(hjust=0.5))
```

\#\#\#\#AMFEMF

``` r
baAMFEMFhist<-MCTdatafull%>%
         filter(Mycotreatment=="AMFEMF",
                FocalSpecies=="Ba")%>%
     pull(Totalbiomass_focal)
hist(sqrt(baAMFEMFhist))
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
shapiro.test(sqrt(baAMFEMFhist))
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  sqrt(baAMFEMFhist)
    ## W = 0.97891, p-value = 0.534

``` r
#normal

BAAMFEMF_1_compnum<-lmer(formula =sqrt(Totalbiomass_focal)~
   Competitornum:CompetitionTreatment+
     (1|TrayNum), 
   data = MCTdatafull%>%
     filter(Mycotreatment=="AMFEMF",
            FocalSpecies=="Ba"))
```

    ## fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

``` r
qqp(residuals(BAAMFEMF_1_compnum))
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

    ## [1]  7 43

``` r
BAAMFEMF_model_compnum<-summary(BAAMFEMF_1_compnum)

BAAMFEMF_aAB_compnum<-BAAMFEMF_model_compnum$coefficients[3,1]/BAAMFEMF_model_compnum$coefficients[1,1]
BAAMFEMF_aAA_compnum<-BAAMFEMF_model_compnum$coefficients[2,1]/BAAMFEMF_model_compnum$coefficients[1,1]

alphatableCompNum$aAB[alphatableCompNum$Mycotreatment=="AMFEMF"]<-BAAMFEMF_aAB_compnum
alphatableCompNum$aAA[alphatableCompNum$Mycotreatment=="AMFEMF"]<-BAAMFEMF_aAA_compnum

BAAMFEMF_predictions_compnum<-ggemmeans(BAAMFEMF_1_compnum,
        terms=c("Competitornum[all]", 
        "CompetitionTreatment[Heterospecific, Conspecific]"),
        back.transform=T, interval="confidence")
```

    ## NOTE: A nesting structure was detected in the fitted model:
    ##     Competitornum %in% (Competitornum*CompetitionTreatment), CompetitionTreatment %in% (Competitornum*CompetitionTreatment)

    ## Model has sqrt-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the sqrt-scale.

``` r
BA_AMFEMF_compnum<-ggplot()+
  geom_jitter(MCTdatafull%>%
         filter(Mycotreatment=="AMFEMF",
                FocalSpecies=="Ba"),
             mapping =aes(x=Competitornum, 
                 y=Totalbiomass_focal, 
         color=CompetitionTreatment), width=0.1)+
   geom_ribbon(data=BAAMF_predictions_CompNum,
              aes(x=x,
                  ymin=conf.low, ymax=conf.high,
              fill=group), alpha=0.25)+
    scale_color_manual(values=CompTreatColors)+
    scale_fill_manual(values=CompTreatColors)+
   geom_line(data=BAAMF_predictions_CompNum, 
            aes(x = x, y = predicted,
                color=group))+
  geom_hline(yintercept = 0, linetype="dashed", alpha=0.7)+
  labs(title="AMF+EMF", 
       x="Number of competitors",
       y="")+
  ylim(0,6)+
  theme(panel.background = element_rect(fill="transparent", colour = NA),
            plot.background = element_rect(fill="transparent",colour = NA),
            panel.border = element_rect(fill="transparent", colour = "black"),
            legend.position = "none", axis.text.y = element_blank(), 
            plot.title = element_text(hjust = 0.5))
```

\#\#\#\#None

``` r
basterilehist<-MCTdatafull%>%
         filter(Mycotreatment=="Sterile",
                FocalSpecies=="Ba")%>%
     pull(Totalbiomass_focal)
hist(sqrt(basterilehist))
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
shapiro.test(sqrt(basterilehist))
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  sqrt(basterilehist)
    ## W = 0.92353, p-value = 0.00397

``` r
##not normal with sqrt transformation; try fitting lmer and looking at residuals 

BASterile_1_compnum<-lmer(formula =sqrt(Totalbiomass_focal)~
   Competitornum:CompetitionTreatment+(1|TrayNum), 
   data = MCTdatafull%>%
     filter(Mycotreatment=="Sterile",
            FocalSpecies=="Ba"))
```

    ## fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

``` r
qqp(residuals(BASterile_1_compnum))
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

    ## [1] 32 25

``` r
##residuals look good 
#tab_model(BASterile_1_compnum)

BASterile_model_compnum<-summary(BASterile_1_compnum)

BASterile_aAB_compnum<-BASterile_model_compnum$coefficients[3,1]/BASterile_model_compnum$coefficients[1,1]
BASterile_aAA_compnum<-BASterile_model_compnum$coefficients[2,1]/BASterile_model_compnum$coefficients[1,1]

alphatableCompNum$aAB[alphatableCompNum$Mycotreatment=="Sterile"]<-BASterile_aAB_compnum
alphatableCompNum$aAA[alphatableCompNum$Mycotreatment=="Sterile"]<-BASterile_aAA_compnum


BASterile_predictions_compnum<-ggemmeans(BASterile_1_compnum,
        terms=c("Competitornum[all]", 
        "CompetitionTreatment[Heterospecific, Conspecific]"),
        back.transform=T, interval="confidence")
```

    ## NOTE: A nesting structure was detected in the fitted model:
    ##     Competitornum %in% (Competitornum*CompetitionTreatment), CompetitionTreatment %in% (Competitornum*CompetitionTreatment)

    ## Model has sqrt-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the sqrt-scale.

``` r
BA_Sterile_compnum<-ggplot()+
  geom_jitter(MCTdatafull%>%
         filter(Mycotreatment=="Sterile",
                FocalSpecies=="Ba"),
             mapping =aes(x=Competitornum, 
                 y=Totalbiomass_focal, 
         color=CompetitionTreatment), width=0.1)+
  geom_ribbon(data=BASterile_predictions_compnum,
              aes(x=x,
                  ymin=conf.low, ymax=conf.high,
              fill=group), alpha=0.25)+
  geom_hline(yintercept = 0, linetype="dashed", alpha=0.7)+
  geom_line(data=BASterile_predictions_compnum, 
            aes(x = x, y = predicted,
                color=group))+
  geom_hline(yintercept = 0, linetype="dashed", alpha=0.7)+
  scale_color_manual(values=CompTreatColors)+
  labs(title="None", 
       x="Number of competitors",
       y="")+
  ylim(0,6)+
  theme(panel.background = element_rect(fill="transparent", colour = NA),
            plot.background = element_rect(fill="transparent",colour = NA),
            panel.border = element_rect(fill="transparent", colour = "black"),
            legend.position = "none", 
            plot.title = element_text(hjust = 0.5))
```

\#\#\#Pinus \#\#\#\#None

``` r
bisterilehist<-MCTdatafull%>%
         filter(Mycotreatment=="Sterile",
                FocalSpecies=="Bi")%>%
     pull(Totalbiomass_focal)
hist(sqrt(bisterilehist))
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
shapiro.test(sqrt(bisterilehist))
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  sqrt(bisterilehist)
    ## W = 0.9305, p-value = 0.007114

``` r
##transformation helps but still not normal

BiSterile_1_compnum<-lmer(formula =sqrt(Totalbiomass_focal)~
       Competitornum:CompetitionTreatment +(1|TrayNum),
     data = MCTdatafull%>%
       filter(Mycotreatment=="Sterile",
              FocalSpecies=="Bi"))
```

    ## fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

``` r
qqp(resid(BiSterile_1_compnum))
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

    ## [1] 16 26

``` r
BiSterile_model_compnum<-summary(BiSterile_1_compnum)

BiSterile_aBA_compnum<-BiSterile_model_compnum$coefficients[3,1]/BiSterile_model_compnum$coefficients[1,1]
BiSterile_aBB_compnum<-BiSterile_model_compnum$coefficients[2,1]/BiSterile_model_compnum$coefficients[1,1]

alphatableCompNum$aBA[alphatableCompNum$Mycotreatment=="Sterile"]<-BiSterile_aBA_compnum
alphatableCompNum$aBB[alphatableCompNum$Mycotreatment=="Sterile"]<-BiSterile_aBB_compnum

BiSterile_predictions_compnum<-ggemmeans(BiSterile_1_compnum,
        terms=c("Competitornum[all]", 
        "CompetitionTreatment[Heterospecific, Conspecific]"),
        back.transform=T, interval="confidence")
```

    ## NOTE: A nesting structure was detected in the fitted model:
    ##     Competitornum %in% (Competitornum*CompetitionTreatment), CompetitionTreatment %in% (Competitornum*CompetitionTreatment)

    ## Model has sqrt-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the sqrt-scale.

``` r
Bi_Sterile_CompNum<-ggplot()+
  geom_jitter(MCTdatafull%>%
         filter(Mycotreatment=="Sterile",
                FocalSpecies=="Bi"),
             mapping =aes(x=Competitornum, 
                 y=Totalbiomass_focal, 
         color=CompetitionTreatment), width = 0.1)+
  geom_line(data=BiSterile_predictions_compnum, 
            aes(x = x, y = predicted,
                color=group))+
   geom_ribbon(data=BiSterile_predictions_compnum,
              aes(x=x,
                  ymin=conf.low, ymax=conf.high,
              fill=group), alpha=0.25)+
  scale_color_manual(values=CompTreatColors)+
  scale_fill_manual(values=CompTreatColors)+
  geom_hline(yintercept = 0, linetype="dashed", alpha=0.7)+
  labs(title="", 
       x="Number of competitors",
       y="")+
  ylim(0,2)+
  theme(panel.background = element_rect(fill="transparent", colour = NA),
            plot.background = element_rect(fill="transparent",colour = NA),
            panel.border = element_rect(fill="transparent", colour = "black"),
            legend.position = "none", 
            plot.title = element_text(hjust = 0.5))
```

\#\#\#\#EMF

``` r
biEMFhist<-MCTdatafull%>%
         filter(Mycotreatment=="EMF",
                FocalSpecies=="Bi")%>%
     pull(Totalbiomass_focal)
hist(sqrt(biEMFhist))
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
shapiro.test(sqrt(biEMFhist))
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  sqrt(biEMFhist)
    ## W = 0.9574, p-value = 0.08485

``` r
#transformation =just barely normal

BiEMF_1comp<-lmer(formula =sqrt(Totalbiomass_focal)~
     Competitornum:CompetitionTreatment+
     (1|TrayNum),
     data = MCTdatafull%>%
       filter(Mycotreatment=="EMF",
              FocalSpecies=="Bi"))
```

    ## fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

    ## boundary (singular) fit: see ?isSingular

``` r
BiEMF_2comp<-lmer(formula =sqrt(Totalbiomass_focal)~
     Competitornum+
       I(Competitornum^2):CompetitionTreatment+
       Competitornum:CompetitionTreatment+
     (1|TrayNum),
     data = MCTdatafull%>%
       filter(Mycotreatment=="EMF",
              FocalSpecies=="Bi"))
```

    ## fixed-effect model matrix is rank deficient so dropping 2 columns / coefficients
    ## boundary (singular) fit: see ?isSingular

``` r
AIC(BiEMF_1comp,BiEMF_2comp)
```

    ##             df      AIC
    ## BiEMF_1comp  5 31.51181
    ## BiEMF_2comp  7 45.27943

``` r
BiEMF_predictions_compnum<-ggemmeans(BiEMF_2comp,
        terms=c("Competitornum[all]", 
        "CompetitionTreatment[Heterospecific, Conspecific]"),
        back.transform=T, interval="confidence")
```

    ## Model has sqrt-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the sqrt-scale.

``` r
plot(BiEMF_predictions_compnum)
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
qqp(residuals(BiEMF_2comp))
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->

    ## [1] 39 36

``` r
BiEMF_model_compnum<-summary(BiEMF_1comp)

BiEMF_aBA_compnum<-BiEMF_model_compnum$coefficients[3,1]/BiEMF_model_compnum$coefficients[1,1]
BiEMF_aBB_compnum<-BiEMF_model_compnum$coefficients[2,1]/BiEMF_model_compnum$coefficients[1,1]

alphatableCompNum$aBA[alphatableCompNum$Mycotreatment=="EMF"]<-BiEMF_aBA_compnum
alphatableCompNum$aBB[alphatableCompNum$Mycotreatment=="EMF"]<-BiEMF_aBB_compnum


Bi_EMF_CompNum<-ggplot()+
  geom_jitter(MCTdatafull%>%
         filter(Mycotreatment=="EMF",
                FocalSpecies=="Bi"),
             mapping =aes(x=Competitornum, 
                 y=Totalbiomass_focal, 
         color=CompetitionTreatment), width = 0.1)+
  geom_line(data=BiEMF_predictions_compnum, 
            aes(x = x, y = predicted, 
                colour = group))+ 
  geom_ribbon(data=BiEMF_predictions_compnum,
              aes(x=x,
                  ymin=conf.low, ymax=conf.high,
              fill=group), alpha=0.25)+
  scale_color_manual(values=CompTreatColors)+
  scale_fill_manual(values=CompTreatColors)+
  geom_hline(yintercept = 0, linetype="dashed", alpha=0.7)+
  labs(title="", 
       x="Number of competitors",
       y="")+
  ylim(0,2)+
  theme(panel.background = element_rect(fill="transparent", colour = NA),
            plot.background = element_rect(fill="transparent",colour = NA),
            panel.border = element_rect(fill="transparent", colour = "black"),
        legend.position = "none",axis.text.y=element_blank(),
        plot.title = element_text(hjust = 0.5))
```

\#\#\#\#AMF

``` r
biAMFhist<-MCTdatafull%>%
         filter(Mycotreatment=="AMF",
                FocalSpecies=="Bi")%>%
     pull(Totalbiomass_focal)
hist(sqrt(biAMFhist))
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
shapiro.test(sqrt(biAMFhist))
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  sqrt(biAMFhist)
    ## W = 0.98852, p-value = 0.926

``` r
#now normal

BiAMF_1_compnum<-lmer(formula =sqrt(Totalbiomass_focal)~
       Competitornum:CompetitionTreatment+ (1|TrayNum),
     data = MCTdatafull%>%
       filter(Mycotreatment=="AMF",
              FocalSpecies=="Bi"))
```

    ## fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

``` r
qqp(residuals(BiAMF_1_compnum))
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

    ## 15  6 
    ## 14  6

``` r
#tab_model(BiAMF_1_compnum)

BiAMF_model_compnum<-summary(BiAMF_1_compnum)

BiAMF_aBA_compnum<-BiAMF_model_compnum$coefficients[3,1]/BiAMF_model_compnum$coefficients[1,1]
BiAMF_aBB_compnum<-BiEMF_model_compnum$coefficients[2,1]/BiEMF_model_compnum$coefficients[1,1]

alphatableCompNum$aBA[alphatableCompNum$Mycotreatment=="AMF"]<-BiAMF_aBA_compnum
alphatableCompNum$aBB[alphatableCompNum$Mycotreatment=="AMF"]<-BiAMF_aBB_compnum


BiAMF_predictions_compnum<-ggemmeans(BiAMF_1_compnum,
        terms=c("Competitornum[all]", 
        "CompetitionTreatment[Heterospecific, Conspecific]"),
        back.transform=T, interval="confidence")
```

    ## NOTE: A nesting structure was detected in the fitted model:
    ##     Competitornum %in% (Competitornum*CompetitionTreatment), CompetitionTreatment %in% (Competitornum*CompetitionTreatment)

    ## Model has sqrt-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the sqrt-scale.

``` r
Bi_AMF_Compnum<-ggplot()+
  geom_jitter(MCTdatafull%>%
         filter(Mycotreatment=="AMF",
                FocalSpecies=="Bi"),
             mapping =aes(x=Competitornum, 
                 y=Totalbiomass_focal, 
         color=CompetitionTreatment))+
  geom_line(data=BiAMF_predictions_compnum, 
            aes(x = x, y = predicted,
                color=group))+
  geom_ribbon(data=BiAMF_predictions_compnum,
              aes(x=x,
                  ymin=conf.low, ymax=conf.high,
              fill=group), alpha=0.25)+
  scale_color_manual(values=CompTreatColors)+
  scale_fill_manual(values=CompTreatColors)+
  geom_hline(yintercept = 0, linetype="dashed", alpha=0.7)+
  labs(title="", 
       x="Number of competitors",
       y="")+
  ylim(-0.02,2)+
  theme(panel.background = element_rect(fill="transparent", colour = NA),
            plot.background = element_rect(fill="transparent",colour = NA),
            panel.border = element_rect(fill="transparent", colour = "black"),
        legend.position = "none",
        legend.background = element_rect(fill="transparent",colour = NA), 
        legend.title = element_blank(),
        axis.text.y=element_blank(),
        plot.title = element_text(hjust = 0.5))
```

\#\#\#\#AMFEMF

``` r
biAMFEMFhist<-MCTdatafull%>%
         filter(Mycotreatment=="AMFEMF",
                FocalSpecies=="Bi")%>%
     pull(Totalbiomass_focal)
hist(sqrt(biAMFEMFhist))
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
shapiro.test(sqrt(biAMFEMFhist))
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  sqrt(biAMFEMFhist)
    ## W = 0.98859, p-value = 0.9181

``` r
BiAMFEMF_1_compnum<-lmer(formula =sqrt(Totalbiomass_focal)~
       Competitornum:CompetitionTreatment+(1|TrayNum),
     data = MCTdatafull%>%
       filter(Mycotreatment=="AMFEMF",
              FocalSpecies=="Bi"))
```

    ## fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

``` r
BiEMF_2comp<-lmer(formula =sqrt(Totalbiomass_focal)~
     Competitornum+
       I(Competitornum^3):CompetitionTreatment+
       I(Competitornum^2):CompetitionTreatment+
       Competitornum:CompetitionTreatment+
     (1|TrayNum),
     data = MCTdatafull%>%
       filter(Mycotreatment=="AMFEMF",
              FocalSpecies=="Bi"))
```

    ## fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients

``` r
qqp(residuals(BiAMFEMF_1_compnum))
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

    ## [1] 39 25

``` r
#tab_model(BiAMFEMF_1_compnum)


BiAMFEMF_model_compnum<-summary(BiAMFEMF_1_compnum)

BiAMFEMF_aBA_compnum<-BiAMFEMF_model_compnum$coefficients[3,1]/BiAMFEMF_model_compnum$coefficients[1,1]
BiAMFEMF_aBB_compnum<-BiAMFEMF_model_compnum$coefficients[2,1]/BiAMFEMF_model_compnum$coefficients[1,1]

alphatableCompNum$aBA[alphatableCompNum$Mycotreatment=="AMFEMF"]<-BiAMFEMF_aBA_compnum
alphatableCompNum$aBB[alphatableCompNum$Mycotreatment=="AMFEMF"]<-BiAMFEMF_aBB_compnum


BiEMFAMF_predictions_compnum<-ggemmeans(BiAMFEMF_1_compnum,
        terms=c("Competitornum[all]", 
        "CompetitionTreatment[Heterospecific, Conspecific]"),
        back.transform=T, interval="confidence")
```

    ## NOTE: A nesting structure was detected in the fitted model:
    ##     Competitornum %in% (Competitornum*CompetitionTreatment), CompetitionTreatment %in% (Competitornum*CompetitionTreatment)

    ## Model has sqrt-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the sqrt-scale.

``` r
plot(BiEMFAMF_predictions_compnum)
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->

``` r
Bi_AMFEMF_CompNum<-ggplot()+
  geom_point(MCTdatafull%>%
         filter(Mycotreatment=="AMFEMF",
                FocalSpecies=="Bi"),
             mapping =aes(x=Competitornum, 
                 y=Totalbiomass_focal, 
         color=CompetitionTreatment))+
  geom_line(data=BiEMFAMF_predictions_compnum, 
            aes(x = x, y = predicted, 
                color=group))+
   geom_ribbon(data=BiAMF_predictions_compnum,
              aes(x=x,
                  ymin=conf.low, ymax=conf.high,
              fill=group), alpha=0.25)+
  scale_color_manual(values=CompTreatColors)+
  scale_fill_manual(values=CompTreatColors)+
  geom_hline(yintercept = 0, linetype="dashed", alpha=0.7)+
  labs(title="", 
       x="Number of competitors",
       y="")+
  ylim(-0.01,2)+
  theme(panel.background = element_rect(fill="transparent", colour = NA),
            plot.background = element_rect(fill="transparent",colour = NA),
            panel.border = element_rect(fill="transparent", colour = "black"),
        legend.position = "none", axis.text.y=element_blank(), 
        plot.title = element_text(hjust = 0.5))
```

\#\#Biomass plots combined

``` r
Bimodels_CompNum<-cowplot::plot_grid(Bi_Sterile_CompNum,Bi_EMF_CompNum, Bi_AMF_Compnum,Bi_AMFEMF_CompNum,
                  labels = c("", ""), 
                  label_size = 14,
                  label_fontfamily = "sans",
                  ncol = 4)
```

    ## Warning: Removed 1 rows containing missing values (geom_point).

``` r
Bamodels_CompNum<-cowplot::plot_grid(BA_Sterile_compnum,BA_EMF_compnum,BA_AMF_compnum,BA_AMFEMF_compnum,
                  labels = c("", ""), 
                  label_size = 14,
                  label_fontfamily = "sans",
                  ncol = 4)

Comp_num_PLOTCOMBINED<-cowplot::plot_grid(Bamodels_CompNum, Bimodels_CompNum, 
                                          nrow = 2)
Comp_num_Legend<-cowplot::plot_grid("", CompPlotLegend,"", 
                                          ncol = 1)
```

    ## Warning in as_grob.default(plot): Cannot convert object of class character into
    ## a grob.

    ## Warning in as_grob.default(plot): Cannot convert object of class character into
    ## a grob.

``` r
Comp_num_PLOTCOMBINEDwL<-cowplot::plot_grid(Comp_num_PLOTCOMBINED,Comp_num_Legend,
               rel_widths  = c(2,0.5),
                                          nrow = 1)
```

\#\#Colonization

\#\#Nutrients

``` r
d15N_Comp<-ggplot()+
  geom_boxplot(data=arrange(MCTdatafull, Competitornum),
             aes(x=Mycotreatment, `d 15N (‰)`,
             fill=CompetitionTreatment,
             color=CompetitionTreatment))+
  facet_wrap(~FocalSpecies,
             scales = "free_y", 
             labeller = labeller(FocalSpecies=plantlabels))+
  labs(x="Mycorrhizal Treatment", 
       y=expression(delta~paste("" ^"15")~ "N (‰)"))+
  scale_x_discrete(labels = c('None','EMF','AMF', "AMF+EMF"))+
  scale_fill_manual(values=CompTreatColors)+
  scale_color_manual(values=CompTreatColors2)+
  theme(
        text = element_text(),
        panel.background = element_rect(fill="transparent", colour = NA),
        plot.background = element_rect(fill="transparent",colour = NA),
        panel.border = element_rect(fill="transparent", colour = "black"),
        axis.title.y = element_text(angle=90, size = 12, margin = margin(r=3, unit = "mm")),
        axis.title.x = element_text(size=12, margin = margin(t=3, unit = "mm")),
        axis.text.x = element_text(size=8, margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")),
        axis.text.y = element_text(size=10, margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
        strip.text = element_text(face="italic"),
        axis.line = element_blank(),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.y = element_line(colour = "black"),
        axis.ticks.length=unit(-1.4, "mm"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")


PerN_std_Comp<-ggplot(data=MCTdatafull,
             aes(x=Mycotreatment, PerN_Std,
             fill=CompetitionTreatment, 
             color=CompetitionTreatment))+
  facet_wrap(~FocalSpecies, 
             labeller = labeller(FocalSpecies=plantlabels), 
                                 scales = "free_y")+  
  geom_boxplot()+
  labs(x="Mycorrhizal Treatment", 
       y="Leaf N (per plant)")+
  scale_x_discrete(labels = c('None','EMF','AMF', "AMF+EMF"))+
  scale_fill_manual(values=CompTreatColors)+
  scale_color_manual(values=CompTreatColors2)+
  theme(
        text = element_text(),
        panel.background = element_rect(fill="transparent", colour = NA),
        plot.background = element_rect(fill="transparent",colour = NA),
        panel.border = element_rect(fill="transparent", colour = "black"),
        axis.title.y = element_text(angle=90, size = 12, margin = margin(r=3, unit = "mm")),
        axis.title.x = element_text(size=12, margin = margin(t=3, unit = "mm")),
        axis.text.x = element_text(size=8, margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")),
        axis.text.y = element_text(size=10, margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
        strip.text.x = element_text(face = "italic"),
        axis.line = element_blank(),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.y = element_line(colour = "black"),
        axis.ticks.length=unit(-1.4, "mm"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")


d13C_comp<-ggplot(data=MCTdatafull,
             aes(x=Mycotreatment, `d 13C (‰)`,
             fill=CompetitionTreatment, 
             color=CompetitionTreatment))+
  geom_boxplot()+
  facet_wrap(~FocalSpecies, labeller = labeller(FocalSpecies=plantlabels))+
  labs(x="Mycorrhizal Treatment", 
       y=expression(delta~paste("" ^"13")~ "C (‰)"))+
  scale_x_discrete(labels = 
                     c('None','EMF','AMF', "AMF+EMF"))+
  scale_fill_manual(values=CompTreatColors)+
  scale_color_manual(values=CompTreatColors2)+
  theme(
        text = element_text(),
        panel.background = element_rect(fill="transparent", colour = NA),
        plot.background = element_rect(fill="transparent",colour = NA),
        panel.border = element_rect(fill="transparent", colour = "black"),
        axis.title.y = element_text(angle=90, size = 12, margin = margin(r=3, unit = "mm")),
        axis.title.x = element_text(size=12, margin = margin(t=3, unit = "mm")),
        axis.text.x = element_text(size=8, margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")),
        axis.text.y = element_text(size=10, margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
        axis.line = element_blank(),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.y = element_line(colour = "black"),
        axis.ticks.length=unit(-1.4, "mm"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(face = "italic"),
        legend.position = "none")

Isolegend<-ggplot(data=MCTdatafull,
             aes(x=Mycotreatment, `d 13C (‰)`,
             fill=CompetitionTreatment, 
             color=CompetitionTreatment))+
  geom_boxplot()+
  facet_wrap(~FocalSpecies, labeller = labeller(FocalSpecies=plantlabels))+
  labs(x="Mycorrhizal Treatment", 
       y=expression(delta~paste("" ^"13")~ "C (‰)"))+
  scale_x_discrete(labels = 
                     c('None','EMF','AMF', "AMF+EMF"))+
  scale_fill_manual(values=CompTreatColors)+
  scale_color_manual(values=CompTreatColors2)+
  theme(
        text = element_text(),
        panel.background = element_rect(fill="transparent", colour = NA),
        plot.background = element_rect(fill="transparent",colour = NA),
        panel.border = element_rect(fill="transparent", colour = "black"),
        axis.title.y = element_text(angle=90, size = 12, margin = margin(r=3, unit = "mm")),
        axis.title.x = element_text(size=12, margin = margin(t=3, unit = "mm")),
        axis.text.x = element_text(size=8, margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")),
        axis.text.y = element_text(size=10, margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
        axis.line = element_blank(),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.y = element_line(colour = "black"),
        axis.ticks.length=unit(-1.4, "mm"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(face = "italic"),
        legend.position = "top")


IsoPlotLegend<-get_legend(Isolegend)
```

    ## Warning: Removed 243 rows containing non-finite values (stat_boxplot).

\#H3 MCT Competition \#\#Alpha table

``` r
#alphatable has the alpha values from models
alphatableCompNum$FitnessRatio<-sqrt((alphatableCompNum$aAA*alphatableCompNum$aAB)/
                           (alphatableCompNum$aBB*alphatableCompNum$aBA))


alphatableCompNum$NicheDifference=1-(sqrt((alphatableCompNum$aBA*alphatableCompNum$aAB)/
                            (alphatableCompNum$aAA*alphatableCompNum$aBB)))  
```

\#\#Chesson Plot

``` r
##Ba is Plant A, Bi is Plant B

fun.1=function(x) 1-x
fun.2=function(x)-(1/(x-1))

Mycocolors3 = c(Sterile="#FF9D00",EMF="#2C4B69", AMF="#812416", AMFEMF="#869BCB")

PlantComp<-ggplot() +
  geom_point(data=alphatableCompNum, 
             aes(x=NicheDifference, y=FitnessRatio,
                 color=Mycotreatment))+
  labs(x =expression(paste("Niche difference ", "(1- ",rho,")")), 
       y = expression(paste("Fitness ratio ", "(", frac(f^B, f^A), ")")))+
  scale_color_manual(values=Mycocolors3, 
      breaks=c('Sterile', 'EMF', 'AMF', 'AMFEMF'), 
      labels=c('None', 'EMF', 'AMF', 'AMF+EMF'), name="Mycorrhizal Treatment")+
  xlim(-0.75,0.75)+
  ylim(-1,4)+
  annotate(geom="text", x=0, y=2.5, 
           label=expression(paste(italic("Pinus")," invades")), size=3)+
  annotate(geom="text",x=0, y=-0.8,
          label=expression(paste(italic("Baccharis")," invades")), size=3)+
  annotate(geom="text", x=-0.55, y=1.3, label="Priority effect", size=3)+
  annotate(geom="text", x=0.55, y=1.3, label="Coexistence", size=3)+
  stat_function(fun=fun.1, colour="black", size=0.3)+
  stat_function(fun=fun.2, color="black", size=0.3, linetype="dotted")+
  geom_area(stat = "function", fun = fun.1, xlim=c(-0.5,0), fill = "gray")+
  geom_area(stat = "function", fun = fun.2, xlim=c(0, 0.25), fill = "gray")+
  geom_vline(xintercept = 0, linetype="dashed", size=0.3)+
  geom_hline(yintercept = 1, linetype="dashed", size=0.3)+
  theme( text = element_text(family = "Helvetica"),
            plot.title = element_text(margin = margin(b=5, r=5, l=5, unit = "mm"),
            size = rel(0.8),
            hjust = 0.5),
            panel.background = element_rect(fill="transparent", colour = NA),
            plot.background = element_rect(fill="transparent",colour = NA),
            panel.border = element_rect(fill="transparent", colour = "black"),
            axis.title = element_text(size = rel(1)),
            axis.title.y = element_text(angle=90, size = rel(1), margin = margin(r=3, unit = "mm")),
            axis.title.x = element_text(size=rel(1), margin = margin(t=3, unit = "mm")),
            axis.text.x = element_text(size=12, margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")),
            axis.text.y = element_text(size=12, margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
            axis.line = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(fill="transparent", colour = NA),
            legend.key.size= unit(4, "mm"),
            legend.text = element_text(size =9),
            legend.spacing = unit(0, "mm"),
            legend.background = element_rect(fill ="transparent", 
                                             colour = NA),
                                             strip.text = element_text(face="bold"))
```

\#Figure formatting

``` r
Figure1a<-cowplot::plot_grid(
  Mycobaseline,
  labels = "(a)", ncol = 1)
Figure1bc<-cowplot::plot_grid(
  PerN_std, d15N,
  labels = c("(b)", "(c)", ncol = 2))
```

    ## Warning: Removed 48 rows containing non-finite values (stat_boxplot).
    
    ## Warning: Removed 48 rows containing non-finite values (stat_boxplot).

``` r
Figure1abc<-cowplot::plot_grid(
  Figure1a, Figure1bc, rel_heights = c(1.8, 2.1), ncol=1)
Figure1abc
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
Figure2<-cowplot::plot_grid(Comp_num_PLOTCOMBINEDwL)
Figure2
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-22-2.png)<!-- -->

``` r
Figure3<-cowplot::plot_grid(
  PlantComp)
```

    ## Warning in is.na(x): is.na() applied to non-(list or vector) of type
    ## 'expression'

    ## Warning in is.na(x): is.na() applied to non-(list or vector) of type
    ## 'expression'

``` r
Figure3
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-22-3.png)<!-- -->

``` r
Figure4legend<-cowplot::plot_grid(IsoPlotLegend)
Figure4ab<-cowplot::plot_grid(
  PerN_std_Comp, d15N_Comp,
  labels = c("(a)", "(b)", 
             ncol = 2))
```

    ## Warning: Removed 243 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 243 rows containing non-finite values (stat_boxplot).

``` r
Figure4c<-cowplot::plot_grid(d13C_comp,
   labels = "(c)", ncol=2)
```

    ## Warning: Removed 243 rows containing non-finite values (stat_boxplot).

``` r
Figure4abcl<-cowplot::plot_grid(
  Figure4legend, Figure4ab,Figure4c, rel_heights = c(0.3, 2,2), nrow=3)

Figure4abcl
```

![](MCT_AMF.EMF.CompAnalyses_files/figure-gfm/unnamed-chunk-22-4.png)<!-- -->
