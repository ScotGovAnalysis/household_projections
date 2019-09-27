# dataset ----------------------------------------------------------------------


load("projection2016.RData")
#load("dummies.RData")
list_of_places = as.list(as.character(unique(data1$areacode)))
list_of_variants = as.list(as.character(unique(data1$variant)))
list_of_size =  as.list(as.character(unique(data2$householdsize)))
list_of_age_group = as.list(as.character(unique(data3$agegroup)))

  ### Common features ----------
g1<-"#536f1b"
g2<-"#bac5a3"
# color palette --- colorblind friendly with grey + the greens used for households branch
cbPalette <- rep(c("#536f1b","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#999999","#CC79A7"), 3)

shinyServer(function(input, output){
#########
# Tab 1 #
#########

 output$my_plot1<- renderPlot({
   
    # --------------------------------------------------------------------------
    # preparing dataset
    # --------------------------------------------------------------------------
    # Aggregating --------------------------------------------------------------
    total_dat <- group_by(data1, variant, areacode, year) %>%
      summarise(Base=sum(Base),
                Projection=sum(households)) %>%
      mutate(Perc_change=(Projection-Base)/Base)
    
    # Allocating variable to plot ----------------------------------------------
    ifelse(input$Type1=="Absolute change",
           total_dat$change<-total_dat$Projection,
           total_dat$change<-total_dat$Perc_change)
    
    # Filtering observations to plot -------------------------------------------
    plot_data1<-filter(total_dat, areacode %in% input$Area1 &
                                  variant %in% input$Variants1 &
                                  year <= input$end_year1)  
    # .. to print table
    table_data1<-plot_data1[,c("variant", "areacode", "year", "Projection", "Perc_change")]
    table_data1$Perc_change<-format(round(table_data1$Perc_change, digits=1), nsmall = 1)
    plot_annot<- filter(plot_data1, areacode %in% input$Area1 & year == input$end_year1 & variant %in% input$Variants1)
  
    #plotting
  ggplot(data = plot_data1, mapping=aes(x=year, 
                                          y=change, group=interaction(areacode, variant))) +
    geom_line( data=plot_data1, aes(colour=areacode, linetype=interaction(areacode,factor(variant))), size=1.2) +
    #linetype=interaction(areacode,factor(variant))
    # geom_label_repel(plot_annot, aes(x=input$end_year1, y=change, label = variant, fill = factor(variant)), color = 'white', size = 3.5)+
    # scale_color_manual(values=ifelse(plot_annot$variant=="Principal", g1,g2)) +
    scale_linetype_manual(values=ifelse(plot_annot$variant=="Principal", "solid", ifelse(plot_annot$variant=="High migration","dashed", "dotted")))+
    scale_color_manual(values=cbPalette) +
    #scale_alpha_manual(values=ifelse(plot_annot$variant=="Principal", 1,0.4)) +
    scale_x_continuous(name="Year", limits = range(plot_data1$year))+ #+c(-3, diff(range(plot_data1$year)/1.5))) +
    scale_y_continuous(name=NULL, labels = ifelse(input$Type1=="Absolute change", function(change) format(paste0(change/1000,"k"))  , scales::percent), 
                         limits = range(subset(total_dat$change, 
                                               total_dat$areacode %in% input$Area1 &
                                               total_dat$year <= input$end_year1))+c(0, diff(range(plot_data1$change)/10))) +
    labs(title=paste("Scottish areas"),subtitle=paste("Households projections", 
                      "\n", input$Type1, ", ",
                      min(data1$year), " - ", input$end_year1, sep=""), color=c("Area"))+    
    guides(linetype=FALSE)

 })

  #########
  # Tab 2 #
  #########
  output$my_plot2a<- renderPlot({
    
    # --------------------------------------------------------------------------
    # preparing dataset
    # --------------------------------------------------------------------------
    #
    # Keep this order, othewise it might not work properly. The dataset needs to
    # be aggregated (summarised) before Perc_change can be calculated.
    
    # Aggregating --------------------------------------------------------------
    total_dat2 <- group_by(data2, areacode, householdsize, year) %>%
    summarise(Base=sum(Base),
                Projection=sum(households)) %>%
      mutate(Perc_change=(Projection-Base)/Base*100)
    
    # # Allocating variable to plot ----------------------------------------------
    # ifelse(input$Type2=="Absolute change",
    #        total_dat2$change<-total_dat2$Projection,
    #        total_dat2$change<-total_dat2$Perc_change)
    
    # Filtering observations to plot -------------------------------------------
    plot_data2<-filter(total_dat2, areacode %in% input$Area2 &
                                  year %in% c(min(year), input$end_year2))
    # # .. to print table
    # table_data2<-plot_data2[,c("Area",  "Household_size", "Year", "Projection", "Perc_change")]
    # #names(table_data1)[5]<-"Percentage change"
    # table_data2$Perc_change<-format(round(table_data2$Perc_change, digits=1), nsmall = 1)
    
    # --------------------------------------------------------------------------
    # plotting dataset
    # --------------------------------------------------------------------------
    # plot_annot<- filter(plot_data2, areacode %in% input$Area2 & year == min(year)) 
    # # &  householdtype %in% input$Size)
    # plot_annot2<- filter(plot_data2, areacode %in% input$Area2 & year == input$end_year2)
    # 
        ggplot(data = plot_data2)+
     geom_bar(mapping=aes(x=householdsize,
                       y=Projection,
                       group=interaction(year, areacode),
                       fill=factor(areacode), alpha=factor(year)),
                       stat="identity", position = "dodge") +
      
      scale_fill_manual(name="Area", values=cbPalette) +
      
      scale_alpha_discrete(name="Year", range=c(0.5, 1)) +  
    
      scale_y_continuous(name=NULL, expand=c(0.1, .01), labels=function(Projection) format(paste0(Projection/1000,"k")),
                         limits = c(0, max(plot_data2$Projection)*1.25))+
      
      scale_x_discrete(name="Household size", labels=str_wrap(levels(data2$householdsize), width=2))+
      # geom_text_repel(data = subset(plot_data1, Year == max(Year)),
      #                 aes(label = paste(Country, Projection, sep=" - ")),
      #                 size = 4,
      #                 nudge_x = 3,
      #                 #nudge_y = 0,
      #                 #point.padding = 0.3,
      #                 #box.padding = 0.9,
      #                 show.legend =FALSE,
      #                 segment.color = NA) +
      
      labs(title="Scottish areas",
           subtitle=paste("Households projections", 
                          input$Type2,"\nAbsolute change, ", 
                          min(data2$year), " - ", input$end_year2, sep=""))

    })
  
  output$my_plot2b<- renderPlot({
    
    # --------------------------------------------------------------------------
    # preparing dataset
    # --------------------------------------------------------------------------
    #
    # Keep this order, othewise it might not work properly. The dataset needs to
    # be aggregated (summarised) before Perc_change can be calculated.
    
    # Aggregating --------------------------------------------------------------
    total_dat2 <- group_by(data2, areacode, householdsize, year) %>%
      summarise(Base=sum(Base),
                Projection=sum(households)) %>%
      mutate(Perc_change=(Projection-Base)/Base)
    
    # # Allocating variable to plot ----------------------------------------------
    # ifelse(input$size2=="Absolute change",
    #        total_dat2$change<-total_dat2$Projection,
    #        total_dat2$change<-total_dat2$Perc_change)
    
    # Filtering observations to plot -------------------------------------------
    plot_data2<-filter(total_dat2, areacode %in% input$Area2 &
                         year == input$end_year2)
    # # .. to print table
    # table_data2<-plot_data2[,c("Area",  "Household_size", "Year", "Projection", "Perc_change")]
    # #names(table_data1)[5]<-"Percentage change"
    # table_data2$Perc_change<-format(round(table_data2$Perc_change, digits=1), nsmall = 1)
    
    # --------------------------------------------------------------------------
    # plotting dataset
    # --------------------------------------------------------------------------
    # plot_annot<- filter(plot_data2, areacode %in% input$Area2 & year == min(year)) 
    # # &  householdsize %in% input$Size)
    # plot_annot2<- filter(plot_data2, areacode %in% input$Area2 & year == input$end_year2)
    # 
  #  plot_data2$choose = ifelse(plot_data2$Perc_change > 0,1,0)
    plot_data2$alpha <- ifelse(plot_data2$Perc_change > 0, 2, 1)
   # size <- ifelse(plot_data2$Perc_change > 0, 10, 25) , size=factor(choose)
    ggplot(data = plot_data2)+
    geom_bar(mapping=aes(x=householdsize,
                           y=Perc_change,
                           group=interaction(alpha, areacode),
                           fill=areacode, alpha=factor(alpha)),
               stat="identity",  position="dodge", width=0.5) +
      #scale_alpha_manual(values=ifelse(plot_data2$Perc_change >0, 1, 0.1)) +  
      scale_fill_manual(name="Area", values=cbPalette) +
      #guides(fill = FALSE) +
      scale_alpha_manual(values=c(0.6, 1)) +
      scale_y_continuous(name=NULL, expand=c(0.1, .01), labels=scales::percent,
                         limits = c(min(plot_data2$Perc_change), max(plot_data2$Perc_change)*1.25))+
      scale_x_discrete(name="Household size", labels=str_wrap(levels(data2$householdsize), width=2))+
      geom_hline(yintercept=0, size=1)+
      labs(title="Scottish areas",
           subtitle=paste("Households projections", input$Type2, "\nPercentage change, " ,
                          min(data2$year), " - ", input$end_year2, sep=""))+
      guides(alpha=FALSE)
    
  })
  
  #   
# 
#  
# 
# 
#   
#   #########
#   # Tab 3 #
#   #########
  
  
  
  
 output$my_plot3<- renderPlot({
   # Aggregating --------------------------------------------------------------
   total_dat3 <- group_by(data3, areacode, agegroup, year)
   
   # Filtering observations to plot -------------------------------------------
   plot_data3<-filter(total_dat3, areacode %in% input$Area3 &
                      year %in% c(2016,input$end_year3))
   #lastyear<-filter(total_dat3, areacode %in% input$Area3 &
   #                 year== input$end_year3)  
   # --------------------------------------------------------------------------
   # plotting dataset
   # --------------------------------------------------------------------------
  ggplot(total_dat3, mapping=aes(agegroup, households)) +
  geom_line(plot_data3, mapping = aes(x=factor(agegroup), y=households, group=interaction(areacode,year), colour= areacode, size=factor(year))) +
  #  geom_line(data=lastyear, aes(x=agegroup, y=households, group= interaction(areacode, year), alpha=areacode), colour=g1, size=1.5) +
     
     scale_color_manual(name="Area", values=cbPalette) +
     scale_size_manual(name="Year", values=ifelse(plot_data3$year==2016, 1, 2)) + 
     scale_y_continuous(name=NULL, labels = function(households) format(paste0(households/1000,"k")),
                        limits = c(0, max(plot_data3$households)*1.25))+
     scale_x_discrete(name="Age group of head of household", breaks=c("16-19","25-29","35-39", "45-49", "55-59", "65-69", "75-79", "85-89"),
                      labels=c("16-19","25-29","35-39", "45-49", "55-59", "65-69", "75-79", "85-89"))+
     # , labels=(str_wrap(levels(plot_data3$agegroup) ,,  width=1
    #  theme(legend.position="none") +
     labs(title="Scottish areas",
          subtitle=paste("Projected number of households by age of head of household", 
                         "\n", min(data3$year), " - ", input$end_year3, sep=""), colour="Area", linetype="Year",
          caption=paste0("The 'head' of the household is normally the first person entered on the census form.","\nData shown for principal projection only.")) +
     guides(colour = guide_legend(override.aes = list(size=1.5))) +
     theme(plot.caption=element_text(size=10, hjust=0, margin(margin(t=15))))
    })
})
