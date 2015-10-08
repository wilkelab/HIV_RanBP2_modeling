
library(plyr) # for ldply(), mapvalues()
library(dplyr) # for select(), arrange(), group_by(), filter() 
library(ggplot2) # ggplot()


fig3D <- list("human_hiv_docking.fasc",
             "chimp_hiv_docking.fasc",
              "gorilla_hiv_docking.fasc",
              "human_SIVmac_docking.fasc",
             "chimp_SIVmac_docking.fasc",
              "gorilla_hiv_docking.fasc")

fig3D_hiv.ran <- c("human_hiv", "chimp_hiv", "gorilla_hiv", "human_SIVmac", "chimp_SIVmac", "gorilla_SIVmac")
names(fig3D) <- fig3D_hiv.ran


setwd("..") #Change the working directory
           
fig3D.data$Ran <- factor(fig3D.data$Ran ) # turn Ran column into factor, so we can group by receptors
#fig3D_hiv.data$Ran <- relevel(  fig3D_hiv.data$Ran, "human_hiv") # pull human in front, otherwise order is alphabetical

fig3D_siv.mac.data$Ran <- factor(fig3D_siv.mac.data$Ran ) # turn Ran column into factor, so we can group by receptors
#fig3D_siv.mac.data$Ran <- relevel(fig3D_siv.mac.data$Ran, "human_SIVmac") # pull human in front, otherwise order is alphabetical


Read all files into a table for each dataset needed for plotting

setwd("./scorefiles/")
# read all files into a table for each dataset needed for plotting
fig3D.data <- ldply(fig3D_hiv, read.table, .id='Ran', header=T, stringsAsFactors=T)


#For each complex in each dataset, get the top ten scoring models for each complex and put in a dataframe. For example for the figure #3D for HIV:


# clean-up data, pick top hits, store in data frame HIV "fig3D_hiv.top.hits"
fig3D_hiv.top.hits <-
    select(fig3D.data, Ran, I_sc, total_score, rms) %>%     # select columns of interest
    arrange( Ran, I_sc ) %>%     # order them by Ran and interface score
    group_by( Ran ) %>%     # group by Ran
    filter( row_number() <= 10 )     # keep the first 10 entries in each group




fig4B_rcm <- list("rcm_SIVrcm_docking.fasc",
             "rcm_hiv_docking.fasc",
             "rcm_SIVcpz_docking.fasc",
             "rcm_SIVgor_docking.fasc")

fig4B_rcm.ran <- c("rcm_SIVrcm", "rcm_hiv", "rcm_SIVcpz", "rcm_SIVgor")
names(fig4B_rcm) <- fig4B_rcm.ran

fig4B_chimp <- list("chimp_SIVrcm_docking.fasc",
             "chimp_hiv_docking.fasc",
             "chimp_SIVcpz_docking.fasc",
             "chimp_SIVgor_docking.fasc")

fig4B_chimp.ran <- c("chimp_SIVrcm", "chimp_hiv", "chimp_SIVcpz", "chimp_SIVgor")
names(fig4B_chimp) <- fig4B_chimp.ran

fig4B_gor <- list("gorilla_SIVrcm_docking.fasc",
             "gorilla_hiv_docking.fasc",
             "gorilla_SIVcpz_docking.fasc",
             "gorilla_SIVgor_docking.fasc")

fig4B_gor.ran <- c("gorilla_SIVrcm", "gorilla_hiv", "gorilla_SIVcpz", "gorilla_SIVgor")
names(fig4B_gor) <- fig4B_gor.ran


fig5A_siv.cpz <- list("human_SIVcpz_docking.fasc", "chimp_SIVcpz_docking.fasc")
fig5A_siv.cpz.ran <- c("human_SIVcpz", "chimp_SIVcpz")
names(fig5A_siv.cpz) <- fig5A_siv.cpz.ran

fig5B_siv.gor <- list("human_SIVgor_docking.fasc", "gorilla_SIVgor_docking.fasc")
fig5B_siv.gor.ran <- c("human_SIVgor", "gorilla_SIVgor")
names(fig5B_siv.gor) <- fig5B_siv.gor.ran

fig6D_hiv <- list("ancestor_hiv_docking.fasc",
  		"ancestor_G75R_hiv_docking.fasc",
			"ancestor_K82R_hiv_docking.fasc",
			"ancestor_Q103E_hiv_docking.fasc",
			"ancestor_G75R_K82R_hiv_docking.fasc",
			"ancestor_G75R_Q103E_hiv_docking.fasc",
			"ancestor_K82R_Q103E_hiv_docking.fasc",
			"ancestor_G75R_K82R_Q103E_hiv_docking.fasc")
			
fig6D_hiv.ran <- c("ancestor_hiv", "ancestor_G75R_hiv", "ancestor_K82R_hiv", "ancestor_Q103E_hiv", "ancestor_G75R_K82R_hiv", "ancestor_G75R_Q103E_hiv", "ancestor_K82R_Q103E_hiv", "ancestor_G75R_K82R_Q103E_hiv")
names(fig6D_hiv) <-fig6D_hiv.ran

fig6D_siv.gor <- list("ancestor_SIVgor_docking.fasc",
			"ancestor_G75R_SIVgor_docking.fasc",
			"ancestor_K82R_SIVgor_docking.fasc",
			"ancestor_Q103E_SIVgor_docking.fasc",
			"ancestor_G75R_SIVgor_docking.fasc",
			"ancestor_G75R_Q103E_SIVgor_docking.fasc",
			"ancestor_K82R_Q103E_SIVgor_docking.fasc",
			"ancestor_G75R_K82R_Q103E_SIVgor_docking.fasc")
			
fig6D_siv.gor.ran <- c("ancestor_SIVgor", "ancestor_G75R_SIVgor", "ancestor_K82R_SIVgor", "ancestor_Q103E_SIVgor", "ancestor_G75R_K82R_SIVgor", "ancestor_G75R_Q103E_SIVgor", "ancestor_K82R_Q103E_SIVgor", "ancestor_G75R_K82R_Q103E_SIVgor")
names(fig6D_siv.gor) <-fig6D_siv.gor.ran

fig6D_siv.cpz<- list("ancestor_SIVcpz_docking.fasc",
			"ancestor_G75R_SIVcpz_docking.fasc",
			"ancestor_K82R_SIVcpz_docking.fasc",
			"ancestor_Q103E_SIVcpz_docking.fasc",
			"ancestor_G75R_K82R_SIVcpz_docking.fasc",
			"ancestor_G75R_Q103E_SIVcpz_docking.fasc",
			"ancestor_K82R_Q103E_SIVcpz_docking.fasc",
			"ancestor_G75R_K82R_Q103E_SIVcpz_docking.fasc")
			
fig6D_siv.cpz.ran <- c("ancestor_SIVcpz", "ancestor_G75R_SIVcpz", "ancestor_K82R_SIVcpz", "ancestor_Q103E_SIVcpz", "ancestor_G75R_K82R_SIVcpz", "ancestor_G75R_Q103E_SIVcpz", "ancestor_K82R_Q103E_SIVcpz", "ancestor_G75R_K82R_Q103E_SIVcpz" )
names(fig6D_siv.cpz) <-fig6D_siv.cpz.ran

logfig_human <-list("human_hiv_docking.fasc",
                      "human_Q103E_hiv_docking.fasc", 
                      "human_G75R_K82R_hiv_docking.fasc",
                      "human_G75R_K82R_Q103E_hiv_docking.fasc")

logfig_human.ran <- c("human_hiv", "human_Q103E_hiv", "human_G75R_K82R_hiv", "human_G75R_K82R_Q103E_hiv")
names(logfig_human) <- logfig_human.ran 

logfig_gorilla <-list("gorilla_hiv_docking.fasc",
                      "gorilla_E103Q_hiv_docking.fasc", 
                      "gorilla_R75G_R82K_hiv_docking.fasc",
                      "gorilla_R75G_R82K_E103Q_hiv_docking.fasc")

logfig_gorilla.ran <- c("gorilla_hiv", "gorilla_E103Q_hiv", "gorilla_R75G_R82K_hiv", "gorilla_R75G_R82K_E103Q_hiv")
names(logfig_gorilla) <- logfig_gorilla.ran 

```


fig3D_siv.mac.data <- ldply(fig3D_siv.mac, read.table, .id='Ran', header=T, stringsAsFactors=T)
fig4B_rcm.data <- ldply(fig4B_rcm, read.table, .id='Ran', header=T, stringsAsFactors=T)
fig4B_chimp.data <- ldply(fig4B_chimp, read.table, .id='Ran', header=T, stringsAsFactors=T)
fig4B_gor.data <- ldply(fig4B_gor, read.table, .id='Ran', header=T, stringsAsFactors=T)
fig5A_siv.cpz.data <-ldply(fig5A_siv.cpz, read.table, .id='Ran', header=T, stringsAsFactors=T)
fig5B_siv.gor.data <-ldply(fig5B_siv.gor, read.table, .id='Ran', header=T, stringsAsFactors=T)
fig6D_hiv.data <-ldply(fig6D_hiv, read.table, .id='Ran', header=T, stringsAsFactors=T)
fig6D_siv.gor.data <-ldply(fig6D_siv.gor, read.table, .id='Ran', header=T, stringsAsFactors=T)
fig6D_siv.cpz.data <-ldply(fig6D_siv.cpz, read.table, .id='Ran', header=T, stringsAsFactors=T)
logfig_human.data <-ldply(logfig_human, read.table, .id='Ran', header=T, stringsAsFactors=T)
logfig_gorilla.data <-ldply(logfig_gorilla, read.table, .id='Ran', header=T, stringsAsFactors=T)





# clean-up data, pick top hits, store in data frame "top.hits"
fig4B_rcm.top.hits <-
    # select columns of interest
    select(fig4B_rcm.data, Ran, I_sc, total_score, rms) %>%
    # order them by Ran and interface score
    arrange( Ran, I_sc ) %>%
    # group by Ran
    group_by( Ran ) %>%
    # keep the first 10 entries in each group
    filter( row_number() <= 10 )

# clean-up data, pick top hits, store in data frame "top.hits"
fig4B_chimp.top.hits <-
    # select columns of interest
    select(fig4B_chimp.data, Ran, I_sc, total_score, rms) %>%
    # order them by Ran and interface score
    arrange( Ran, I_sc ) %>%
    # group by Ran
    group_by( Ran ) %>%
    # keep the first 10 entries in each group
    filter( row_number() <= 10 )

# clean-up data, pick top hits, store in data frame "top.hits"
fig4B_gor.top.hits <-
    # select columns of interest
    select(fig4B_gor.data, Ran, I_sc, total_score, rms) %>%
    # order them by Ran and interface score
    arrange( Ran, I_sc ) %>%
    # group by Ran
    group_by( Ran ) %>%
    # keep the first 10 entries in each group
    filter( row_number() <= 10 )


# clean-up data, pick top hits, store in data frame "top.hits"
fig4A_gor.top.hits <-
    # select columns of interest
    select(fig4B_gor.data, Ran, I_sc, total_score, rms) %>%
    # order them by Ran and interface score
    arrange( Ran, I_sc ) %>%
    # group by Ran
    group_by( Ran ) %>%
    # keep the first 10 entries in each group
    filter( row_number() <= 10 )


# clean-up data, pick top hits, store in data frame "top.hits"
fig5A_siv.cpz.top.hits <-
    # select columns of interest
    select(fig5A_siv.cpz.data, Ran, I_sc, total_score, rms) %>%
    # order them by Ran and interface score
    arrange( Ran, I_sc ) %>%
    # group by Ran
    group_by( Ran ) %>%
    # keep the first 10 entries in each group
    filter( row_number() <= 10 )

# clean-up data, pick top hits, store in data frame "top.hits"
fig5B_siv.gor.top.hits <-
    # select columns of interest
    select(fig5B_siv.gor.data, Ran, I_sc, total_score, rms) %>%
    # order them by Ran and interface score
    arrange( Ran, I_sc ) %>%
    # group by Ran
    group_by( Ran ) %>%
    # keep the first 10 entries in each group
    filter( row_number() <= 10 )

# clean-up data, pick top hits, store in data frame "top.hits"
fig6D_hiv.top.hits <-
    # select columns of interest
    select(fig6D_hiv.data, Ran, I_sc, total_score, rms) %>%
    # order them by Ran and interface score
    arrange( Ran, I_sc ) %>%
    # group by Ran
    group_by( Ran ) %>%
    # keep the first 10 entries in each group
    filter( row_number() <= 10 )

# clean-up data, pick top hits, store in data frame "top.hits"
fig6D_siv.gor.top.hits <-
    # select columns of interest
    select(fig6D_siv.gor.data, Ran, I_sc, total_score, rms) %>%
    # order them by Ran and interface score
    arrange( Ran, I_sc ) %>%
    # group by Ran
    group_by( Ran ) %>%
    # keep the first 10 entries in each group
    filter( row_number() <= 10 )

# clean-up data, pick top hits, store in data frame "top.hits"
fig6D_siv.cpz.top.hits <-
    # select columns of interest
    select(fig6D_siv.cpz.data, Ran, I_sc, total_score, rms) %>%
    # order them by Ran and interface score
    arrange( Ran, I_sc ) %>%
    # group by Ran
    group_by( Ran ) %>%
    # keep the first 10 entries in each group
    filter( row_number() <= 10 )


# clean-up data, pick top hits, store in data frame "top.hits"
logfig_human.top.hits <-
    # select columns of interest
    select(logfig_human.data, Ran, I_sc, total_score, rms) %>%
    # order them by Ran and interface score
    arrange( Ran, I_sc ) %>%
    # group by Ran
    group_by( Ran ) %>%
    # keep the first 10 entries in each group
    filter( row_number() <= 10 )

# clean-up data, pick top hits, store in data frame "top.hits"
logfig_gorilla.top.hits <-
    # select columns of interest
    select(logfig_gorilla.data, Ran, I_sc, total_score, rms) %>%
    # order them by Ran and interface score
    arrange( Ran, I_sc ) %>%
    # group by Ran
    group_by( Ran ) %>%
    # keep the first 10 entries in each group
    filter( row_number() <= 10 )
