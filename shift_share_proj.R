# File:    shift_share_proj.r
# Author:  Joe Yuke
# Purpose: shift share analysis, SB County#
# Outputs: Shift_share.xlsx, Local_graph_ind.jpg, US_bar.jpg, State_bar.jpg, Local_bar.jpg


# 6 input vars:
#                                     "b" for base year
# local jobs in industry i @ t-1:     local_i_b
# local jubs in industry i @ t:       local_i
# national jobs @ t-1:                US_b
# national jobs @ t:                  US
# national jobs in industry i @ t-1:  US_i_b
# national jobs in industry i @ t:    US_i


# SS = NS + IM + RS
# national share:   local_i_b*(US/US_b)
# Industry mix:     (local_i_b*US_i/US_i_b) - NS
# Regional shift:   local_i_b*(local_i/local_i_b - US_i/US_i_b)


# example calculation
{
  local_i_b <- 99593
  local_i   <- 99346
  US_b      <- 121044320
  US        <- 129877063
  US_i_b    <- 1687808
  US_i      <- 1688864
  
  NS        <- local_i_b*(US/US_b)
  IM        <- (local_i_b*US_i/US_i_b) - NS
  RS        <- local_i_b*(local_i/local_i_b - US_i/US_i_b)
  SS        <- NS + IM + RS
}


# ------------------------- #
#                           #
#       Preliminaries       #
#                           #
# ------------------------- #

# packages
library(xlsx)
library(tidyverse)
library(ggplot2)
library(scales)

# directories
outputloc <- "C:/Users/Joe Yuke/OneDrive - BAY AREA TECHNOLOGY MANAGEMENT INC/Documents/EFP/Shift_share_proj/Output"

# color template
EFPcolors <- c("deepskyblue4","brown3","darkolivegreen4", "cornflowerblue",
               "darkorange3","brown4","mediumpurple4","red", "darkslateblue")



# ---------------- #
#                  #
#       Main       #
#                  #
# ---------------- #

# manually set parameters
base_yr <- 2018
yr      <- 2019


# industries of interest: NAICS code
code_list <- c("11", "23", "31-33", "42", "44-45", "48-49", "51", "52", "53", "54", "55",
               "56", "62", "71", "72")
ind_names <- c("Agriculture", "Construction", "Manufacturing", "Wholesale Trade",
               "Retail Trade", "Transportation & Warehousing", "Information",
               "Finance & Insurance", "Real Estate", "Prof. & Technical Services",
               "Management", "Admin & Waste Services", "Health Care & \nSocial Assistance",
               "Arts, Entertainment, & Recreation", "Accommodation & \nFood Services")
num_i <- length(code_list)


# shift share function:
shiftshare <- function(li1,li2,ri1,ri2,r1,r2){
  
  # "l" for local; "r" for regional
  # 1 for time period 1; 2 for time period 2
  NS <- li1 * (r2/r1)
  IM <- (li1 * ri2/ri1) - NS
  RS <- li1 * (li2/li1 - ri2/ri1)
  return(c(NS,IM,RS))
  
}


# Data from: State and County Employment and Wages
# (Quarterly Census of Employment & Wages - QCEW)
# https://www.bls.gov/data/
# Note: Private Sector Jobs

# Totals for national/state:
US_data <- read.xlsx("US_tot.xlsx", sheetIndex=1, startRow=13)
US_data <- select(US_data, Year, Annual) %>% subset(Year == base_yr | Year == yr)
CA_data <- read.xlsx("CA_tot.xlsx", sheetIndex=1, startRow=13)
CA_data <- select(CA_data, Year, Annual) %>% subset(Year == base_yr | Year == yr)

# Picking out data points
US_b <- US_data[1,2]
US   <- US_data[2,2]
CA_b <- CA_data[1,2]
CA   <- CA_data[2,2]


# SB/US matrix
US_Shift_share <- matrix(data = 0,num_i,4)
colnames(US_Shift_share)[1:4] <- c("Industry(NAICS)","National_Share",
                                   "Industry_Mix","Regional_Share")
US_Shift_share[,1] <- code_list
rownames(US_Shift_share)[1:num_i] <- ind_names


# SB/CA matrix
CA_Shift_share <- matrix(data = 0,num_i,4)
colnames(CA_Shift_share)[1:4] <- c("Industry(NAICS)","CA_Share",
                                   "Industry_Mix","Regional_Share")
CA_Shift_share[,1] <- code_list
rownames(CA_Shift_share)[1:num_i] <- ind_names


# industry employment changes matrix
e_change <- matrix(data = 0,num_i,10)
colnames(e_change)[1:10] <- c("Industry(NAICS)","t-1:county","t:county",
                              "Pct_Chng:county","t-1:state","t:state","Pct_Chng:state",
                              "t-1:national","t:national","Pct_Chng:national")
e_change[,1] <- code_list
rownames(e_change)[1:num_i] <- ind_names

# shift-share calculations:
for (i in 1:num_i) {
  
  US_i_data <- read.xlsx(sprintf("National/US_%s.xlsx",code_list[i]), sheetIndex = 1, startRow = 13)
  US_i_data <- select(US_i_data, Year, Annual) %>%
    subset(Year == base_yr | Year == yr)
  CA_i_data <- read.xlsx(sprintf("State/CA_%s.xlsx",code_list[i]), sheetIndex = 1, startRow = 13)
  CA_i_data <- select(CA_i_data, Year, Annual) %>%
    subset(Year == base_yr | Year == yr)
  SB_i_data <- read.xlsx(sprintf("Local/SB_%s.xlsx",code_list[i]), sheetIndex = 1, startRow = 13)
  SB_i_data <- select(SB_i_data, Year, Annual) %>%
    subset(Year == base_yr | Year == yr)
  
  #Picking out data points:
  US_i_b    <- US_i_data[1,2]
  US_i      <- US_i_data[2,2]
  CA_i_b    <- CA_i_data[1,2]
  CA_i      <- CA_i_data[2,2]
  local_i_b <- SB_i_data[1,2]
  local_i   <- SB_i_data[2,2]
  
  #using shift-share function:
  US_Shift_share[i,2:4] <- shiftshare(local_i_b,local_i,US_i_b,US_i,US_b,US)
  CA_Shift_share[i,2:4] <- shiftshare(local_i_b,local_i,CA_i_b,CA_i,CA_b,CA)
  
  #percent change:
  US_chng <- (US_i - US_i_b)/US_i_b
  CA_chng <- (CA_i - CA_i_b)/CA_i_b
  local_chng <- (local_i - local_i_b)/local_i_b
  e_change[i,2:10] <- c(local_i_b,local_i,local_chng,CA_i_b,CA_i,CA_chng,
                        US_i_b,US_i,US_chng)
  
}


# Save Output as an excel file:
write.xlsx(US_Shift_share, file="Shift_share.xlsx", sheetName="US", append=F)
write.xlsx(CA_Shift_share, file="Shift_share.xlsx", sheetName="CA", append=T)
write.xlsx(e_change, file="Shift_share.xlsx", sheetName="Change", append=T)



#--------------#
#              #
#     Plots    #
#              #
#--------------#

Years       <- c((yr-10):yr)

# number of industries to graph
num_j       <- 3


# data frame for plots
graph_data <- as.data.frame(matrix(data = 0, length(Years), 4+3*num_j))
colnames(graph_data)[1] <- "Year"
colnames(graph_data)[(2+3*num_j):(4+3*num_j)] <- c("US_tot","State_tot","Local_tot")


# Identifying n largest local industries:

# manual entry
#graph_codes <- c("11", "62", "72")
#ind_names   <- c("Agriculture","Health Care & Social Assistance", "Accommodation & Food Services")

graph_codes <- rep(0,num_j)
ind_names   <- rep(0,num_j)

ind_size <- as.data.frame(e_change) %>% select(`Industry(NAICS)`,`t:county`)
ind_size$`t:county` <- as.integer(as.character(ind_size$`t:county`))
ind_size1 <- ind_size

for (j in 1:num_j) {
  
  index <- which.max(ind_size1$`t:county`)
  graph_codes[j] <- as.character(ind_size1$`Industry(NAICS)`[index])
  
  ind_size1 <- ind_size1[-index,]
  
}

graph_codes <- as.character(sort(as.integer(graph_codes)))

for (j in 1:num_j) {
  
  index <- which(ind_size$`Industry(NAICS)` == graph_codes[j])
  
  ind_names[j]   <- row.names(ind_size)[index]
  
  colnames(graph_data)[(3*j-1):(3*j+1)] <- 
    sprintf(c("US_%s","State_%s","Local_%s"), graph_codes[j])
  
}


# Filling in graph_data:
graph_data$Year <- Years
graph_data$Year <- as.Date(paste0(as.factor(graph_data$Year),"-01-01"))

for (j in 1:num_j) {
  
  US <- read.xlsx(sprintf("National/US_%s.xlsx",graph_codes[j]), sheetIndex = 1, startRow = 13) %>%
    select(Year, Annual)
  graph_data[,(3*j-1)] <- US$Annual
  
  State <- read.xlsx(sprintf("State/CA_%s.xlsx",graph_codes[j]), sheetIndex = 1, startRow = 13) %>%
    select(Year, Annual)
  graph_data[,(3*j)] <- State$Annual
  
  Local <- read.xlsx(sprintf("Local/SB_%s.xlsx",graph_codes[j]), sheetIndex = 1, startRow = 13) %>%
    select(Year, Annual)
  graph_data[,(3*j+1)] <- Local$Annual
  
}

US <- as.data.frame(read.xlsx("US_tot.xlsx", sheetIndex = 1, startRow = 13))
graph_data$US_tot <- US$Annual
State <- as.data.frame(read.xlsx("CA_tot.xlsx", sheetIndex = 1, startRow = 13))
graph_data$State_tot <- State$Annual
Local <- as.data.frame(read.xlsx("SB_tot.xlsx", sheetIndex = 1, startRow = 13))
graph_data$Local_tot <- Local$Annual


# bar chart manipulation:
graph_data_bar <- gather(graph_data[7:11,], type, employment, -Year)
graph_data_bar <- separate(graph_data_bar, type, c("location","industry"), sep = "_")


# defining theme:
efp_theme <- theme( text = element_text(family = "serif"),
                    legend.title = element_blank(),
                    legend.background = element_blank(),
                    legend.key = element_blank(),
                    panel.grid.major = element_blank() ,
                    panel.grid.minor = element_blank() ,
                    plot.title = element_text(face = "bold", size = 16),
                    axis.title.y = element_text(size = 14),
                    axis.title.x = element_blank(),
                    axis.line = element_line(color = 'black'),
                    plot.subtitle = element_text(face = "italic"),
                    plot.caption = element_text(hjust = -0.3, face= "italic", size = 8),
                    legend.position = 'bottom',
                    panel.background = element_rect(fill = 'white'))


# ----- County line graph ----- #

Local_graph_ind <- ggplot(graph_data) +
  geom_line(size = 1.2, aes(x = Year, y = graph_data[,4], color = "first")) +
  geom_line(size = 1.2, aes(x = Year, y = graph_data[,7], color = "second")) +
  geom_line(size = 1.2, aes(x = Year, y = graph_data[,10], color = "third")) +
  scale_x_date(labels = date_format("%Y"),
               breaks = seq.Date(from = min(graph_data$Year), to = max(graph_data$Year), by = "2 years")) +
  scale_y_continuous(labels = scales::comma_format()) +
  ggtitle("Santa Barbara County Employment","Private Sector") +
  labs(caption = "Source: Quarterly Census of Employment and Wages (QCEW)") +
  ylab("Annual Employment") +
  scale_color_manual(labels = c('first'  =  ind_names[1],
                                'second' =  ind_names[2],
                                'third'  =  ind_names[3]),
                     values = c('first'  = EFPcolors[3],
                                'second' = EFPcolors[1],
                                'third'  = EFPcolors[2])) +
  efp_theme +
  theme(panel.grid.major.y = element_line(color="gray"))

# ----- US bar graph ----- #

US_bar <- ggplot() +
  geom_col(data = subset(graph_data_bar, location == "US" & industry == "tot"),
           aes(x = Year, y = employment, fill = "Total Employment"), width=300) +
  geom_col(data = subset(graph_data_bar, location == "US" & industry != "tot"),
           aes(x = Year, y = employment, fill = industry), width=300) +
  scale_y_continuous(labels = scales::comma_format()) +
  ggtitle("US Private Employment","Industry Portion of Total Employment") +
  labs(caption = "Source: Quarterly Census of Employment and Wages (QCEW)") +
  ylab("Annual Employment") +
  scale_fill_manual(labels = c(ind_names[1], ind_names[2], ind_names[3], 'Total \nEmployment'),
                    values = c(EFPcolors[3], EFPcolors[1], EFPcolors[2], EFPcolors[4])) +
  efp_theme

# ----- State Bar Graph ----- #

State_bar <- ggplot() +
  geom_col(data = subset(graph_data_bar, location == "State" & industry == "tot"),
           aes(x = Year, y = employment, fill = "Total Employment"), width=300) +
  geom_col(data = subset(graph_data_bar, location == "State" & industry != "tot"),
           aes(x = Year, y = employment, fill = industry), width=300) +
  scale_y_continuous(labels = scales::comma_format()) +
  ggtitle("California Private Employment","Industry Portion of Total Employment") +
  labs(caption = "Source: Quarterly Census of Employment and Wages (QCEW)") +
  ylab("Annual Employment") +
  scale_fill_manual(labels = c(ind_names[1], ind_names[2], ind_names[3], 'Total \nEmployment'),
                    values = c(EFPcolors[3], EFPcolors[1], EFPcolors[2], EFPcolors[4])) +
  efp_theme



# ----- Local Bar Graph ----- #

Local_bar <- ggplot() +
  geom_col(data = subset(graph_data_bar, location == "Local" & industry == "tot"),
           aes(x = Year, y = employment, fill = "Total Employment"), width=300) +
  geom_col(data = subset(graph_data_bar, location == "Local" & industry != "tot"),
           aes(x = Year, y = employment, fill = industry), width=300) +
  scale_y_continuous(labels = scales::comma_format()) +
  ggtitle("Santa Barbara County Private Employment","Industry Portion of Total Employment") +
  labs(caption = "Source: Quarterly Census of Employment and Wages (QCEW)") +
  ylab("Annual Employment") +
  scale_fill_manual(labels = c(ind_names[1], ind_names[2], ind_names[3], 'Total \nEmployment'),
                    values = c(EFPcolors[3], EFPcolors[1], EFPcolors[2], EFPcolors[4])) +
  efp_theme

ggsave("Local_graph_ind.jpg", Local_graph_ind, height = 4, width = 6, path = outputloc)
ggsave("US_bar.jpg", US_bar, height = 4, width = 6, path = outputloc)
ggsave("State_bar.jpg", State_bar, height = 4, width = 6, path = outputloc)
ggsave("Local_bar.jpg", Local_bar, height = 4, width = 6, path = outputloc)