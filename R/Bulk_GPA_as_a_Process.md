Bulk GPA as a Process
================
Dr. Mario Martinez
4/11/2022

First Objective: determine if transfer credits verify out to the
transfer credits taken on the books. Be sure to

``` r
a <- "C:\\Users\\path to documents here\\Downloads\\TRANSFER.xlsx" # transfer courses

b <- "C:\\Users\\path to documents here\\Downloads\\HOURS.xlsx" # transfer hours

library(ie)

this <- ie::inspectXfer(a,b)

head(this) # inspect
```

Now that the first process is complete, Get the PSIDs to process the
rest of the GPA for the verified transfer hour cases.

``` r
this$yes <- ifelse(this$Transfer - this$`sum(Unit.Taken)` == 0, 0,1)


verified <- this[,c(1:8)]

# openxlsx::write.xlsx(verified, "c:/users/path to your documents /downloads/verified.xlsx")


verified1 <- this[grepl("0", this$yes),] %>%
  .[!duplicated(.$ID),] %>%
  .[,1] %>%
  data.frame()

  head(verified1)
```

From here, put the PSIDs into peoplesoft. Retrieve from Sharepoint
folder

``` r
d <- "c:/users/mario/downloads/uh-3-23-22.xlsx"

uh <- readxl::read_excel(d) %>%
    .[!(grepl("[[:digit:]]", .$`Drop Dt`)),] %>%
    .[,-12] %>%
    .[!(grepl("2160", .$Term)),]

head(uh) #inspect
```

Cleanup with the code below

``` r
library(dplyr)
uh$subcat <- paste(uh$Subject, uh$Catalog, sep = " ") 

uh4 <- uh %>% dplyr::mutate(New_Grades = ifelse(`Grade In` == "A", 4.00,
                                                ifelse(`Grade In` == "A-", 3.67,
                                                ifelse(`Grade In` == "B+", 3.33,
                                                ifelse(`Grade In` ==  "B", 3.00,
                                                ifelse(`Grade In` == "B-", 2.67,
                                                ifelse(`Grade In` == "C+", 2.33,
                                                ifelse(`Grade In` == "C", 2.00,
                                                ifelse(`Grade In` == "C-", 1.67,
                                                ifelse(`Grade In` == "D+", 1.33,
                                                ifelse(`Grade In` == "D", 1.00,
                                                ifelse(`Grade In` == "D-", 0.67,
                                                ifelse(`Grade In` == "F", 0.00, "2"))))))))))))) # S = 2 otherwise


uh1 <- uh4[,c(1,12,2,7,6,8,13,11)] %>%
  data.frame() # Keep these columns and reorder them 

names(uh1)[5]<- "Course.Descript"
names(uh1)[7] <- "Grade_pt_p_unit"
names(uh1)[4] <- "Unit.Taken"
names(uh1)[6] <- "Grade"
names(uh1)[8] <- "college.Descr"
uh2 <- uh1 %>% dplyr::filter(complete.cases(.))
uh3 <- uh2[!grepl("NCR",uh2$Grade),]
```

Get CONTENT GPA

``` r
data <- data.frame(rbind(uh2,verified)) %>%
  .[order(.$ID),] 
## STOP HERE AND INSPECT CAREFULLY.

data$Grade_pt_p_unit <- as.numeric(data$Grade_pt_p_unit)

  
fin1  <- data %>%
  dplyr::group_by(ID) %>% 
  dplyr::mutate(grdpt_unit = Unit.Taken * Grade_pt_p_unit)

sumgpu1  <- fin1 %>% 
  dplyr::group_by(ID) %>%
  dplyr::summarise(sum(grdpt_unit))


sum_u1 <- fin1 %>% 
    dplyr::group_by(ID) %>%
    dplyr::summarise(sum(Unit.Taken))

  
totals1 <- merge(sumgpu1, sum_u1, by = "ID", all.x = TRUE)
      totals1$GPA <- totals1[,2]/totals1[,3]


tr71 <- merge(fin1, totals1, by = "ID", all.x = TRUE)

head(tr71) # inspect carefully.
```

Content GPA below. This will depend on the certificate. Below we give
for generalist EC-6

``` r
subs <- "ENGL|MATH|BCHM|BIOL|CHEM|GEOL|PHYS|ANTH|ECON|GEOG|HIST|POLS|PSYC|SOC|ARED|ART|ARTH|MUAP|MUED|MUSA|MUSI|DAN|THEA|DRAM|KIN|PEB|HLT|NUTR"

data1 <- data[grepl(subs, data$subcat),]

names(data1)[4] <- "Unit.Taken1"
names(data1)[7] <- "Grade_pt_p_unit1"

  fina1  <- data1 %>%
  dplyr::group_by(ID) %>% 
  dplyr::mutate(grdpt_unit = Unit.Taken1 * Grade_pt_p_unit1)
  
  sumgpu1a  <- fina1 %>% 
  dplyr::group_by(ID) %>%
  dplyr::summarise(sum(grdpt_unit))
  
    sum_u1a <- fina1 %>% 
    dplyr::group_by(ID) %>%
    dplyr::summarise(sum(Unit.Taken1))
    
        totals1a <- merge(sumgpu1a, sum_u1a, by = "ID", all.x = TRUE)
        
              totals1a$GPA <- totals1a[,2]/totals1a[,3]
              
  tr72 <- merge(tr71,totals1a, by= "ID", all.x = T)
```
