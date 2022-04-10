bilingual sped keeping only those rows
================
Dr. Mario Martinez
4/10/2022

## Get just rows for undergrads and bilinguals and speds for one row without duplicates

GOAL: get file with all people, no dups. AND bilinguals and SPEDS
without duplicating core rows 1. grab bilingual rows with grepl

2.  Then take out bilingual rows from original dataframe (call it orig)
    to make generic data frame without bilingual. Now there are 2 data
    frames (orig + bilin).

3.  Take out all IDs identifying bilinguals from orig data frame.

4.  paste bilin to orig. Then you have no dups.

5.  Do the same with SPED

6.  first delete advanced

The file starts from a finisher file that has been downloaded from ecos
as finisher file.

``` r
a <- readxl::read_excel("C:\\path\\to\\document\\here.xlsx")


a1 <- a[!grepl("934|936|1983|1042|1984|903|1110", a$`Cert Code`),] # just undergrads--take out advanced

a1 <- a1[!grepl("Alt", a1$`Program Type`),] # take out alts

bilin <- a1[grepl("1497",a1$`Cert Code`),] # bilingual only

orig <- a1[!grepl("953|979|1497|1870|953", a1$`Cert Code`),] # take out codes for bilingual and sped but this leaves the rows for PSID 
                                                                #cases for those that have bilin and SPED we want to eliminate these

teaid <- bilin$`TEA ID`

teaid <- as.character(teaid)

that <- paste(teaid, sep = "", collapse= "|")


orig1 <- orig[!grepl(that, orig$`TEA ID`),] #now we've eliminated those cases that are bilin and have core

sped <- a1[grepl("953|979|1870|953",a1$`Cert Code`),]


teaid1 <- sped$`TEA ID`

teaid1 <- as.character(teaid1)

other <- paste(teaid1, sep = "", collapse= "|")

orig2 <- orig1[!grepl(other, orig1$`TEA ID`),] #now we've eliminated those cases that are bilin and have core and sped and have core... we just have core

data <- data.frame(rbind(bilin,sped, orig2)) # This is the final data frame without duplicates 
```
