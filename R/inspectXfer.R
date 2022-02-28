#' a function to root out and delete spurious and duplicate transfer courses
#' @param a transfer course file from UH peoplesoft, untouched
#' @param b transfer course hours file from UH peoplesoft, untouched
#' @export



inspectXfer <- function(a = transfer_courses_file, b = transfer_hours_file){

  library(readxl)
  library(magrittr)
  library(dplyr)
  trans1 <- readxl::read_excel(a, col_types = "text")

  names(trans1)[11] <- "college.Descr"
  names(trans1)[7]<- "Course.Descript"
  names(trans1)[37] <- "Grade_pt_p_unit"
  names(trans1)[31] <- "Unit.Taken"

  trans <- trans1 %>%
    .[!grepl("North Harris Montgomery C C Di", .$college.Descr),] %>%
    .[!duplicated(.[,c("ID", "Subject", "Catalog")]),]

  hours <- readxl::read_excel(b, col_types = "text") %>%
    .[,c("Transfer", "ID", "Term")] %>%
    .[rev(order(.$ID,.$Term)),] %>%
    .[!duplicated(.$ID),]

  trans2 <- merge(trans,hours, by = "ID", all.x = TRUE) %>% .[,c("ID", "Name", "Term", "Subject", "Catalog", "Transfer.x", "Course.Descript", "Grade", "Grade_pt_p_unit","college.Descr")] %>%
    .[grepl("A|B|C", .$Grade),]

  names(trans2)[6] <- "Unit.Taken"

  trans2 <- trans2[,-c(3)]

  trans2$subcat <- paste(trans2$Subject, trans2$Catalog, sep = " ") ############# subject and category was merged here

  trans2 <- trans2[,-c(3,4)]

  trans2 <- trans2[,c(1,8,2,3,4,5,6,7)]


  trans2$subcat <- gsub('ANTH 1300','ANTH 2346',trans2$subcat)
     trans2$subcat <- gsub('ANTH 2301','ANTH 2301',trans2$subcat)
     trans2$subcat <- gsub('ANTH 2302','ANTH 2351',trans2$subcat)
     trans2$subcat <- gsub('ANTH 2303','ANTH 2302',trans2$subcat)
     trans2$subcat <- gsub('ARAB 2301','ARAB 2311',trans2$subcat)
     trans2$subcat <- gsub('ARAB 2302','ARAB 2312',trans2$subcat)
      trans2$subcat <- gsub('ART 1301','ARTS 1316',trans2$subcat)
      trans2$subcat <- gsub('ART 1304','ARTS 2316',trans2$subcat)
     trans2$subcat <- gsub('ART 1310','ARTS 2333',trans2$subcat)
     trans2$subcat <- gsub('ART 1330','ARTS 1311',trans2$subcat)
     trans2$subcat <- gsub('ART 1350','ARTS 2346',trans2$subcat)
     trans2$subcat <- gsub('ART 1360','ARTS 2326',trans2$subcat)
     trans2$subcat <- gsub('ART 1371','ARTS 2356',trans2$subcat)
     trans2$subcat <- gsub('ART 1382','ARTS 2348',trans2$subcat)
     trans2$subcat <- gsub('ART 3300','ARTS 1317',trans2$subcat)
     trans2$subcat <- gsub('ART 3303','ARTS 2311',trans2$subcat)
     trans2$subcat <- gsub('ART 3306','ARTS 2366',trans2$subcat)
    trans2$subcat <- gsub('ARTH 1380','ARTS 1303',trans2$subcat)
    trans2$subcat <- gsub('ARTH 1381','ARTS 1304',trans2$subcat)
    trans2$subcat <- gsub('ASLI 1301','SGNL 1301',trans2$subcat)
    trans2$subcat <- gsub('ASLI 1302','SGNL 1302',trans2$subcat)
    trans2$subcat <- gsub('ASLI 2301','SGNL 2301',trans2$subcat)
    trans2$subcat <- gsub('ASLI 2302','SGNL 2302',trans2$subcat)
    trans2$subcat <- gsub('BIOL 1134','BIOL 2101',trans2$subcat)
    trans2$subcat <- gsub('BIOL 1144','BIOL 2102',trans2$subcat)
    trans2$subcat <- gsub('BIOL 1153','BIOL 2120',trans2$subcat)
    trans2$subcat <- gsub('BIOL 1161','BIOL 1106',trans2$subcat)
    trans2$subcat <- gsub('BIOL 1162','BIOL 1107',trans2$subcat)
     trans2$subcat <- gsub('BIOL 1309','BIOL 1319',trans2$subcat)
  trans2$subcat <- gsub('BIOL 1310','BIOL 1308',trans2$subcat)
  trans2$subcat <- gsub('BIOL 1320','BIOL 1309',trans2$subcat)
  trans2$subcat <- gsub('BIOL 1334','BIOL 2301',trans2$subcat)
  trans2$subcat <- gsub('BIOL 1344','BIOL 2302',trans2$subcat)
  trans2$subcat <- gsub('BIOL 1353','BIOL 2320',trans2$subcat)
  trans2$subcat <- gsub('BIOL 1361','BIOL 1306',trans2$subcat)
  trans2$subcat <- gsub('BIOL 1362','BIOL 1307',trans2$subcat)
  trans2$subcat <- gsub('BIOL 3132','BIOL 2121',trans2$subcat)
  trans2$subcat <- gsub('BIOL 3332','BIOL 2321',trans2$subcat)
   trans2$subcat <- gsub('BTEC 3100','BTEC 3200',trans2$subcat)
    trans2$subcat <- gsub('CHEM 1101','CHEM 1105',trans2$subcat)
    trans2$subcat <- gsub('CHEM 1301','CHEM 1305',trans2$subcat)
    trans2$subcat <- gsub('CHEM 1331','CHEM 1311',trans2$subcat)
    trans2$subcat <- gsub('CHEM 1332','CHEM 1312',trans2$subcat)
     trans2$subcat <- gsub('CHEM 2133','CHEM 3133',trans2$subcat)
   trans2$subcat <- gsub('CHEM 2233','CHEM 3333',trans2$subcat)
     trans2$subcat <- gsub('CHEM 3131','CHEM 2131',trans2$subcat)
   trans2$subcat <- gsub('CHEM 3132','CHEM 2132',trans2$subcat)
    trans2$subcat <- gsub('CHEM 3221','CHEM 2123',trans2$subcat)
    trans2$subcat <- gsub('CHEM 3222','CHEM 2125',trans2$subcat)
    trans2$subcat <- gsub('CHEM 3331','CHEM 2323',trans2$subcat)
    trans2$subcat <- gsub('CHEM 3332','CHEM 2325',trans2$subcat)
    trans2$subcat <- gsub('CHNS 2301','CHIN 2311',trans2$subcat)
    trans2$subcat <- gsub('CHNS 2302','CHIN 2312',trans2$subcat)
    trans2$subcat <- gsub('CIVE 2330','ENGR 2301',trans2$subcat)
    trans2$subcat <- gsub('COMM 1301','COMM 1307',trans2$subcat)
    trans2$subcat <- gsub('COMM 1333','SPCH 1318',trans2$subcat)
    trans2$subcat <- gsub('COMM 2310','COMM 2311',trans2$subcat)
    trans2$subcat <- gsub('COSC 1306','COSC 1336',trans2$subcat)
    trans2$subcat <- gsub('COSC 1430','COSC 1437',trans2$subcat)
    trans2$subcat <- gsub('COSC 2430','COSC 2436',trans2$subcat)
    trans2$subcat <- gsub('COSC 2440','COSC 2425',trans2$subcat)
    trans2$subcat <- gsub('CUIN 2320','CUIN 1350',trans2$subcat)
     trans2$subcat <- gsub('DAN 1201','DANC 1241',trans2$subcat)
     trans2$subcat <- gsub('DAN 1210','DANC 1247',trans2$subcat)
     trans2$subcat <- gsub('DAN 1211','DANC 1245',trans2$subcat)
      trans2$subcat <- gsub('DAN 1212','DANC 1246',trans2$subcat)
   trans2$subcat <- gsub('DAN 2300','DANC 1301',trans2$subcat)
    trans2$subcat <- gsub('DAN 2303','DANC 1309',trans2$subcat)
     trans2$subcat <- gsub('DAN 2307','DANC 2303',trans2$subcat)
    trans2$subcat <- gsub('ECON 2304','ECON 2302',trans2$subcat)
    trans2$subcat <- gsub('ECON 2305','ECON 2301',trans2$subcat)
    trans2$subcat <- gsub('EDUC 3301','EDUC 1301',trans2$subcat)
    trans2$subcat <- gsub('ENGL 1303','ENGL 1301',trans2$subcat)
    trans2$subcat <- gsub('ENGL 1304','ENGL 1302',trans2$subcat)
    trans2$subcat <- gsub('ENGL 2301','ENGL 2332',trans2$subcat)
    trans2$subcat <- gsub('ENGL 2302','ENGL 2333',trans2$subcat)
    trans2$subcat <- gsub('ENGL 2303','ENGL 2322',trans2$subcat)
    trans2$subcat <- gsub('ENGL 2304','ENGL 2323',trans2$subcat)
     trans2$subcat <- gsub('ENGL 2323', 'ENGL 2314',trans2$subcat)
  trans2$subcat <- gsub('FREN 2301','FREN 2311',trans2$subcat)
  trans2$subcat <- gsub('FREN 2302','FREN 2312',trans2$subcat)
 trans2$subcat <- gsub('GEOL 1130','GEOL 1103',trans2$subcat)
  trans2$subcat <- gsub('GEOL 1150','GEOL 1147',trans2$subcat)
  trans2$subcat <- gsub('GEOL 1176','GEOL 1104',trans2$subcat)
  trans2$subcat <- gsub('GEOL 1330','GEOL 1303',trans2$subcat)
  trans2$subcat <- gsub('GEOL 1350','GEOL 1347',trans2$subcat)
  trans2$subcat <- gsub('GEOL 1360','GEOL 1345',trans2$subcat)
  trans2$subcat <- gsub('GEOL 1376','GEOL 1304',trans2$subcat)
  trans2$subcat <- gsub('GERM 2331','GERM 2311',trans2$subcat)
  trans2$subcat <- gsub('GERM 2332','GERM 2312',trans2$subcat)
  trans2$subcat <- gsub('HDFS 2317','HDFS 2314',trans2$subcat)
   trans2$subcat <- gsub('HIST 1377','HIST 1301',trans2$subcat)
     trans2$subcat <- gsub('HIST 1378','HIST 1302',trans2$subcat)
   trans2$subcat <- gsub('HIST 2301','HIST 2303',trans2$subcat)
    trans2$subcat <- gsub('HIST 2341','HIST 2301',trans2$subcat)
     trans2$subcat <- gsub('HIST 2343','HIST 2302',trans2$subcat)
   trans2$subcat <- gsub('HIST 2343','HIST 2302',trans2$subcat)
    trans2$subcat <- gsub('HIST 2351','HIST 2311',trans2$subcat)
    trans2$subcat <- gsub('HIST 2353','HIST 2312',trans2$subcat)
   trans2$subcat <- gsub('HIST 2361','HIST 2321',trans2$subcat)
   trans2$subcat <- gsub('HIST 2363','HIST 2322',trans2$subcat)
   trans2$subcat <- gsub('HIST 3332','HIST 2327',trans2$subcat)
   trans2$subcat <- gsub('HIST 3333','HIST 2328',trans2$subcat)
   trans2$subcat <- gsub('HLT 2310','PHED 1306',trans2$subcat)
   trans2$subcat <- gsub('ITAL 2301','ITAL 2311',trans2$subcat)
   trans2$subcat <- gsub('ITAL 2302','ITAL 2312',trans2$subcat)
   trans2$subcat <- gsub('JPNS 2301','JAPN 2311',trans2$subcat)
   trans2$subcat <- gsub('JPNS 2302','JAPN 2312',trans2$subcat)
   trans2$subcat <- gsub('LATN 2301','LATI 2311',trans2$subcat)
   trans2$subcat <- gsub('LATN 2302','LATI 2312',trans2$subcat)
   trans2$subcat <- gsub('MATH 1310','MATH 1314',trans2$subcat)
   trans2$subcat <- gsub('MATH 1311','MATH 1332',trans2$subcat)
   trans2$subcat <- gsub('MATH 1312','MATH 1351',trans2$subcat)
   trans2$subcat <- gsub('MATH 1313','MATH 1324',trans2$subcat)
   trans2$subcat <- gsub('MATH 1314','MATH 1325',trans2$subcat)
   trans2$subcat <- gsub('MATH 1330','MATH 2312',trans2$subcat)
   trans2$subcat <- gsub('MATH 1332','MATH 1332',trans2$subcat)
   trans2$subcat <- gsub('MATH 1351','MATH 1351',trans2$subcat)
   trans2$subcat <- gsub('MATH 1431','MATH 2413',trans2$subcat)
   trans2$subcat <- gsub('MATH 1432','MATH 2414',trans2$subcat)
   trans2$subcat <- gsub('MATH 2311','MATH 1342',trans2$subcat)
   trans2$subcat <- gsub('MATH 2331','MATH 2318',trans2$subcat)
   trans2$subcat <- gsub('MATH 2433','MATH 2415',trans2$subcat)
   trans2$subcat <- gsub('MUSI 1160','MUSI 1181',trans2$subcat)
   trans2$subcat <- gsub('MUSI 1161','MUSI 1182',trans2$subcat)
   trans2$subcat <- gsub('MUSI 1170','MUSI 1116',trans2$subcat)
   trans2$subcat <- gsub('MUSI 1171','MUSI 1117',trans2$subcat)
   trans2$subcat <- gsub('MUSI 1300','MUSI 1303',trans2$subcat)
   trans2$subcat <- gsub('MUSI 1310','MUSI 1311',trans2$subcat)
   trans2$subcat <- gsub('MUSI 1311','MUSI 1312',trans2$subcat)
   trans2$subcat <- gsub('MUSI 2101','MUSI 1160',trans2$subcat)
   trans2$subcat <- gsub('MUSI 2160','MUSI 2181',trans2$subcat)
   trans2$subcat <- gsub('MUSI 2161','MUSI 2182',trans2$subcat)
   trans2$subcat <- gsub('MUSI 2170','MUSI 2116',trans2$subcat)
   trans2$subcat <- gsub('MUSI 2171','MUSI 2117',trans2$subcat)
   trans2$subcat <- gsub('MUSI 3300','MUSI 1307',trans2$subcat)
   trans2$subcat <- gsub('PHYS 1101','PHYS 1101',trans2$subcat)
   trans2$subcat <- gsub('PHYS 1102','PHYS 1102',trans2$subcat)
   trans2$subcat <- gsub('PHYS 1121','PHYS 2125',trans2$subcat)
  trans2$subcat <- gsub('PHYS 1122','PHYS 2126',trans2$subcat)
   trans2$subcat <- gsub('PHYS 1301','PHYS 1301',trans2$subcat)
   trans2$subcat <- gsub('PHYS 1302','PHYS 1302',trans2$subcat)
   trans2$subcat <- gsub('PHYS 1305','PHYS 1304',trans2$subcat)
   trans2$subcat <- gsub('PHYS 1306','PHYS 1303',trans2$subcat)
   trans2$subcat <- gsub('PHYS 1321','PHYS 2325',trans2$subcat)
   trans2$subcat <- gsub('PHYS 1322','PHYS 2326',trans2$subcat)
 trans2$subcat <- gsub('POLS 1336','GOVT 2306',trans2$subcat)
 trans2$subcat <- gsub('POLS 1337','GOVT 2305',trans2$subcat)
 trans2$subcat <- gsub('PSYC 1300','PSYC 2301',trans2$subcat)
  trans2$subcat <- gsub('PSYC 2301','PSYC 2305',trans2$subcat)
  trans2$subcat <- gsub('PSYC 2335','PSYC 3335',trans2$subcat)
  trans2$subcat <- gsub('PSYC 2344','PSYC 4344',trans2$subcat)
trans2$subcat <- gsub('PSYC 2350','PSYC 2308',trans2$subcat)
 trans2$subcat <- gsub('PSYC 2351','PSYC 2307',trans2$subcat)
 trans2$subcat <- gsub('PSYC 2380','PSYC 2319',trans2$subcat)
  trans2$subcat <- gsub('PSYC 3301','PSYC 2317',trans2$subcat)
  trans2$subcat <- gsub('PSYC 3325','PSYC 2316',trans2$subcat)
  trans2$subcat <- gsub('PSYC 3337','PSYC 2306',trans2$subcat)
  trans2$subcat <- gsub('PSYC 3341','PSYC 2330',trans2$subcat)
 trans2$subcat <- gsub('PSYC 4321','PSYC 2320',trans2$subcat)
 trans2$subcat <- gsub('RUSS 2301','RUSS 2311',trans2$subcat)
 trans2$subcat <- gsub('RUSS 2302','RUSS 2312',trans2$subcat)
  trans2$subcat <- gsub('SOC 1300','SOCI 1301',trans2$subcat)
 trans2$subcat <- gsub('SPAN 2301','SPAN 2311',trans2$subcat)
 trans2$subcat <- gsub('SPAN 2302','SPAN 2312',trans2$subcat)
 trans2$subcat <- gsub('SPEC 3360','EDUC 2301',trans2$subcat)
 trans2$subcat <- gsub('THEA 1329','DRAM 2336',trans2$subcat)
 trans2$subcat <- gsub('THEA 1331','DRAM 1310',trans2$subcat)
  trans2$subcat <- gsub('THEA 1338','DRAM 1351',trans2$subcat)
 trans2$subcat <- gsub('THEA 1339','DRAM 1352',trans2$subcat)
  trans2$subcat <- gsub('THEA 2223','DRAM 1341',trans2$subcat)
 trans2$subcat <- gsub('THEA 2320','DRAM 2351',trans2$subcat)
 trans2$subcat <- gsub('THEA 2335','DRAM 1330',trans2$subcat)
trans2$subcat <- gsub('THEA 2336','DRAM 2331',trans2$subcat)
trans2$subcat <- gsub('ARTS ELEC','ART ELEC',trans2$subcat)
trans2$subcat <- gsub('DANC ELEC','DAN ELEC',trans2$subcat)
trans2$subcat <- gsub('EPSY 3300','HDFS 3300',trans2$subcat)
trans2$subcat <- gsub('HDFS 3300','EPSY 3300',trans2$subcat)
trans2$subcat <- gsub('HDFS 3360','SPEC 3360',trans2$subcat)




  ############you're going to need to split sub cat for the rest of the code to work again below


  ##################################################

  ################# End of GSUBs

  #####################################################





   ####################### below is the transfer numbers from peoplesoft

  now <- readxl::read_excel(b) %>%
    .[rev(order(.$ID,.$Term)),]%>%
    .[,c(1,7,8)] %>%
    .[!duplicated(.$ID),]


final <- merge(trans2, now, by = "ID", all.x = T) #carry forward english


  final9 <- final %>%
    dplyr::group_by(ID) %>%
    dplyr::distinct(subcat,.keep_all = TRUE)

  final9$Unit.Taken <- as.numeric(final9$Unit.Taken)

  final3 <- final9 %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(sum(Unit.Taken))

  finall <- merge(final9,final3, by = "ID", all.x = T)

  finall <- finall[order(finall$ID,finall$subcat),]

  return(finall)

}


