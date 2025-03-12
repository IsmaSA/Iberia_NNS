
### The order of co-authors was redefined based on their effort to double checl the data 
# This does not apply to Fran and Sergio as part of the original core team (Sergio, Fran & Ismael)

getwd()

ib <- read_xlsx("/home/ismael-soto/Desktop/ELZA/Iberia/Database/ListNNS.Iberia.xlsx", sheet = 2)

unique(ib$Double_check_by)

fix <- c(
  "EmiliGB" = "EmiliGB",
  "Cesar" = "Cesar",
  "IS" = "IS",
  "IS, Javier O." = "IS, Javier O.",
 "IS, Cesar, Javier O." = "IS, Cesar, Javier O.",
 "IS,Cesar"  = "IS, Cesar" ,
  "Sergio; Carolina MM" = "Sergio, Carolina MM",
  "IS; CMM" = "IS, Carolina MM",
  "Sergio; CMM" = "Sergio, Carolina MM",
  "Carlos CB; CMM" = "Carlos CB, Carolina MM",
  "Carolina MM" = "Carolina MM",
  "Sergio" = "Sergio",
  "Sergio, Cesar" = "Sergio, Cesar",
  "RSousa" = "RSousa",
  "RSousa, EGB" = "RSousa, EmiliGB",
  "IS, FRAN" = "IS, FRAN",
  "Carlos CB" = "Carlos CB",
  "IS, Emili" = "IS, EmiliGB",
  "X" = NA,  # esto creo que soy yo :)
  "Carlos CB/Sergio" = "Carlos CB, Sergio",
  "IS, EG" = "IS, EmiliGB",
  "Rsousa" = "RSousa",
  "RSousa; Carolina MM" = "RSousa, Carolina MM",
  "Carlos CB; Carolina MM" = "Carlos CB, Carolina MM")

ib <- ib %>%mutate(Double_check_by = str_replace_all(Double_check_by, fix)) %>%  
separate_rows(Double_check_by, sep=",") %>% mutate(Double_check_by = trimws(Double_check_by))

table(ib$Double_check_by)
order <- ib %>% group_by(Double_check_by) %>% summarise(n = n()) %>% arrange(desc(n))

### ORDER:

 1 IS               3340
 2 Cesar             126
 3 RSousa             87
 4 EmiliGB            75
 5 Carolina MM        40
 6 Sergio             35
 7 Javier O.          18
 8 Carlos CB          15
 9 NA                  5
10 FRAN                2