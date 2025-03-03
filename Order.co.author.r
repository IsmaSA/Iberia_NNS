
### The order of co-authors was redefined based on their effort to double checl the data 
# This does not apply to Fran and Sergio as part of the original core team (Sergio, Fran & Ismael)

getwd()

ib <- read_xlsx("/home/ismael-soto/Documents/GitHub/Iberia_NNS/Database/ListNNS.Iberia.xlsx", sheet = 2)

unique(ib$Double_check_by)

fix <- c(
  "EmiliGB" = "EmiliGB",
  "IS" = "IS",
  "Sergio; Carolina MM" = "Sergio, Carolina MM",
  "IS; CMM" = "IS, Carolina MM",
  "Sergio; CMM" = "Sergio, Carolina MM",
  "Carlos CB; CMM" = "Carlos CB, Carolina MM",
  "Carolina MM" = "Carolina MM",
  "Sergio" = "Sergio",
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

1 IS               3240
2 RSousa             87
3 EmiliGB            75
4 Carolina MM        40
5 Sergio             35
6 Carlos CB          15
7 NA                  5
8 FRAN                2