require(dplyr)
require(stringr)

qualifica_telefone <- function(dados, col_id, col_telefone){
  
  ddds <- c(11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 24, 27, 28, 
            31, 32, 33, 34, 35, 37, 38, 41, 42, 43, 44, 45, 46, 47, 
            48, 49, 51, 53, 54, 55, 61, 62, 63, 64, 65, 66, 67, 68,
            69, 71, 73, 74, 75, 77, 79, 81, 82, 83, 84, 85, 86, 87, 
            88, 89, 91, 92, 93, 94, 95, 96, 97, 98, 99)
  
  telefones <- dados[[col_telefone]] %>% 
    as.character() %>% 
    stringr::str_remove_all("[^[0-9]]")
  
  # Padronização
  saida <- dados %>% 
    mutate(tel_aux = telefones,
           DDD = as.character(tel_aux) %>% 
             stringr::str_sub(1, 2) %>% 
             as.numeric(),
           Tel = as.character(tel_aux) %>% 
             stringr::str_sub(3, 11) %>% 
             as.numeric()) %>% 
    select(-tel_aux)
  
  saida <- saida %>% 
    mutate(Tel = case_when(
      is.na(Tel) ~ Tel,
      between(Tel, 60000000, 99999999) ~ as.numeric(paste0('9', Tel)),
      T ~ as.numeric(Tel)
    ),
    Tel_qualificado = paste0(DDD, Tel) %>% 
      as.numeric())
  
  # Identificação
  saida <- saida %>% 
    mutate(valid_tel = case_when(
      between(Tel, 900000000, 999999999) | between(Tel, 20000000, 59999999) ~ T,
      T ~ F
    ),
    tipo_telefone = case_when(
      between(Tel, 900000000, 999999999) ~ "Movel",
      between(Tel, 20000000, 59999999) ~ "Fixo",
      T ~ "NI"
    ),
    valid_ddd = case_when(
      DDD %in% ddds ~ T,
      T ~ F
    )
    )

  return(saida)
}
  

teste <- data.frame(id_pessoa = 1:20,
                    telefone = c("(19) 43925677", "(20) 2463-3099", "(11) 9873-9332",
                                 "(14) 85-8504", "(94) 4525-780k2", "(79) 3901-9808", 
                                 "(15) 9964-5178", "(37) 4714-7535", "(15) 4900965-5535",
                                 "(41) 3382-4193", "(11) 79772950", "(31) 6830-6321", 
                                 "(11) 56507084", "(51) 3460-8449", "(91) 4074-7353",
                                 "(44) 6340-7164", "(11) 5698-4418", "(31) 8951-2702", 
                                 "(43) 3495-3576", "(16) 3669-2444"))

qualifica_telefone(dados = teste, "id_pessoa", "telefone")