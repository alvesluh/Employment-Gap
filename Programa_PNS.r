#################################################
# Pesquisa Nacional de Saúde - 2013             #
#################################################

library(survey)

# Opções do R
options(digits = 15, scipen = 15)

# Carregar libraries 
library(PNSIBGE)
library(survey)
library(foreign)

# Opções de estimação da survey
options(survey.lonely.psu = "average")
options(survey.adjust.domain.lonely = TRUE)

# Realizando a leitura da base PNS 2013
base_pns = get_pns(year=2013, selected=FALSE, anthropometry=FALSE, 
vars=c("G006","V0001","C008","E01602","E01604","E01802","E01804","VDD004","E014","I001","P027"),
labels=TRUE, deflator=TRUE, design=TRUE,reload=TRUE, curlopts=list(), savedir=tempdir())

base_pns$variables$one = 1

# Variável de deficiência 
base_pns$variables$def[base_pns$variables$G006==1] = 1
base_pns$variables$def[!(base_pns$variables$G006==1)] = 0


# Criando variável de 14 anos ou mais de idade
base_pns$variables$uf = base_pns$variables$V0001
base_pns$variables$pia[base_pns$variables$C008 > "013"] = 1 
base_pns$variables$pia[base_pns$variables$C008 < "014"] = 0 


# Construção da variável rendimento de todos os trabalhos
rendtrab = cbind(base_pns$variables$E01602,base_pns$variables$E01604,base_pns$variables$E01802,base_pns$variables$E01804)
base_pns$variables$tt = apply(rendtrab,MARGIN = 1, FUN = sum, na.rm=T)
base_pns$variables$tt[base_pns$variables$tt==0] = NA

# Construindo nível de instrução
base_pns$variables$nivel[base_pns$variables$VDD004 %in% c(1,2)] = 1
base_pns$variables$nivel[base_pns$variables$VDD004 %in% c(3,4)] = 2
base_pns$variables$nivel[base_pns$variables$VDD004 %in% c(5,6)] = 3
base_pns$variables$nivel[base_pns$variables$VDD004 %in% c(7)] = 4
base_pns$variables$nivel = as.factor(base_pns$variables$nivel)

# Construindo posição na ocupação
base_pns$variables$posic[base_pns$variables$E014 %in% c(1)] = 1
base_pns$variables$posic[base_pns$variables$E014 %in% c(2)] = 2
base_pns$variables$posic[base_pns$variables$E014 %in% c(3)] = 3 
base_pns$variables$posic[base_pns$variables$E014 %in% c(4)] = 4
base_pns$variables$posic[base_pns$variables$E014 %in% c(5)] = 5
base_pns$variables$posic[base_pns$variables$E014 %in% c(6)] = 6
base_pns$variables$posic[base_pns$variables$E014 %in% c(7,8)] = 7
base_pns$variables$posic = as.factor(base_pns$variables$posic)


#Plano de saude - apenas teste
plano = svytotal( ~ I001 , base_pns , na.rm = TRUE )

#Deficiência física

# Criando desenhos com deficiencia e sem deficiencia
comdef = subset(base_pns, base_pns$variables$def==1)
semdef = subset(base_pns, !(base_pns$variables$def==1))
wpia = subset(base_pns, (base_pns$variables$pia==1))

# Pessoas 
t_pessoas = svyby( ~ one , ~ def , base_pns , svytotal , na.rm = TRUE )
pessoas = svyby( ~ one , ~ uf , base_pns , svytotal , na.rm = TRUE )
pessoas_com = svyby( ~ one , ~ uf , comdef , svytotal , na.rm = TRUE )
pessoas_sem = svyby( ~ one , ~ uf , semdef , svytotal , na.rm = TRUE )

100*(cv(t_pessoas))
100*(cv(pessoas_com))
100*(cv(pessoas_sem))

# Pessoas em idade de trabalhar (14 anos ou mais de idade)
pit = svytotal( ~ pia, base_pns, na.rm = TRUE)
pit_def = svyby( ~ pia , ~ def , base_pns , svytotal , na.rm = TRUE )
pit_tot = svyby( ~ pia , ~ uf , base_pns , svytotal , na.rm = TRUE )
pit_com = svyby( ~ pia , ~ uf , comdef , svytotal , na.rm = TRUE )
pit_sem = svyby( ~ pia , ~ uf , semdef , svytotal , na.rm = TRUE )

100*(cv(pit))
100*(cv(pit_def))
100*(cv(pit_tot))
100*(cv(pit_com))
100*(cv(pit_sem))

