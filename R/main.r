#install.packages("tidyverse")
#install.packages("anytime")
#install.packages("maps")
#install.packages("scales")


library(tidyverse) 
library(lubridate)
library(anytime)
library(maps)
library(scales)

# Cores para os plots
pinkColors <- c("#FA3C8A", "#000000", "#370117", "#6E022F", "#A50446", "#DD055E", "#F9217A", "#FA599B", "#FC90BC", "#FDC7DD")

df <- read.csv2("../teste_dados_ecommerce.csv")

head(df)

df$UnitPrice <- as.numeric(df$UnitPrice)
df$StoreId <- as.character(df$StoreId)
# Existem 2 tipos de máscaras diferentes nos dados de datas, deixei de usar o as.POSIXct para usar o parse_date_time.
df$InvoiceDate <- parse_date_time(df$InvoiceDate, orders = c('mdy HM', 'mdY HM'))

df <- df  %>% mutate(PurchaseValue = Quantity * UnitPrice)

head(df)

df  %>% select(Description, Quantity,PurchaseValue ) %>%  arrange(PurchaseValue)%>% distinct(Description)  %>% head()
# Como existem valores em Quantity negativos e com descrições do tipo "AMAZON FEE", "POSTAGE", etc. Pode-se subenteder que os valores negativos representam 
# Perdas ou despesas. Para calcular o valor das lojas que tem o maior faturamento não será necessário levar em consideração esses valores negativos, porém para 
# calcular os lucros de cada loja será preciso fazer a conta lucro = Faturamento - Despesas. Para todos os efeitos estou usando um filtro onde  PurchaseValue > 0 para evitar problemas
# em summarizes

# Optei por retirar da análise esses valores que aparentam não serem operações de compra.
df <- df  %>% filter(!StockCode %in% c("S","M", "m", "POST", "DOT", "BANK CHARGES", "AMAZONFEE", "B", "C2")) 

# Insight 1: 10 lojas com os maiores faturamentos em pedidos
# Insight 1.1: Volume total de items de venda para as 10 lojas com maior faturamento em pedidos
insight1 <- df  %>% 
    select(StoreId, Quantity, PurchaseValue )  %>% 
    filter( PurchaseValue > 0) %>%  
    group_by(StoreId)  %>%
    summarise(sumQuantity = sum(Quantity), sumPurchaseValue = sum(PurchaseValue))  %>%  
    arrange(desc(sumPurchaseValue))  %>%
    head(10)
insight1

plotInsight1 <- insight1  %>% 
    ggplot(aes(x =  reorder(StoreId, sumPurchaseValue), y = sumPurchaseValue,fill = StoreId)) + 
    geom_bar(stat= "identity") +  
    geom_text(aes(label = label_number_si(accuracy=0.01, )(sumPurchaseValue)), hjust = -0.3,fill = "white", label.size = 0)+
    coord_flip()+ 
    theme_minimal()+ 
    scale_fill_manual(values = pinkColors)+
    theme(  plot.title = element_text(hjust = 0.5, size = 24),axis.text.y = element_text(size = 20, hjust = 1), axis.ticks.x = element_blank(), axis.text.x = element_blank(),panel.background = element_rect(fill = 'grey'),) +
    labs(title="Insight 1: 10 lojas com os maiores faturamentos em pedidos",x ="StoreId", y = "PurchaseValue",fill="none") +
    guides(fill="none")

plotInsight1.1 <- insight1 %>% ggplot(aes(x =  reorder(StoreId, sumPurchaseValue), y = sumQuantity,fill = StoreId)) +
    geom_bar(stat= "identity") +
    geom_text(aes(label = label_number_si(accuracy=0.01, )(sumQuantity)), hjust = -0.3,fill = "white", label.size = 0)+
    coord_flip()+ 
    theme_minimal()+ 
    scale_fill_manual(values = pinkColors)+
    theme( plot.title = element_text(hjust = 0.5, size = 24), axis.text.y = element_text(size = 20, hjust = 1),  axis.ticks.x = element_blank(), axis.text.x = element_blank(), panel.background = element_rect(fill = 'grey'),) +
    labs(title="Insight 1.1: Volume total de items de venda para as 10 lojas com maior faturamento em pedidos \n ordenado por maior faturamento",x ="StoreId", y = "sumQuantity",fill="none") +
    guides(fill="none")

insight2 <- df %>% 
    select(CustomerID,InvoiceDate,  PurchaseValue) %>% 
    filter(PurchaseValue > 0, !is.na(CustomerID)) %>% 
    group_by(CustomerID, monthYear = floor_date(InvoiceDate, "month"))  %>% 
    summarise(PurchaseValue = sum(PurchaseValue)) %>% 
    arrange(monthYear)
    
insight2$monthYear <- format(insight2$monthYear, "%b/%Y")

head(insight2)

# Quantidade de clientes
insight2  %>% select(CustomerID)  %>% distinct()  %>% nrow()

# Divisão da soma do valor total de faturamento do mês pelo número total de clientes (4334) para chegar no Ticket médio mensal
# Insight 2: Ticket médio mensal dos pedidos
insight2 <- insight2   %>% 
    select(monthYear, PurchaseValue)  %>% 
    group_by(monthYear = factor(monthYear, levels = unique(monthYear)))  %>% 
    summarise(PurchaseValue = round(sum(PurchaseValue)/4334, 2))
insight2

plotInsight2 <- insight2  %>% ggplot(aes(x= monthYear, y = PurchaseValue, group = 1)) +
    geom_line(color="#fb3d8a",size=1.2) +
    geom_point(color="#fb3d8a",size=2) +
    geom_text(aes(label = PurchaseValue), nudge_y = 7) +
    theme_minimal() +
    labs(title="Insight 2: Ticket médio mensal dos pedidos")+
    theme( plot.title = element_text(hjust = 0.5, size = 24),axis.text.y = element_text(size = 20, hjust = 1), axis.text.x = element_text(angle = 15, size = 20, hjust = 1),panel.background = element_rect(fill = 'grey'),)

 # Como existem faturas que não constam número de clientes, para responder o volume médio mensal de vendas de todas as lojas optei por fazer 2 pesquisas:
 
 # Uma pesquisa não levando em consideração as faturas que não estão relacionadas a clientes:
 insight2.1Customer <- df %>% 
    select(CustomerID,InvoiceDate,  PurchaseValue) %>% 
    filter(PurchaseValue > 0, !is.na(CustomerID)) %>% 
    group_by(CustomerID, monthYear = floor_date(InvoiceDate, "month"))  %>% 
    summarise(PurchaseValue = sum(PurchaseValue)) %>% 
    arrange(monthYear)
insight2.1Customer$monthYear <- format(insight2.1Customer$monthYear, "%b/%Y")

# E uma pesquisa levando em consideração todos os valores, inclusive as faturas que não estão relacionadas a um cliente:
insight2.1NoCustomer <- df %>% 
    select(CustomerID,InvoiceDate,  PurchaseValue) %>% 
    filter(PurchaseValue > 0) %>% 
    group_by(CustomerID, monthYear = floor_date(InvoiceDate, "month"))  %>% 
    summarise(PurchaseValue = sum(PurchaseValue)) %>% 
    arrange(monthYear)
insight2.1NoCustomer$monthYear <- format(insight2.1NoCustomer$monthYear, "%b/%Y")

insight2.1Customer <- insight2.1Customer  %>% 
    select(monthYear, PurchaseValue)  %>% 
    group_by(monthYear = factor(monthYear, levels = unique(monthYear)))  %>% 
    summarise(PurchaseValue = round(sum(PurchaseValue), 2))

# Para a primeira análise temos R$ 673.928,20 de volume médio mensal de vendas.
insight2.1Customer %>% select(PurchaseValue)  %>% summarise(mean(PurchaseValue))

insight2.1NoCustomer <- insight2.1NoCustomer  %>% 
    select(monthYear, PurchaseValue)  %>% 
    group_by(monthYear = factor(monthYear, levels = unique(monthYear)))  %>% 
    summarise(PurchaseValue = round(sum(PurchaseValue), 2))

# Para a segunda análise temos R$ 790.163 de volume médio mensal de vendas.
insight2.1NoCustomer %>% select(PurchaseValue)  %>% summarise(mean(PurchaseValue))

# Insight 3: Países com os maiores números de vendas.
insight3 <- df %>% select(Country,PurchaseValue)  %>% 
    filter( PurchaseValue > 0) %>% 
    group_by(Country) %>% 
    summarise(PurchaseValue = sum(PurchaseValue)) %>% 
    arrange(desc(PurchaseValue))

# Pequena mudança manual para plotar no mapa mundi.
insight3$Country[insight3$Country == "United Kingdom"] <- "UK"
insight3


world_map <- map_data("world")
world_map <- subset(world_map, region != "Antarctica")
# HeatMap dos países com os maiores números de vendas.
plotInsight3 <- ggplot(data = insight3) +
  geom_map(dat = world_map, map = world_map, aes(map_id = region),fill = "grey", color = "#7f7f7f", size = 0.1) +
  geom_map(map = world_map, aes(map_id = Country, fill = PurchaseValue), size = 0.25, color = "#7f7f7f") +
  scale_fill_gradient(low = "#ffffff", high = "#FA3C8A",trans = "log") +
  coord_fixed(1.3) +
  expand_limits(x = world_map$long, y = world_map$lat)+
  theme(plot.title = element_text(hjust = 0.5, size = 24),axis.ticks.x = element_blank(),axis.text.x = element_blank(), axis.ticks.y = element_blank(),axis.text.y = element_blank())+
  labs(title="Insight 3: HeatMap dos países com maior volume de vendas",) 

# Insight 3.1: Relação de produtos mais vendidos dentro do país com maior volume de vendas (Reino Unido).
insight3.1 <- df %>% select(StockCode, Description,PurchaseValue, Country,Quantity)  %>% 
    filter( PurchaseValue > 0, Country == "United Kingdom") %>% 
    group_by(StockCode, Description, Country) %>% 
    summarise(PurchaseValue = sum(PurchaseValue), Quantity = sum(Quantity)) %>% 
    arrange(desc(PurchaseValue))  %>% 
    head(10)
insight3.1 

plotInsight3.1 <- ggplot(data = insight3.1, aes(x = reorder(Description, -PurchaseValue), y = PurchaseValue, fill = Description))+
    geom_bar(stat='identity')+
    theme( plot.title = element_text(hjust = 0.5, size = 24),axis.text.y = element_text(size = 20, hjust = 1), , axis.text.x = element_text(angle = 80, size = 20, hjust = 1),panel.background = element_rect(fill = 'grey'),) +
    scale_fill_manual(values = pinkColors)+
    labs(title="Insight 3.1: Os 10 Items mais vendidos no Reino Unido ",x ="Description", y = "PurchaseValue",fill="none") +
    guides(fill="none")


# Insight 3.2: Relação de produtos que tiveram os menores valores de compra dentro do país com maior volume de vendas (Reino Unido).

insight3.2 <- df %>% select(StockCode, Description,PurchaseValue, Country,Quantity)  %>% 
    filter( PurchaseValue > 0, Country == "United Kingdom") %>% 
    group_by(StockCode, Description,Country) %>% 
    summarise(PurchaseValue = sum(PurchaseValue), Quantity = sum(Quantity)) %>% 
    arrange(PurchaseValue) %>% 
    head(10)
insight3.2 

plotInsight3.2 <- ggplot(data = insight3.2, aes(x = reorder(Description, PurchaseValue), y = PurchaseValue, fill = Description))+
    geom_bar(stat='identity')+
    theme( plot.title = element_text(hjust = 0.5, size = 24),axis.text.y = element_text(size = 20, hjust = 1), , axis.text.x = element_text(angle = 80, size = 20, hjust = 1),panel.background = element_rect(fill = 'grey'),) +
    scale_fill_manual(values = pinkColors)+
    labs(title="Insight 3.2: Os 10 Items com menor valor de compra no Reino Unido ",x ="Description", y = "PurchaseValue",fill="none") +
    guides(fill="none")

# Insight 3.3: Relação dos produtos menos comprados (Quantidade) dentro do país com maior volume de vendas (Reino Unido).
# OBS: Existem cerca de 86 produtos que só venderam 1 unidade.
insight3.3 <- df %>% select(StockCode, Description,PurchaseValue, Country,Quantity)  %>% 
    filter( PurchaseValue > 0, Country == "United Kingdom") %>% 
    group_by(StockCode, Description,Country) %>% 
    summarise(PurchaseValue = sum(PurchaseValue), Quantity = sum(Quantity)) %>% 
    arrange(Quantity)  %>% 
    head(10)
insight3.3

# Plot dos Insights em jpeg
jpeg(file = "insight1.jpeg", width=1280, height=720)
plotInsight1
dev.off()

jpeg(file = "insight1.1.jpeg", width=1280, height=720)
plotInsight1.1
dev.off()

jpeg(file = "insight2.jpeg", width=1280, height=720)
plotInsight2
dev.off()

jpeg(file = "insight3_map.jpeg", width=1280, height=720)
plotInsight3
dev.off()

jpeg(file = "insight3.1.jpeg", width=1280, height=900)
plotInsight3.1
dev.off()

jpeg(file = "insight3.2.jpeg", width=1280, height=900)
plotInsight3.2
dev.off()

# Salvando as alterações para serem usadas futuramente
write.csv(df,"../teste_dados_ecommerce_tratado.csv", row.names = FALSE)
