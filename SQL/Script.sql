SELECT  * FROM ec_data; 

-- Insight 1: 10 lojas com os maiores faturamentos em pedidos
-- Insight 1.1: Volume total de items de venda para as 10 lojas com maior faturamento em pedidos
SELECT StoreId, 
	sum(Quantity) as Quantity, 
	sum(PurchaseValue) as PurchaseValue   
FROM ec_data
WHERE PurchaseValue > 0 
GROUP BY StoreId 
ORDER BY sum(PurchaseValue) DESC 
LIMIT 10;


-- Insight 2: Ticket médio mensal dos pedidos
SELECT STRFTIME('%Y/%m', InvoiceDate ) as monthYear, 
	sum(PurchaseValue)/4334 as PurchaseValue  
	FROM ec_data 
WHERE PurchaseValue > 0 
	AND CustomerID is not 'NA'
GROUP BY monthYear;

-- Insight 2.1: Volume médio mensal de vendas
-- Para a primeira análise temos R$ 673.928,20 de volume médio mensal de vendas sem levar em consideração faturas que não constam número de clientes.
SELECT 
	AVG(PurchaseValue) 
FROM ( 
	SELECT  
		STRFTIME('%Y/%m', InvoiceDate ) as yearMonth,  
		sum(PurchaseValue) as PurchaseValue
	FROM ec_data
	WHERE PurchaseValue > 0 
		AND CustomerID is not 'NA'
	GROUP BY yearMonth
);


-- Insight 3: Países com os maiores números de vendas.
SELECT 
	Country, 
	sum(PurchaseValue) as PurchaseValue 
	FROM ec_data
WHERE  PurchaseValue > 0  
GROUP BY Country 
ORDER BY PurchaseValue DESC;

-- Insight 3.1: Relação de produtos mais vendidos dentro do país com maior volume de vendas (Reino Unido).
SELECT 
	StockCode, 
	Description, 
	sum(PurchaseValue) as PurchaseValue, 
	Country,
	sum(Quantity) as Quantity 
FROM ec_data
WHERE PurchaseValue > 0
	AND Country == "United Kingdom" 
GROUP BY StockCode, Description, Country
ORDER BY PurchaseValue DESC;

-- Insight 3.2: Relação de produtos que tiveram os menores valores de compra dentro do país com maior volume de vendas (Reino Unido).
SELECT 
	StockCode, 
	Description, 
	sum(PurchaseValue) as PurchaseValue, 
	Country,
	sum(Quantity) as Quantity
FROM ec_data
WHERE PurchaseValue > 0
	AND Country == "United Kingdom" 
GROUP BY StockCode, Description, Country
ORDER BY PurchaseValue;

-- Insight 3.3: Relação dos produtos menos comprados (Quantidade) dentro do país com maior volume de vendas (Reino Unido).
-- OBS: Existem cerca de 86 produtos que só venderam 1 unidade.
SELECT 
	StockCode, 
	Description, 
	sum(PurchaseValue) as PurchaseValue, 
	Country,
	sum(Quantity) as Quantity
FROM ec_data
WHERE PurchaseValue > 0
	AND Country == "United Kingdom" 
GROUP BY StockCode, Description, Country
ORDER BY Quantity;


