SELECT  * FROM ec_data; 

--Insight 1: 10 lojas com os maiores faturamentos em pedidos
--Insight 1.1: Volume total de items de venda para as 10 lojas com maior faturamento em pedidos
select StoreId, 
	sum(Quantity) as Quantity, 
	sum(PurchaseValue) as PurchaseValue   
from ec_data ed 
where PurchaseValue > 0 
group by StoreId 
order by sum(PurchaseValue) desc 
LIMIT 10;



select STRFTIME('%Y/%m', InvoiceDate ) as monthYear, 
	sum(PurchaseValue)/4334 as PurchaseValue  
	from ec_data ed  
WHERE PurchaseValue > 0 
	and CustomerID is not 'NA'
GROUP by monthYear;




