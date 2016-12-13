/*Avg Order Lines per customer */

CREATE VIEW qry_view2 AS

SELECT Lines$.Order_ID, CAST(count(Lines$.SKU_ID) AS FLOAT) as SKUs, Cust$.First_Name, Cust$.Last_Name
FROM Lines$ INNER JOIN OrderToCust$
ON Lines$.Order_ID = OrderToCust$.Order_ID
INNER JOIN Cust$
ON OrderToCust$.Cust_ID = Cust$.Cust_ID
GROUP BY Lines$.Order_ID, Cust$.First_Name, Cust$.Last_Name

SELECT avg(SKUs) as avg_skus, first_name, last_name
FROM qry_view2
GROUP BY First_Name, Last_Name

select * from qry_view2