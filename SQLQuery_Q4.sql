/* Avg Revenue per line for customer whose last name begine with a Vowel */


SELECT avg(ItemMaster$.Item_Price * Lines$.Qty) AS Revenue
FROM Lines$ INNER JOIN ItemMaster$
ON Lines$.SKU_ID = ItemMaster$.SKU_ID
INNER JOIN OrderToCust$
ON OrderToCust$.Order_ID = Lines$.Order_ID
INNER JOIN Cust$
ON OrderToCust$.Cust_ID = Cust$.Cust_ID

WHERE Cust$.Last_Name LIKE '[aeiouy]%'
