SELECT flavor, sum(profit) as total_profit
FROM qry_big_table
WHERE order_date BETWEEN '2016-08-03' AND '2016-08-06'
GROUP BY flavor
ORDER BY flavor