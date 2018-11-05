#8.	Write the SQL syntax to answer the following question: How many continent names end in the word ‘America’? (2.0 points)

SELECT COUNT(contname) from continent_table
	WHERE contname REGEXP 'America$';
    
SELECT COUNT(contname) from continent_table 
	WHERE contname LIKE '%America%';
    
#9.	What is the average population of countries whose name start and end with an a, and speak the language 'en'. (2.0 points)

SELECT AVG(population) FROM country_table
	WHERE cname REGEXP '^a$'
    AND cname REGEXP 'a$'
    AND language = 'en';

SELECT cname FROM country_table
	WHERE cname REGEXP '^a'
    AND cname REGEXP 'a$';
    
#10.	Write a query to show the number of unique languages spoken in the country table (2.0 points)


SELECT DISTINCT(COUNT(language)) FROM country_table;


#11.	Your friend can speak English (en), German (de), Spanish (es), 
#and French (fr). Write the SQL syntax to answer: How many countries 
#can she visit where she speaks the local language. 
#(recommendation: do not use the like operator here…) (2.0 points)

SELECT COUNT(language) FROM country_table
	WHERE language IN ('en','de','es','fr');
    
# Write a query that reports the SECOND highest population value in the country table. (2.0 Points)
SELECT population FROM country_table;

SELECT MAX(population) FROM country_table
	WHERE population < (SELECT MAX(population) FROM country_table);
    
#13.	Write a query that identifies a list of countries by code and name that
 #have a population greater than 5 million people AND are on a continent where
 #the highest point begins with the word “Mount” 
 
 SELECT ccode, cname FROM country_table
	JOIN continent_table ON continent_table.continentPK = country_table.continentFK
    WHERE population > 5000000 AND highpoint LIKE '%Mount%';
    
    
#14.	Write a query that reports a country’s name and the lowest point
#on the country’s continent for all countries that have a land area greater
 #than the average land area for all countries. (2.0 points)
 
 SELECT cname , lowpoint FROM country_table
	JOIN continent_table ON continent_table.continentPK = country_table.continentFK
    WHERE land > (SELECT AVG(land) FROM country_table);
