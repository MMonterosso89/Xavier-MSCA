# Lab #1 Query from A Single Table

/*--------------------------------------------------------------------------------------
Instructions:

You will need to import the data for Chicago Salary in order to complete this assignment. 

The Chicago Salary table should be called 'ChicagoSalary' and have 4 fields plus some AI primary key 
Name 
PositionTitle 
Department 
Salary 

You should have a total of 3663 rows. 


Answer each question as best as possible.  
Show your work if you need to take multiple step to answer a problem. 
Partial answers will count.
--------------------------------------------------------------------------------------*/


/* 
Q1.
Write the query to COUNT the number of Records in the Salary table 
*/

/* Q1. Query*/

SELECT COUNT(*) FROM ChicagoSalary;




/* 
Q2.
Write a query to display the number of unique names
*/

/* Q2. Query */

SELECT COUNT(DISTINCT name)
	FROM ChicagoSalary; 




/* 
Q3.
Write a query to display the only the name and positiontitle of those with a  name that contains the text 'Spangler' in it
*/

/* Q3. Query*/


SELECT name, positiontitle
	FROM ChicagoSalary WHERE name LIKE '%SPANGLER%';



/* 
Q4. Write a query to display the name and position of the person who has the lowest Half year Salary (salary/2) in the the AVIATION department. Give the calculated half-year-salary variable a new name
*/

/* Q4. Query */

SELECT name, positiontitle, MIN(salary) / 2 AS HalfSalary FROM ChicagoSalary
	WHERE department = 'aviation';
		




/* 
Q5. Write a query to display all the names and salaries of everyone in the WATER MGMNT department
who make more than 150K in salary, order the output by Salary descending
*/

/* Q5. Query */ 

SELECT name, salary FROM ChicagoSalary
	WHERE department = 'WATER MGMNT' AND salary > 150000
		ORDER BY salary DESC;




/* 
Q6. Display the total salary of everyone in the entire Chicago salary table
*/

/* Q6. Query */

SELECT SUM(salary) FROM ChicagoSalary;




/* 
Q7. Display the Name, Department, Salary (to the nearest whole number) 
	of any employee who has a salary of 60000 or more and their name begins with 'Aaron'
*/

/* Q7. Query */

SELECT name, department, ROUND(salary) FROM ChicagoSalary
	WHERE salary >= 60000 AND name REGEXP '^Aaron';




/* 
Q8. Which Employee has the highest salary? List the employee's name, department and salary. 
	
*/

/*Q8. Query */

SELECT name, department , salary FROM ChicagoSalary
	WHERE salary = (SELECT MAX(salary) from ChicagoSalary);




/* 
Q9. If we wanted to stored Social Security Number (SSN) for each employee what kind of data type should we use for the column?
	   What type of data type should we use for Phone Number?
	   Explain your reasoning for both questions.
*/ 

/* Write your response to this question directly on the answer sheet  */

#SSNs are 9 digits none of which are after a decimal point, so int would likely work best. This is because
#it allows for 10 digits with only 4 bytes of usage. This is 1 less byte than numeric with precision 9!

#Phone numbers can be up to 11 characters (1 country code + 3 area + 7 number), so bigint must be used for
#international databases, but national databases can use int, using half the memory!



/* Q10. Create a new user, 'Joel' who has access only from 'localhost' and has the password 'info674'. 
		Grant the user 'Joel' a superuser account (all privileges) with access to all databases.
		Show the grants for the new user and report them on the answer sheet
		
*/
CREATE USER 'joel'@'localhost' IDENTIFIED BY 'info674';

GRANT ALL PRIVILEGES ON *.* TO 'joel'@'localhost';

SHOW GRANTS FOR 'joel'@'%';



/* Bonus Q11.  Look on dev.mysql.com or another resource and learn how to grant only SELECT and UPDATE privileges. 
			   Write commands that delete the user 'Joel,' create a new user named 'Greg' who has access from any host,
			   can access any database and grant only SELECT and UPDATE privileges to the user 'Greg'
			   Show the grants for the new user and report them on the answer sheet
*/

/* Q11. Query */

DROP USER 'joel';
 
CREATE USER 'greg'@'%' IDENTIFIED BY 'dank';

SHOW GRANTS FOR 'greg'@'%';    ### Used GUI for this






