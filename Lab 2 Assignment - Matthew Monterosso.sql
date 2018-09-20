/*
Respond to each of the following questions using a query.
The script required to create the associated database is on canvas

Introduction:
As a part of a university course on service learning, students are required to
complete community service hours as tutors for high school students.
Student performance is summarized in the associated database table.
Comhours is the attribute for the number of community service hours completed

Assignment:
Write queries that answer the following questions.
No need for screenshots of the output-- I will run these queries using your own
database to confirm that they work! :)
*/

-- 1.	Display the names, id and major information of all students in the class.

SELECT lname, sid, major FROM student;

-- 2.	Display a list of all the student majors in the class.

SELECT lname, major FROM student;

-- 3.	List the various majors represented in the class (each major only once)

SELECT DISTINCT major from student;

-- 4.	List the names, major and grade level of all math majors.

SELECT lname, major, gradelevel FROM student
	WHERE major = 'Math';

-- 5.	List the names of students who are math majors at the graduate (GR) level.

SELECT lname from student
	WHERE GradeLevel = 'GR'
    AND major = 'Math';

-- 6.	Who are the students that are math or accounting majors?

SELECT lname from student
	WHERE major = 'Math'
    OR major = 'Accounting';


-- 7.	Who are the students that have no interest in history?

SELECT lname from student
	WHERE major != 'History';


-- 8.	List all information of students with student ID’s that are between 200 and 300

SELECT * from student
	WHERE sid BETWEEN 200 AND 300;

-- 9.	Display the students who have a gradelevel ending with ‘R’

SELECT lname from student
	WHERE gradelevel regexp 'R$';

-- 10.	List the name, grade and major of all accounting students in alphabetical order.

SELECT lname, gradelevel, major from student
	WHERE major = 'accounting'
    ORDER BY lname;
    

-- 11.	Display all information about students that are in freshman, sophomores or seniors by first ordering them alphabetically by major and then by gradelevel in descending order.

SELECT * from student
	WHERE GradeLevel IN ('FR','SO','SN')
    ORDER BY major;
    

-- 12.	How many students are in this database?

SELECT COUNT(sid) from student;

-- 13.	How many different majors are represented in this class?

SELECT COUNT(DISTINCT major) from student;

-- 14.	What is the average GPA of students in the class?

SELECT AVG(GPA) from student;

-- 15.	What is the lowest GPA in this class?
SELECT MIN(GPA) from student;

-- 16.	Who has the lowest GPA in this class?

SELECT lname from student
	WHERE GPA = (SELECT MIN(GPA) from student);

-- 17.	How many community engagement hours have been logged by this class?

SELECT SUM(Comhours) from student;

-- 18.	Participation is assessed on a scale of 100. Fifteen community engagement hours is the maximum number of hours that can count towards participation. Compute the participation grade for each student in this class.

SELECT lname, (Comhours / 15) * 100 AS ParticipationGrade from student;

-- 19.	Who has the highest participation grade in the class?

SELECT lname from student
	WHERE comhours = (SELECT MAX(comhours) from student);

-- 20.	Who has logged more than the average number of community engagement hours in the class?

SELECT lname from student
	WHERE comhours > (SELECT AVG(comhours) from student);