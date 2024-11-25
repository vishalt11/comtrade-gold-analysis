-- get current username
select user;

-- current database
select current_database();

CREATE TABLE Public."rsa_ken_GoldExport"(
refYear INT, 
reporterDesc VARCHAR(100), 
partnerDesc VARCHAR(100), 
cmdCode INT, 
qty FLOAT, 
altQty FLOAT, 
primaryValue FLOAT);

-- output the table
SELECT * FROM Public."rsa_ken_GoldExport";

-- copy the csv data into the postgresql table
COPY Public."rsa_ken_GoldExport"(refYear, reporterDesc, partnerDesc, cmdCode, qty, altQty, primaryValue)
FROM 'D:\UE Applied Sciences\Semester I\DS&B\Project\data\cleaned_data.csv'
DELIMITER ','
CSV HEADER;

-- count number of rows
SELECT COUNT(*) FROM Public."rsa_ken_GoldExport";

-- only kenya and south africa are reporters, hence two
SELECT COUNT(DISTINCT reporterDesc) FROM Public."rsa_ken_GoldExport";

-- get unique names in the respective column ordered descending
SELECT DISTINCT partnerDesc FROM Public."rsa_ken_GoldExport" ORDER BY partnerDesc ASC;

SELECT * FROM Public."rsa_ken_GoldExport" ORDER BY reporterDesc DESC, partnerDesc ASC;

-- ***************************
-- Example commands, wrangling
-- ***************************

SELECT * FROM Public."rsa_ken_GoldExport" WHERE reporterDesc = 'South Africa';

SELECT * FROM Public."rsa_ken_GoldExport" 
WHERE reporterDesc = 'South Africa' AND cmdCode = '7108' AND partnerDesc IN ('Areas, nes');

SELECT COUNT(*) FROM Public."rsa_ken_GoldExport" 
WHERE partnerDesc LIKE 'Can%' OR partnerDesc LIKE 'Cam%';

-- regex example, find partners ending in 'o'
-- https://www.atlassian.com/data/sql/how-regex-works-in-sql
-- https://www.rexegg.com/regex-quickstart.php
SELECT * FROM Public."rsa_ken_GoldExport"
WHERE partnerDesc ~ '.?o$' OR partnerDesc ~ '^W';

SELECT * FROM Public."rsa_ken_GoldExport"
WHERE partnerDesc LIKE '%o';

SELECT * FROM Public."rsa_ken_GoldExport"
WHERE partnerDesc LIKE '___t%';

SELECT * FROM Public."rsa_ken_GoldExport" 
WHERE primaryValue BETWEEN 10000 AND (SELECT ROUND(MAX(primaryValue)) FROM Public."rsa_ken_GoldExport")
ORDER BY primaryValue
LIMIT 20;

-- GROUP BY examples
SELECT reporterDesc, ROUND(SUM(primaryValue)) FROM Public."rsa_ken_GoldExport" 
GROUP BY reporterDesc
ORDER BY ROUND(SUM(primaryValue)) DESC;

-- WHERE filters individual rows before GROUPING
SELECT reporterDesc, partnerDesc, ROUND(SUM(primaryValue)) FROM Public."rsa_ken_GoldExport" 
WHERE partnerDesc != 'World' AND cmdCode = 7108
GROUP BY reporterDesc, partnerDesc
ORDER BY reporterDesc, ROUND(SUM(primaryValue)) ASC;

-- HAVING filters individual rows after GROUPING
-- the output of HAVING should be boolean
SELECT reporterDesc, partnerDesc, ROUND(SUM(primaryValue))
FROM Public."rsa_ken_GoldExport"
GROUP BY reporterDesc, partnerDesc
HAVING ROUND(SUM(primaryValue)) > 10000;

-- AS is used to rename aggregated results into more meaningful names
-- each reporter country have exported to these many unique countries
SELECT reporterDesc, COUNT(DISTINCT(partnerDesc)) AS num_partner_countries
FROM Public."rsa_ken_GoldExport"
WHERE partnerDesc != 'World' AND cmdCode = 7108
GROUP BY reporterDesc

-- Store query results into temporary table that you can extract from later 
-- It gets deleted after you close the session
-- You cannot override the temporary table without dropping it while in the same session
SELECT reporterDesc, partnerDesc, ROUND(SUM(primaryValue))
INTO temp_analysisTable FROM Public."rsa_ken_GoldExport"
WHERE partnerDesc != 'World' AND cmdCode = 7108
GROUP BY reporterDesc, partnerDesc
ORDER BY reporterDesc, ROUND(SUM(primaryValue)) ASC;

SELECT * FROM temp_analysisTable;

DROP TABLE temp_analysisTable;

SELECT * FROM Public."rsa_ken_GoldExport"
ORDER BY recordid;

ALTER TABLE Public."rsa_ken_GoldExport"
ADD unit_price FLOAT;

--ALTER TABLE Public."rsa_ken_GoldExport"
--ALTER COLUMN unit_price INT;

-- add column
ALTER TABLE Public."rsa_ken_GoldExport"
ADD COLUMN RecordID INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY (INCREMENT BY 1); 

-- update specific row value using column key (here primary key)
UPDATE Public."rsa_ken_GoldExport"
SET altqty = 52300100, unit_price = 10.5
WHERE recordid = 1;

-- write command to fill in unit_price

-- savepoint & rollback
