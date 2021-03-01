-- ---- OPERATIONAL LAYER -----

SHOW VARIABLES LIKE "secure_file_priv";

-- Create database schema 'airbnb' to store data for Toronto Airbnbs
DROP SCHEMA IF EXISTS airbnb;
CREATE SCHEMA airbnb;

-- Set airbnb schema as default
USE airbnb;

-- Create table 'hosts' in airbnb schema
DROP TABLE IF EXISTS hosts;
CREATE TABLE hosts (
    host_id INT NOT NULL,
    PRIMARY KEY(host_id),
    host_name VARCHAR(100),
    host_since DATE,
    host_is_superhost CHAR(5),
    host_listings_count INT
);

-- Import host.csv data into hosts table
LOAD DATA INFILE 'C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\hosts_austin.csv' 
INTO TABLE hosts
FIELDS TERMINATED BY ',' ENCLOSED BY '"'
LINES TERMINATED BY '\r\n' 
IGNORE 1 LINES 
(host_id, host_name, @host_since, host_is_superhost, @host_listings_count)
SET
host_since = nullif(@host_since, ''),
host_listings_count = nullif(@host_listings_count,'');


-- Create table 'listings' in airbnb schema
DROP TABLE IF EXISTS listings;
CREATE TABLE listings (
    listing_id INT NOT NULL,
    PRIMARY KEY (listing_id),
    host_id INT NOT NULL,
    listing_name VARCHAR(255),
    listing_description VARCHAR(10500),
    property_type VARCHAR(50),
    accommodates VARCHAR(50),
    bathrooms VARCHAR(25),
    bedrooms INT,
    beds INT,
    price INT,
    minimum_nights INT,
    maximum_nights INT,
    number_of_reviews INT,
    review_scores_rating INT,
    FOREIGN KEY(host_id) REFERENCES airbnb.hosts(host_id));
    
    
-- Import listing_austin.csv data into listings table
LOAD DATA INFILE 'C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\listings_austin.csv' 
INTO TABLE listings
FIELDS TERMINATED BY ',' ENCLOSED BY '"'
LINES TERMINATED BY '\r\n' 
IGNORE 1 LINES 
(listing_id,host_id,listing_name,listing_description,property_type,accommodates,bathrooms,@bedrooms,@beds,@price,@minimum_nights,@maximum_nights,number_of_reviews,@review_scores_rating)
SET
bedrooms = nullif(@bedrooms, ''),
beds = nullif(@beds, ''),
price = nullif(@price, ''),
minimum_nights = nullif(@minimum_nights, ''),
maximum_nights = nullif(@maximum_nights, ''),
review_scores_rating = nullif(@review_scores_rating , '');

-- Create table 'calendar' in airbnb schema
DROP TABLE IF EXISTS calendar;
CREATE TABLE calendar (
    listing_id INT NOT NULL,
    available_date DATE,
    available VARCHAR(5),
    price INT,
    minimum_nights INT,
    maximum_nights INT,
    FOREIGN KEY (listing_id) REFERENCES listings(listing_id)
);

-- Import calendar_austin.csv data into calendar table
LOAD DATA INFILE 'C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\calendar_austin.csv' 
INTO TABLE calendar
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\r\n' 
IGNORE 1 LINES 
(listing_id, available_date, available, @price, @minimum_nights, @maximum_nights)
SET
price = nullif(@price, ''),
minimum_nights = nullif(@minimum_nights, ''),
maximum_nights = nullif(@maximum_nights, '');


-- ###############Stored Procedures#################   ###############Stored Procedures#################  ###############Stored Procedures##################

-- Create procedure for 'Available listings' data warehouse
DROP PROCEDURE IF EXISTS Get_available_listings;

DELIMITER $$

CREATE PROCEDURE Get_available_listings()
BEGIN

	DROP TABLE IF EXISTS available_listings;

	CREATE TABLE available_listings AS
	SELECT 
	   listings.listing_id,
	   listings.listing_name,
	   calendar.available_date,
       listings.property_type,
       listings.accommodates,
       listings.beds,
       calendar.minimum_nights,
       calendar.maximum_nights,
       calendar.price,
       ROUND(calendar.price/listings.accommodates)  AS price_per_person,
	   hosts.host_id,
       hosts.host_name
	FROM
		listings
	INNER JOIN
		calendar USING (listing_id)
	INNER JOIN
		hosts USING (host_id)
	WHERE available = 't'
	ORDER BY available_date;
    
    ALTER TABLE available_listings
    MODIFY price_per_person FLOAT;

END $$
DELIMITER ;

Call Get_available_listings();

-- View Data Warehouse
SELECT * FROM available_listings;

-- Create  for 'host_ratings' data warehouse

DROP PROCEDURE IF EXISTS Get_host_ratings;

DELIMITER $$

CREATE PROCEDURE Get_host_ratings()
BEGIN

DROP TABLE IF EXISTS host_ratings;
CREATE TABLE host_ratings
SELECT host_id,
	host_name,
	host_since,
	host_is_superhost,
	number_of_reviews,
	host_listings_count,
	ROUND(avg(review_scores_rating),1) AS host_rating
	FROM listings
	INNER JOIN hosts
	USING(host_id)
	GROUP BY host_id;
    
    ALTER TABLE host_ratings
    MODIFY host_rating FLOAT;
    
END $$

DELIMITER ;

Call Get_host_ratings();

-- View Data Warehouse
SELECT * FROM host_ratings;

-- ###############TRIGGERS##################    ###############TRIGGERS##################  ###############TRIGGERS##################

DROP TABLE IF EXISTS listings_audit;    

CREATE TABLE listings_audit (
	listing_id INT NOT NULL PRIMARY KEY,
    host_id INT NOT NULL,
    listing_name VARCHAR(255),
    listing_description VARCHAR(10500),
    property_type VARCHAR(50),
    accommodates VARCHAR(50),
    bathrooms VARCHAR(25),
    bedrooms INT,
    beds INT,
    price INT,
    minimum_nights INT,
    maximum_nights INT,
    number_of_reviews INT,
    review_scores_rating INT,
    updatedAt TIMESTAMP DEFAULT NOW(),
    FOREIGN KEY(listing_id) REFERENCES airbnb.listings(listing_id));

DROP TRIGGER IF EXISTS before_host_info_update;

DELIMITER $$

CREATE TRIGGER before_host_info_update
AFTER UPDATE        
ON listings FOR EACH ROW
BEGIN
    INSERT INTO listings_audit(listing_id,host_id,listing_name,listing_description,property_type,accommodates,bathrooms,bedrooms,beds,price,minimum_nights,maximum_nights,number_of_reviews,review_scores_rating)
    VALUES(OLD.listing_id,OLD.host_id,OLD.listing_name,OLD.listing_description,OLD.property_type,OLD.accommodates,OLD.bathrooms,OLD.bedrooms,OLD.beds,OLD.price,OLD.minimum_nights,OLD.maximum_nights,OLD.number_of_reviews,OLD.review_scores_rating);
END$$

DELIMITER ;

-- TEST TRIGGER BY UPDATING ROW IN LISTING TABLES

UPDATE listings
SET 
	number_of_reviews = 28,
    review_scores_rating = 78
WHERE
    listing_id = 2265;
 

-- ###############VIEWS##################    ###############VIEWS##################  ###############VIEWS##################

-- Create VIEW for summary statistics of property type in relation to price
DROP VIEW IF EXISTS property_type_stats; 

CREATE VIEW `property_type_stats` AS
SELECT property_type AS 'Property Type',
		COUNT(property_type) AS 'No of Listings',
	   ROUND(Min(price)) AS 'Min Price (Total)',
       ROUND(Max(price)) AS 'Max Price (Total)',
       ROUND(AVG(price)) AS 'Avg Price (Total)',
       ROUND( MIN(price/accommodates)) AS 'Min Price (Person)',
       ROUND(MAX(price/accommodates)) AS 'Max Price (Person)',
       ROUND(AVG(price/accommodates)) AS 'Avg Price (Person)'
FROM available_listings
GROUP BY property_type
ORDER BY property_type;

SELECT * FROM property_type_stats;


-- Hosts with host rating score less than 50 | Materialized View with Event

CREATE TABLE messages (
    id INT PRIMARY KEY AUTO_INCREMENT,
    message VARCHAR(255) NOT NULL,
    created_at DATETIME NOT NULL
);

SET GLOBAL event_scheduler = ON;

-- Create a VIEW through an EVENT
DELIMITER $$

CREATE EVENT host_rating_warning_refresh
ON SCHEDULE EVERY 1 MINUTE
STARTS CURRENT_TIMESTAMP
ENDS CURRENT_TIMESTAMP + INTERVAL 5 minute -- 5 minutes set for test purposes
ON COMPLETION PRESERVE
DO
	BEGIN
		DROP VIEW IF EXISTS host_rating_warning;
        
		CREATE VIEW host_rating_warning AS
		SELECT *
		FROM host_ratings
		WHERE host_rating < 70 AND number_of_reviews > 0 -- criteria to filter put bad performers
		ORDER BY host_rating DESC;

		INSERT INTO messages(message,created_at)
		VALUES('Event was generated. Host_rating_warning view was updated.',NOW());
	
	END$$
DELIMITER ;

SELECT * FROM host_rating_warning;

-- Create view for top 5 hosts through event which will run every 30 days for 5 months
DROP EVENT IF EXISTS top_5_hosts_monthly;

DELIMITER $$

CREATE EVENT top_5_hosts_monthly
ON SCHEDULE EVERY 30 DAY
STARTS CURRENT_TIMESTAMP
ENDS CURRENT_TIMESTAMP + INTERVAL 5 MONTH-- 5 minutes set for test purposes
ON COMPLETION PRESERVE
DO
BEGIN
DROP VIEW IF EXISTS top_5_superhosts;

CREATE VIEW `top_5_superhosts` AS
SELECT 
host_id AS 'Host ID',
host_name AS 'Host Name',
host_since AS 'Host Since',
number_of_reviews AS 'Number of Reviews',
host_listings_count AS 'No of listings by Host',
host_rating AS 'Host Rating'
FROM host_ratings
WHERE host_is_superhost = 't'
ORDER BY
host_rating DESC, number_of_reviews DESC, host_listings_count DESC, host_since DESC
LIMIT 5;

END$$
DELIMITER ;

-- Call the view
Select * FROM top_5_superhosts;









