-- Drop tables if exist (for clean rerun)
DROP TABLE IF EXISTS squid_catch;
DROP TABLE IF EXISTS concentrations;
DROP TABLE IF EXISTS distance_land;

-- Create squid_catch table
CREATE TABLE squid_catch (
    sample_id CHAR(5) PRIMARY KEY,
	area CHAR(2),
	gender VARCHAR(6),
	catch_month INT,
	catch_day INT,
	catch_year INT,
    longitude DOUBLE PRECISION NOT NULL,
    latitude DOUBLE PRECISION NOT NULL,
	water_temperature NUMERIC,
    catch_length_mm NUMERIC,
    catch_weight_g NUMERIC,
    maturity_level NUMERIC,
    geom GEOGRAPHY(Point, 4326) -- Store lat/lon as geography points
);


-- Create concentrations table in mg/kg
CREATE TABLE concentrations (
    sample_id CHAR(5) PRIMARY KEY,
	area CHAR(2),
	Organic_A NUMERIC,
    Organic_B NUMERIC,
	Organic_C NUMERIC,
	Organic_D NUMERIC,
 	Metal_A NUMERIC,
    Metal_B NUMERIC,
	Metal_C NUMERIC,
	Metal_D NUMERIC,
	Metal_E NUMERIC,
	Metal_F NUMERIC,
	Metal_G NUMERIC,
	Metal_H NUMERIC,
	Metal_I NUMERIC,
	Metal_J NUMERIC
);


-- Create distance_land table
CREATE TABLE distance_land (
    area CHAR(2) PRIMARY KEY,
	catch_year CHAR(4),
    longitude DOUBLE PRECISION NOT NULL,
    latitude DOUBLE PRECISION NOT NULL,
    distance_from_argentina_km NUMERIC,
	distance_from_falkland_islands_km NUMERIC,
    geom GEOGRAPHY(Point, 4326)
);