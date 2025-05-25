-- Drop tables if exist (for clean rerun)
DROP TABLE IF EXISTS squid_catch;
DROP TABLE IF EXISTS distance_land;

-- Create squid_catch table
CREATE TABLE squid_catch (
    sample_id SERIAL PRIMARY KEY,
    latitude DOUBLE PRECISION NOT NULL,
    longitude DOUBLE PRECISION NOT NULL,
    catch_weight NUMERIC,
    catch_size NUMERIC,
    catch_age NUMERIC,
    geom GEOGRAPHY(Point, 4326) -- Store lat/lon as geography points
);

-- Create distance_land table
CREATE TABLE distance_land (
    id SERIAL PRIMARY KEY,
    location_name TEXT,
    distance_km NUMERIC,
    geom GEOGRAPHY(Point, 4326)
);