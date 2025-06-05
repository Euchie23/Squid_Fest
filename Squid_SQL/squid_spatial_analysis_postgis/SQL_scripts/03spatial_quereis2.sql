-- files were imported directly to pgAdmin from CSVs into tables so below is to update the geom columns of the respective tables

---Squid_catch_table
UPDATE squid_catch
SET geom = ST_SetSRID(ST_MakePoint(longitude, latitude), 4326)::geography;

---distance_to_land_table
UPDATE distance_land
SET geom = ST_SetSRID(ST_MakePoint(longitude, latitude), 4326)::geography;


-- Find the closest distance_land point for each squid_catch sample
SELECT 
    sc.sample_id,
    sc.catch_weight_g,
    sc.catch_length_mm,
    sc.maturity_level,
    dl.area,
    dl.distance_from_Argentina_km,
    dl.distance_from_Falkland_Islands_km,
    ST_Distance(sc.geom, dl.geom)/1000 AS distance_m_km
FROM squid_catch sc
JOIN distance_land dl
  ON ST_DWithin(sc.geom, dl.geom, 50000)
ORDER BY sc.sample_id, distance_m_km;

---“How close are squid samples to the nearest land area, and do proximity patterns relate to squid characteristics like maturity or size?”
---Proximity Influence on Maturity or Size 
--You may find that squid caught farther from land are more mature or larger — suggesting a growth or migration pattern.
--Or the opposite — squid may spawn or mature closer to certain coastal areas.
--Environmental or Ecological Clustering
--If certain land areas (e.g., near Falklands vs. Argentina) consistently have different squid maturity/weight, this might hint at ecological zones.



---1. Which area codes consistently yield the most mature squid?
---This will rank area numbers by the average maturity level. Higher values indicate areas that yield more mature squid consistently.
SELECT catch_year, area, ROUND(AVG(maturity_level), 2) AS avg_maturity
FROM squid_catch
GROUP BY catch_year, area
ORDER BY avg_maturity DESC;
---squids with the highest maturity were mainly found in the years 2020 and 2021 mainly in areas 65 (2020) and 47(2021)
---“Identifying areas with higher maturity tells us where squid are more developed — these might be better for harvest. We can focus research or fishing effort here while avoiding immature areas.”

---2. What is the distribution of maturity levels in the sample?
---This identifies whether the sample is biased toward any particular maturity stage.
SELECT maturity_level, COUNT(*) AS count
FROM squid_catch
GROUP BY maturity_level
ORDER BY maturity_level;
---The most squids caught were at maturity level 3 indicating sampling bias. since most of the squids got to the catch location after they had already pass level 2. This explain the low catch of squids at maturity levels 1 and 2.
---“A balanced distribution means good sampling. If skewed (e.g., mostly immature), it may reflect seasonal bias or incomplete data — important for interpreting other results.”

---3. Is there a significant difference in squid length across maturity levels?
---This shows if different maturity levels correlate with squid size, and how much variability exists at each level.
SELECT maturity_level, 
       ROUND(AVG(catch_length_mm), 2) AS avg_length,
       ROUND(STDDEV(catch_length_mm), 2) AS std_dev
FROM squid_catch
GROUP BY maturity_level
ORDER BY maturity_level;
--- A positive correlation up to level 4 between maturity level and squid length.
---Level 1 has the lowest variability in size (std dev = 4.58), likely due to:
--Smaller sample size.
--More uniform early growth.
--From level 2 onward, variability increases significantly (20–23 mm), indicating more individual variation in growth rates as squids mature.
---“If more mature squid are consistently longer, this confirms growth with age, and supports using size as a non-invasive maturity proxy in the field.


---4. How do squid characteristics vary by distance from land (e.g. Argentina)?
--- This is to examine how weight, length, or maturity vary with distance from Argentina and the Falkland Islands, by linking tables.
----“Squid may be larger or more mature further offshore. That can inform zoning, seasonal access, and gear type by distance.”

SELECT 
  CASE 
    WHEN d.distance_from_argentina_km < 500 THEN '<500km'
    WHEN d.distance_from_argentina_km BETWEEN 500 AND 550 THEN '500-550km'
    ELSE '>550km'
  END AS argentina_distance_range,

  CASE 
    WHEN d.distance_from_falkland_islands_km < 100 THEN '<100km'
    WHEN d.distance_from_falkland_islands_km BETWEEN 100 AND 150 THEN '100-150km'
    ELSE '>150km'
  END AS falklands_distance_range,

  ROUND(AVG(s.catch_weight_g)::numeric, 2) AS avg_weight_grams,
  ROUND(AVG(s.catch_length_mm)::numeric, 2) AS avg_length_mm,
  ROUND(AVG(s.maturity_level)::numeric, 2) AS avg_maturity

FROM squid_catch s
JOIN distance_land d
  ON s.area = d.area
  AND s.catch_year::text = d.catch_year::text

GROUP BY 
	argentina_distance_range,falklands_distance_range
ORDER BY 
	argentina_distance_range,falklands_distance_range;

---The biggest and longest squids seem to be found at least 500-550km from Argentina and more than 150km from the Falkland Islands where as the smallest squids in terms of length, size and maturity seem to be found less than 500 km from Argentina (closer to shore) but more than 150 km from the Falkland islands. This might be as a result of the differences in dietary compositions and avoidance of predation. Smaller squids mainly depend on upwelling that happens close to the coast in addition to small crustaceans as compared to bigger squids that hunt for fish in the open ocean and are less vulnerable to predation as compared to smaller squids.


---5. Are there clusters of squid catches within certain areas? (K-means)
---
SELECT latitude, longitude
FROM squid_catch
WHERE latitude IS NOT NULL AND longitude IS NOT NULL;
---“Clusters indicate preferred squid habitats or oceanographic zones. This helps with marine spatial planning and targeted monitoring.”



---6. Are certain areas more contaminated with metals?
---This compares average metal levels by area to identify hotspots for contamination.
---“We can flag pollution hotspots, which may align with ports, rivers, or human activity. Good for recommending mitigation or prioritizing studies.”

DROP TABLE IF EXISTS contaminant_levels_table;
CREATE TABLE contaminant_levels_table (
    area TEXT,
    geom geometry(Point, 4326),
    pollutant TEXT,
    pollutant_type TEXT,
    concentration NUMERIC
); 

INSERT INTO contaminant_levels_table
SELECT c.area, ST_SetSRID(s.geom::geometry, 4326) AS geom,'Metal_A' AS pollutant, 'metal' AS pollutant_type, ROUND(c.Metal_A, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, ST_SetSRID(s.geom::geometry, 4326) AS geom, 'Metal_B', 'metal', ROUND(c.Metal_B, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, ST_SetSRID(s.geom::geometry, 4326) AS geom, 'Metal_C', 'metal', ROUND(c.Metal_C, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, ST_SetSRID(s.geom::geometry, 4326) AS geom, 'Metal_D', 'metal', ROUND(c.Metal_D, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, ST_SetSRID(s.geom::geometry, 4326) AS geom, 'Metal_E', 'metal', ROUND(c.Metal_E, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, ST_SetSRID(s.geom::geometry, 4326) AS geom, 'Metal_F', 'metal', ROUND(c.Metal_F, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, ST_SetSRID(s.geom::geometry, 4326) AS geom, 'Metal_G', 'metal', ROUND(c.Metal_G, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, ST_SetSRID(s.geom::geometry, 4326) AS geom, 'Metal_H', 'metal', ROUND(c.Metal_H, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, ST_SetSRID(s.geom::geometry, 4326) AS geom, 'Metal_I', 'metal', ROUND(c.Metal_I, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, ST_SetSRID(s.geom::geometry, 4326) AS geom, 'Metal_J', 'metal', ROUND(c.Metal_J, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, ST_SetSRID(s.geom::geometry, 4326) AS geom, 'Organic_A', 'organic', ROUND(c.Organic_A, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area,ST_SetSRID(s.geom::geometry, 4326) AS geom, 'Organic_B', 'organic', ROUND(c.Organic_B, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, ST_SetSRID(s.geom::geometry, 4326) AS geom, 'Organic_C', 'organic', ROUND(c.Organic_C, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, ST_SetSRID(s.geom::geometry, 4326) AS geom, 'Organic_D', 'organic', ROUND(c.Organic_D, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area
	
ORDER BY concentration DESC;
---

-- Add a spatial index for better QGIS performance
CREATE INDEX contaminant_levels_geom_idx ON contaminant_levels_table USING GIST (geom);

-- Register geometry columns for PostGIS metadata (optional but recommended)
SELECT Populate_Geometry_Columns('contaminant_levels_table'::regclass);


DROP VIEW IF EXISTS summarized_pollution_by_area;
CREATE TABLE summarized_pollution_by_area (
    area TEXT,
    geom geometry(Point, 4326),
    avg_metal NUMERIC,
	avg_organic NUMERIC
); 

INSERT INTO summarized_pollution_by_area 
SELECT
  c.area,
  s.geom::geometry,
  AVG(CASE WHEN pollutant_type = 'metal' THEN concentration ELSE NULL END) AS avg_metal,
  AVG(CASE WHEN pollutant_type = 'organic' THEN concentration ELSE NULL END) AS avg_organic
FROM contaminant_levels_table c
JOIN squid_catch s ON c.area = s.area
GROUP BY c.area, s.geom::geometry;






SELECT c.area, s.geom,'Metal_A' AS pollutant, 'metal' AS pollutant_type, ROUND(c.Metal_A, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, s.geom, 'Metal_B', 'metal', ROUND(c.Metal_B, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, s.geom, 'Metal_C', 'metal', ROUND(c.Metal_C, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, s.geom, 'Metal_D', 'metal', ROUND(c.Metal_D, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, s.geom, 'Metal_E', 'metal', ROUND(c.Metal_E, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, s.geom, 'Metal_F', 'metal', ROUND(c.Metal_F, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, s.geom, 'Metal_G', 'metal', ROUND(c.Metal_G, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, s.geom, 'Metal_H', 'metal', ROUND(c.Metal_H, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, s.geom, 'Metal_I', 'metal', ROUND(c.Metal_I, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, s.geom, 'Metal_J', 'metal', ROUND(c.Metal_J, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, s.geom, 'Organic_A', 'organic', ROUND(c.Organic_A, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, s.geom, 'Organic_B', 'organic', ROUND(c.Organic_B, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, s.geom, 'Organic_C', 'organic', ROUND(c.Organic_C, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area

UNION ALL

SELECT c.area, s.geom, 'Organic_D', 'organic', ROUND(c.Organic_D, 2) AS concentration
FROM concentrations c
JOIN squid_catch s ON c.area = s.area
	
ORDER BY concentration DESC;
---




