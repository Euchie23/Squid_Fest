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
    sc.catch_weight,
    sc.catch_length,
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