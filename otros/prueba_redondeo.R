floor_date(
  now(), 
  unit = "12 hours")

floor_date(
  as_datetime("2025-12-02 12:38:55 -03"), 
  unit = "6 hours")

floor_date(
  as_datetime("2025-12-02 20:38:55 -03"), 
  unit = "6 hours")
