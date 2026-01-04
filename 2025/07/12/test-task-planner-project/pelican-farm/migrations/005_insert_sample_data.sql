-- Insert sample pelicans
INSERT OR IGNORE INTO pelicans (name, species, age, weight, health_status, arrival_date, notes) VALUES
('Charlie', 'Brown Pelican', 3, 2.5, 'healthy', '2024-01-15', 'Rescued with minor wing injury, fully recovered'),
('Bella', 'American White Pelican', 5, 4.2, 'healthy', '2023-08-22', 'Adult female, excellent health'),
('Rocky', 'Brown Pelican', 2, 2.1, 'under_observation', '2024-11-30', 'Young male, monitoring weight gain'),
('Luna', 'American White Pelican', 7, 4.8, 'healthy', '2022-03-10', 'Senior female, regular health checks'),
('Finn', 'Brown Pelican', 1, 1.8, 'recovering', '2024-12-01', 'Juvenile, recovering from fishing line injury');

-- Insert sample feeding records
INSERT OR IGNORE INTO feeding_records (pelican_id, food_type, amount_kg, feeding_time, notes) VALUES
(1, 'fish', 0.8, '2024-12-07 08:00:00', 'Morning feeding - ate well'),
(1, 'fish', 0.9, '2024-12-07 18:00:00', 'Evening feeding - normal appetite'),
(2, 'fish', 1.2, '2024-12-07 08:00:00', 'Large adult portion'),
(2, 'fish', 1.1, '2024-12-07 18:00:00', 'Slightly reduced evening portion'),
(3, 'fish', 0.6, '2024-12-07 08:00:00', 'Small portion for young bird'),
(4, 'fish', 1.3, '2024-12-07 08:00:00', 'Senior bird - good appetite'),
(5, 'fish', 0.5, '2024-12-07 08:00:00', 'Recovering juvenile - small portions');

-- Insert sample health checks
INSERT OR IGNORE INTO health_checks (pelican_id, check_date, weight, temperature, notes, veterinarian) VALUES
(1, '2024-12-01', 2.5, 40.2, 'Routine checkup - all vitals normal', 'Dr. Sarah Johnson'),
(2, '2024-11-28', 4.2, 40.1, 'Annual health assessment - excellent condition', 'Dr. Michael Chen'),
(3, '2024-12-03', 2.1, 40.4, 'Weight monitoring - slight increase from last week', 'Dr. Sarah Johnson'),
(4, '2024-11-25', 4.8, 40.0, 'Senior bird checkup - arthritis managed well', 'Dr. Emily Rodriguez'),
(5, '2024-12-02', 1.8, 40.5, 'Post-injury assessment - healing well', 'Dr. Michael Chen');

-- Insert sample feeding schedules
INSERT OR IGNORE INTO feeding_schedules (pelican_id, scheduled_time, food_type, amount_kg, completed) VALUES
(1, '2024-12-08 08:00:00', 'fish', 0.8, FALSE),
(1, '2024-12-08 18:00:00', 'fish', 0.9, FALSE),
(2, '2024-12-08 08:00:00', 'fish', 1.2, FALSE),
(2, '2024-12-08 18:00:00', 'fish', 1.1, FALSE),
(3, '2024-12-08 08:00:00', 'fish', 0.6, FALSE),
(3, '2024-12-08 14:00:00', 'fish', 0.5, FALSE),
(3, '2024-12-08 18:00:00', 'fish', 0.6, FALSE),
(4, '2024-12-08 08:00:00', 'fish', 1.3, FALSE),
(4, '2024-12-08 18:00:00', 'fish', 1.2, FALSE),
(5, '2024-12-08 08:00:00', 'fish', 0.5, FALSE),
(5, '2024-12-08 14:00:00', 'fish', 0.4, FALSE),
(5, '2024-12-08 18:00:00', 'fish', 0.5, FALSE);
