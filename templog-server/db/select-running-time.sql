WITH pairs AS (
	SELECT ((julianday(late.taken_at) - julianday(early.taken_at)) * 24 * 60 * 60) as diff, early.taken_at as stop, late.taken_at as restart, early.*, late.* FROM measurement as early
	INNER JOIN measurement as late ON late.measurement_id = early.measurement_id + 1
), 
long_pauses AS (
	SELECT * from pairs
	WHERE diff > 120
), 
time_online AS (
	SELECT ((julianday(stopped.stop) - julianday(started.restart)) * 24) AS time_running, started.restart AS started, stopped.stop AS stopped, * FROM long_pauses AS stopped
	INNER JOIN long_pauses AS started ON started.restart = (SELECT max(restart) FROM long_pauses AS l WHERE l.restart < stopped.stop)
)
SELECT * FROM time_online
ORDER BY started DESC
