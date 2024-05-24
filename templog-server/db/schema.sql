CREATE TABLE Measurement (
    measurement_id  INTEGER PRIMARY KEY,
    device          TEXT NOT NULL,
    temperature     DOUBLE NOT NULL,
    humidity        DOUBLE NOT NULL,
    taken_at        DATETIME NOT NULL
);

CREATE TABLE Assignment (
    assignment_id   INTEGER PRIMARY KEY,
    device          TEXT NOT NULL,
    name            TEXT NOT NULL,
    assigned_at     DATETIME NOT NULL
);
