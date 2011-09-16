/* Script to set up data base structure, 
i.e. create all tables
*/


-- create table tiles
CREATE TABLE tiles (tileid INT NOT NULL,
   xmin DOUBLE, 
   ymin DOUBLE,
   xmax DOUBLE,
   ymax DOUBLE,
   PRIMARY KEY (tileid));

-- create table runconfig
CREATE TABLE runconfig (runconfigid INT NOT NULL,
   sres VARCHAR(10),
   gcm VARCHAR(50),
   year VARCHAR(15),
   PRIMARY KEY (runconfigid));

-- create table points
CREATE TABLE points (pid INT NOT NULL,
   lon DOUBLE,
   lat DOUBLE,
   tileid INT,
   PRIMARY KEY (pid),
   FOREIGN KEY (tileid) REFERENCES tiles(tileid));

-- create table runs
CREATE TABLE runs (id INT NOT NULL,
   runconfigid INT,
   pid INT,
   location VARCHAR(50),
   exitstatus VARCHAR(100),
   proccessedby VARCHAR(15),
   timestarted DATETIME,
   timefinished DATETIME,
   PRIMARY KEY (id),
   FOREIGN KEY (runconfigid) REFERENCES runconfig(runconfigid),
   FOREIGN KEY (pid) REFERENCES points(pid));
   
