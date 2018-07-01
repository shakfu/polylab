DROP TABLE IF EXISTS Amounts;
CREATE TABLE Amounts (
  NUTR varchar(4) DEFAULT NULL,
  FOOD varchar(5) DEFAULT NULL,
  amt float DEFAULT NULL
) ;

INSERT INTO Amounts VALUES ('A','BEEF',60),('C','BEEF',20),('B1','BEEF',10),('B2','BEEF',15),('NA','BEEF',938),('CAL','BEEF',295),('A','CHK',8),('C','CHK',0),('B1','CHK',20),('B2','CHK',20),('NA','CHK',2180),('CAL','CHK',770),('A','FISH',8),('C','FISH',10),('B1','FISH',15),('B2','FISH',10),('NA','FISH',945),('CAL','FISH',440),('A','HAM',40),('C','HAM',40),('B1','HAM',35),('B2','HAM',10),('NA','HAM',278),('CAL','HAM',430),('A','MCH',15),('C','MCH',35),('B1','MCH',15),('B2','MCH',15),('NA','MCH',1182),('CAL','MCH',315),('A','MTL',70),('C','MTL',30),('B1','MTL',15),('B2','MTL',15),('NA','MTL',896),('CAL','MTL',400),('A','SPG',25),('C','SPG',50),('B1','SPG',25),('B2','SPG',15),('NA','SPG',1329),('CAL','SPG',370),('A','TUR',60),('C','TUR',20),('B1','TUR',15),('B2','TUR',10),('NA','TUR',1397),('CAL','TUR',450);

CREATE TABLE Foods (
  FOOD varchar(5) DEFAULT NULL,
  cost float DEFAULT NULL,
  f_min float DEFAULT NULL,
  f_max float DEFAULT NULL
);

INSERT INTO Foods VALUES ('BEEF',3.19,2,10),('CHK',2.59,2,10),('FISH',2.29,2,10),('HAM',2.89,2,10),('MCH',1.89,2,10),('MTL',1.99,2,10),('SPG',1.99,2,10),('TUR',2.49,2,10);


DROP TABLE IF EXISTS Nutrients;
CREATE TABLE Nutrients (
  NUTR varchar(4) DEFAULT NULL,
  n_min float DEFAULT NULL,
  n_max float DEFAULT NULL
) ;


INSERT INTO Nutrients VALUES ('A',700,20000),('C',700,20000),('B1',700,20000),('B2',700,20000),('NA',0,50000),('CAL',16000,24000);
