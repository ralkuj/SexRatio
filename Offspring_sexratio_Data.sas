*********************************************************************************;
*	File name:	Offspring_sexratio_Data.sas
*	Author:		Ralf Kuja-Halkola
*	Date:		2020-01-29
*	Purpose:	Create data for analysis of sex of offspring. ;
*********************************************************************************;

/*	Created data:
	1. Cohort included
	2. all pairs of offspring to full siblings, born 1932-1984.
*/

*	Access data tables, database refered to below as 'SOURCEDATA'. Code removed (one row) due to data secrecy. ;
*** REMOVED CODE ***;

*********************************************;
*	Import data ;
* 	Parents ;
DATA indexgeneration;
SET  SOURCEDATA.v_individual;
IF  FODELSEDATUM>193200;
IF  FODELSEDATUM<200000;
IF  FODELSEGRUPP=0;
KON=KON-1;
KEEP LOPNR KON FODELSEDATUM;
RUN;
*	Children ;
DATA children;
SET  SOURCEDATA.v_individual;
IF  FODELSEDATUM>194200;
IF FODELSEGRUPP=0;
LOPNRBARN = LOPNR;
konbarn = KON-1;
dobbarn = FODELSEDATUM;
KEEP LOPNRBARN konbarn dobbarn;
RUN;
*	Grandparents ;
DATA mor;
SET  SOURCEDATA.v_individual;
IF  FODELSEDATUM<198500;
IF FODELSEGRUPP=0;
LOPNRMOR = LOPNR;
dobmor = FODELSEDATUM;
KEEP LOPNRMOR dobmor;
RUN;
DATA far;
SET  SOURCEDATA.v_individual;
IF  FODELSEDATUM<198500;
IF FODELSEGRUPP=0;
LOPNRFAR= LOPNR;
dobfar= FODELSEDATUM;
KEEP LOPNRFAR dobfar;
RUN;
*	Link ;
DATA linkparent;
SET  SOURCEDATA.v_parent;
RUN;

DATA linkchild;
SET  SOURCEDATA.v_child;
RUN;

*	Spouse link;
DATA linkspousemale;
SET  linkparent;
LOPNRBARN =LOPNR;
LOPNR = LOPNRFAR ;
LOPNRSPOUSE = LOPNRMOR ;
KEEP LOPNR LOPNRBARN LOPNRSPOUSE ;
RUN;
DATA linkspousefemale;
SET  linkparent;
LOPNRBARN =LOPNR;
LOPNR = LOPNRMOR ;
LOPNRSPOUSE = LOPNRFAR ;
KEEP LOPNR LOPNRBARN LOPNRSPOUSE ;
RUN;
DATA linkspouse;
SET  linkspousemale linkspousefemale;
RUN;
PROC SORT DATA=linkspouse; BY LOPNR; RUN;

*********************************************;

*********************************************;
*	Merge datasets;
PROC SORT DATA=indexgeneration; BY LOPNR; RUN; 
PROC SORT DATA=linkparent; BY LOPNR; RUN; 
PROC SORT DATA=linkchild; BY LOPNR; RUN; 
DATA family;
MERGE indexgeneration linkparent linkchild;
BY LOPNR;
IF FODELSEDATUM ^=. ;
IF LOPNRFAR ^=. ;
IF LOPNRMOR ^=. ;
RUN;
*	Number of kids ;
PROC SQL;
SELECT COUNT(UNIQUE(lopnrbarn)) AS nrbarn FROM family; QUIT;
*	N barn = 5065596;

*	Add info for relatives ;
PROC SORT DATA=family; BY LOPNRMOR; RUN; 
PROC SORT DATA=mor; BY LOPNRMOR; RUN; 
DATA family;
MERGE family mor;
BY LOPNRMOR;
IF FODELSEDATUM ^=. ;
IF dobmor ^=. ;
IF LOPNRFAR ^=. ;
IF LOPNRMOR ^=. ;
RUN;
PROC SORT DATA=family; BY LOPNRFAR; RUN; 
PROC SORT DATA=far; BY LOPNRFAR; RUN; 
DATA family;
MERGE family far;
BY LOPNRFAR;
IF FODELSEDATUM ^=. ;
IF dobfar ^=. ;
IF LOPNRFAR ^=. ;
IF LOPNRMOR ^=. ;
RUN;
*	Number of kids ;
PROC SQL;
SELECT COUNT(UNIQUE(lopnrbarn)) AS nrbarn FROM family; QUIT;
*	N barn = 4788566;


*	Children... Exclude childless index-generation individuals ;
PROC SORT DATA=family; BY LOPNRBARN; RUN; 
PROC SORT DATA=children; BY LOPNRBARN; RUN; 
DATA family;
MERGE family children;
BY LOPNRBARN;
IF FODELSEDATUM ^=. ;
IF dobbarn ^=. ;
IF LOPNR ^=. ;
IF LOPNRFAR ^=. ;
IF LOPNRMOR ^=. ;
IF LOPNRBARN ^=. ;
RUN;
*	Number of kids ;
PROC SQL;
SELECT COUNT(UNIQUE(lopnrbarn)) AS nrbarn FROM family; QUIT;
*	N barn = 4753269;

*	Spouse ID ;
PROC SORT DATA=family; BY LOPNR LOPNRBARN; RUN; 
PROC SORT DATA=linkspouse; BY LOPNR LOPNRBARN; RUN; 
DATA family;
MERGE family linkspouse;
BY LOPNR LOPNRBARN;
IF FODELSEDATUM ^=. ;
RUN;
PROC SORT DATA=family; BY LOPNR; RUN; 
*********************************************;

*********************************************;
*	Create family indicators and birth orders (bnr) for children ;
PROC SORT DATA=family; BY LOPNR dobbarn; RUN;
DATA family ;
SET  family ;
RETAIN bnr 1 ;
BY LOPNR  ;
IF first.LOPNR  THEN bnr=1 ;
ELSE bnr+1 ;
RUN;
*	bnr by mor (mother) ;
PROC SORT DATA=family; BY LOPNRMOR FODELSEDATUM ; RUN;
DATA family ;
SET  family ;
RETAIN bnrmor 1;
BY LOPNRMOR FODELSEDATUM;
IF FIRST.LOPNRMOR THEN bnrmor=1;
ELSE IF FIRST.FODELSEDATUM THEN bnrmor+1;
RUN;
*	bnr by far (father) ;
PROC SORT DATA=family; BY LOPNRFAR FODELSEDATUM; RUN;
DATA family;
SET  family;
RETAIN bnrfar 1;
BY LOPNRFAR FODELSEDATUM ;
IF FIRST.LOPNRFAR THEN bnrfar=1;
ELSE IF FIRST.FODELSEDATUM THEN bnrfar+1;
RUN;

*********************************************;

*********************************************;
*	Twin indicator by index individual ;
PROC SQL;
CREATE TABLE family AS
	SELECT *, COUNT(UNIQUE LOPNRBARN )  AS nrbornsamedate 
	FROM family
	GROUP BY LOPNR, dobbarn;
QUIT;
*********************************************;


*********************************************;
*	Create an outer join, by mor or far (mother and father) ;
DATA family2;
SET  family;
RUN;
PROC DATASETS LIB=work;
MODIFY family2;
RENAME LOPNR=LOPNR2 KON=KON2 FODELSEDATUM=FODELSEDATUM2 LOPNRFAR=LOPNRFAR2 LOPNRMOR=LOPNRMOR2 LOPNRBARN=LOPNRBARN2 LOPNRSPOUSE=LOPNRSPOUSE2
		dobmor=dobmor2 dobfar=dobfar2 konbarn=konbarn2 dobbarn=dobbarn2 bnr=bnr2 bnrmor=bnrmor2 bnrfar=bnrfar2 nrbornsamedate=nrbornsamedate2;
LABEL  LOPNR2=LOPNR2 KON2=KON2 FODELSEDATUM2=FODELSEDATUM2 LOPNRFAR2=LOPNRFAR2 LOPNRMOR2=LOPNRMOR2 LOPNRBARN2=LOPNRBARN2 LOPNRSPOUSE2=LOPNRSPOUSE2
		dobmor2=dobmor2 dobfar2=dobfar2 konbarn2=konbarn2 dobbarn2=dobbarn2 bnr2=bnr2 bnrmor2=bnrmor2 bnrfar2=bnrfar2 nrbornsamedate2=nrbornsamedate2;
RUN;
PROC SQL;
CREATE TABLE pairsmor AS
	SELECT * FROM family AS A
	FULL OUTER JOIN family2 AS B
	ON A.LOPNRMOR = B.LOPNRMOR2
	WHERE A.LOPNR ^= B.LOPNR2 AND A.LOPNRBARN ^= B.LOPNRBARN2 ; 
QUIT;
PROC SORT DATA=pairsmor; BY LOPNRMOR; RUN;
PROC SQL;
CREATE TABLE pairsfar AS
	SELECT * FROM family AS A
	FULL OUTER JOIN family2 AS B
	ON  A.LOPNRFAR = B.LOPNRFAR2
	WHERE A.LOPNR ^= B.LOPNR2 AND A.LOPNRBARN ^= B.LOPNRBARN2 ; 
QUIT;
PROC SORT DATA=pairsfar; BY LOPNRFAR; RUN;

*	Add together and remove potential duplications ;
DATA pairstemp ;
SET  pairsmor pairsfar ;
RUN;
PROC SORT DATA=pairstemp OUT=pairs NODUPKEY ;
BY LOPNR LOPNR2 LOPNRBARN LOPNRBARN2;
RUN;

*	Remove twins in parental generation ;
DATA pairsmor ;
SET  pairsmor ;
IF FODELSEDATUM ^= FODELSEDATUM2 ;
RUN;
DATA pairsfar ;
SET  pairsfar ;
IF FODELSEDATUM ^= FODELSEDATUM2 ;
RUN;
DATA pairs ;
SET  pairs ;
IF FODELSEDATUM ^= FODELSEDATUM2 ;
RUN;

*	Create parental sibtype indicator ;
DATA pairsmor;
SET  pairsmor;
IF (LOPNRMOR = LOPNRMOR2 AND LOPNRFAR = LOPNRFAR2) THEN sibtype=0;
ELSE IF (LOPNRMOR = LOPNRMOR2 AND LOPNRFAR ^= LOPNRFAR2) THEN sibtype=1;
ELSE IF (LOPNRMOR ^= LOPNRMOR2 AND LOPNRFAR = LOPNRFAR2) THEN sibtype=2;
ELSE sibtype=3;
RUN;
DATA pairsfar;
SET  pairsfar;
IF (LOPNRMOR = LOPNRMOR2 AND LOPNRFAR = LOPNRFAR2) THEN sibtype=0;
ELSE IF (LOPNRMOR = LOPNRMOR2 AND LOPNRFAR ^= LOPNRFAR2) THEN sibtype=1;
ELSE IF (LOPNRMOR ^= LOPNRMOR2 AND LOPNRFAR = LOPNRFAR2) THEN sibtype=2;
ELSE sibtype=3;
RUN;
DATA pairs;
SET  pairs;
IF (LOPNRMOR = LOPNRMOR2 AND LOPNRFAR = LOPNRFAR2) THEN sibtype=0;
ELSE IF (LOPNRMOR = LOPNRMOR2 AND LOPNRFAR ^= LOPNRFAR2) THEN sibtype=1;
ELSE IF (LOPNRMOR ^= LOPNRMOR2 AND LOPNRFAR = LOPNRFAR2) THEN sibtype=2;
ELSE sibtype=3;
RUN;



/*
PROC FREQ DATA=pairs; TABLE sibtype maxbnr maxbnr2 ; RUN;
*/

***************************************;
*	Export data ;

*	Fix manageable dataset, remove LOPNRs (except parent and spouse) and order stuff neatly ;
DATA pairsexport ;
RETAIN 	konbarn konbarn2 sibtype
		LOPNR  LOPNRSPOUSE  KON  FODELSEDATUM  LOPNRBARN  dobbarn  bnr nrbornsamedate LOPNRMOR LOPNRFAR 
		LOPNR2 LOPNRSPOUSE2 KON2 FODELSEDATUM2 LOPNRBARN2 dobbarn2 bnr2 nrbornsamedate2 LOPNRMOR2 LOPNRFAR2;
SET  pairs ;
KEEP 	LOPNR  LOPNRSPOUSE  KON  FODELSEDATUM  LOPNRBARN  konbarn  dobbarn  bnr nrbornsamedate LOPNRMOR LOPNRFAR 
		LOPNR2 LOPNRSPOUSE2 KON2 FODELSEDATUM2 LOPNRBARN2 konbarn2 dobbarn2 bnr2 nrbornsamedate2 LOPNRMOR2 LOPNRFAR2 sibtype;
RUN;

*	Export the cohort data ;
PROC EXPORT DATA= WORK.family 
            OUTFILE= "P:\Neurodev\Neurodev_Research\Data\Temp for sexratio\Familiality_of_offspring_sex_cohort_20190116.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

*	Export cousin-pair data ;
PROC EXPORT DATA= WORK.pairsexport 
            OUTFILE= "P:\Neurodev\Neurodev_Research\Data\Temp for sexratio\Familiality_of_offspring_sex_cousinpairs_20190116.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

***************************************;

*********************************************************************************;
*********************************************************************************;
***************************** END OF FILE ***************************************;
*********************************************************************************;
*********************************************************************************;
