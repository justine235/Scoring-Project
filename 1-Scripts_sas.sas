*=======================================================================

Projet Scoring

=======================================================================;
* PLAN 

	1) Chemin + librairies 

	2) Importation des data

	3) Pré-traitement des variables 

	4) Analyses des variables 
			4.1) Listing des variables 
			4.2) Taux de couverture des variables ;



*=== 1) Chemin + librairies ;
%let data = C:\Users\mottierm\Documents\Perso\Scoring ;
libname lib "&data.\data" ; run;


*=== 2) Importation des data ;
proc import datafile ="&data\data\ResiliationContratAuto.txt"
            out=lib.tab1
            dbms=dlm 
			replace;
            delimiter='|';
            getnames=yes;
run;


*=== 3) Pré-traitement des variables ;
* On va renommer les variables pour identifier plus rapidement leur provenance et les différencier de celles que l'on va créer ;
%macro rename(table=, sortie=, suffixe=) ;
   proc contents data=&table. out=content noprint ;
   run ;
   data _null_;
      set content ;
      call symput('var'||compress(_n_),compress(name)) ;
      call symput('obs',compress(_n_)) ;
   run;

   data &sortie. ;
      set &table.
		(rename=(
            %do i=1 %to &obs. ;
                &&var&i = %sysfunc(left(&suffixe))&&var&i
            %end;) 
		);
   run ;

%mend rename ;

%rename(table=lib.tab1, sortie=lib.tab2, suffixe=I_); * I_ comme input;

*== 4) Analyses des variables ;
* 4.1) Listing des variables + première corrélation des variables ;
proc corr data = lib.tab2 out=lib.corr ;
	var _numeric_ ;
run ; quit ;

data lib.corr; 
set lib.corr; 
if _type_ in ("mean", "std", "n") then delete;
drop _type_;
run;

ods excel file="&data\Output\analyses.xlsx" style=sasweb
options(autofilter="all" 	frozen_headers="yes" contents="yes" index="yes"	sheet_name="Proc contents"); 	
	proc contents data = lib.tab2 out=content; run;
	proc print data=lib.corr; run;
ods excel close;

* La variable I_NIVBM et I_CRM sont très corrélée => suppression de I_NIVBM;


* 4.2) Taux de couverture des variables ;
* 1e méthode : macro programme;
%macro CalculNmiss(tab_entree,tab_sortie);
	data inter ;
	 set &tab_entree.;
	 array cha _character_;
	 array num _numeric_;

	do over cha;
	 if missing(cha) then do;
	 vnm=vname(cha);
	 output;
	 end;
	end;

	do over num;
	 if missing(num) then do;
	 vnm=vname(num);
	 output;
	 end;
	end;
	run;
	 	 
	proc sql;
	create table &tab_sortie. as
	select distinct vnm, count(*) as nmiss
	from inter
	group by vnm;
	quit;

%Mend CalculNmiss;

%CalculNmiss (lib.tab2,lib.nmiss);
 
* 2e méthode : proc IML ;
proc iml;
use lib.tab2;
read all var _NUM_ into x[colname=nNames]; 
n = countn(x,"col");
nmiss = countmiss(x,"col");
 
read all var _CHAR_ into x[colname=cNames]; 
close lib.tab2;
c = countn(x,"col");
cmiss = countmiss(x,"col");
 
/* combine results for num and char into a single table */
Names = cNames || nNames;
rNames = {"    Missing", "Not Missing"};
cnt = (cmiss // c) || (nmiss // n);
print cnt[r=rNames c=Names label=""];

* 3e méthode : format;
/* create a format to group missing and nonmissing */
proc format;
 value $missfmt ' '='Missing' other='Not Missing';
 value  missfmt  . ='Missing' other='Not Missing';
run;


proc freq data=lib.tab2; 
format _CHAR_ $missfmt.; /* apply format for the duration of this PROC */
tables _CHAR_ / missing missprint nocum nopercent;
format _NUMERIC_ missfmt.;
tables _NUMERIC_ / missing missprint nocum nopercent;
run;


* Exports des résultats ;
ods excel file="&data\Output\analyses2.xlsx" style=sasweb
	options(autofilter="all" 	frozen_headers="yes" contents="yes" index="yes"	sheet_name="Proc contents"); 	
	proc print data = lib.nmiss; run;
ods excel close;


* 4.3) Comptages ;
ods pdf file="&data\Output\comptages_char.pdf";
	proc freq data=lib.tab2; 
	table _CHAR_; 
	run;
ods pdf close;

ods pdf file="&data\Output\comptages_num.pdf";
	proc freq data=lib.tab2; 
	table _NUMERIC_; 
	run;
ods pdf close;


*== 5) Traitement des variables ;
* 5.1) Traitement des dates; 
* 5.1.a) Modification des formats / informats ;
proc contents data=lib.tab2; run;

data lib.tab3;
set lib.tab2;
attrib I_DTDBUCON I_DTEFTMVT I_DTOBTPDC I_DTPMRMCI I_ANCCLI I_DT_NAI  format = mmddyy10.; 
attrib I_DTDBUCON I_DTEFTMVT I_DTOBTPDC I_DTPMRMCI I_ANCCLI I_DT_NAI  informat = mmddyy10.; 
run;

* 5.2) Traitement des variable dates : création de nouvelles variables ;
data lib.tab3;
set lib.tab3;
if I_DTDBUCON ne . then o_age_contrat 			= intck("year",I_DTDBUCON,I_DTEFTMVT);
if I_DTOBTPDC ne . then o_age_permis 			= intck("year",I_DTOBTPDC,today());
if I_DTPMRMCI ne . then o_age_circulation_veh	= intck("year",I_DTPMRMCI,I_DTEFTMVT);
if I_DT_NAI ne .   then o_age_indi				= intck("year",I_DT_NAI,today());
if I_ANCCLI  ne .  then o_duree_anciennete		= intck("year",I_ANCCLI, I_DTEFTMVT); 
run;

proc univariate data=lib.tab3; var o_age_contrat o_age_last_mvt o_age_permis o_age_circulation_veh o_age_indi; run;

* 5.3) Traitement des autres variables ;
* Nous allons regrouper certaines modalités ainsi que certains NA dans une catégorie autre 
quand cela est possible. Sinon nous procéderons à une imputation
+ regroupement des modalités les plus faibles ;
data lib.tab4;
set lib.tab3;

*== Regroupement des modalités dans une catégories autres ;
* Variable I_CD_FML  ;
if I_CD_FML  in ("1","3","5","6","7","A","B","G","H","K","L","M","N","P","") then I_CD_FML ="Z";

* Variable I_NOTAREFF ;
if I_NOTAREFF in ("1","6","8","B090","B125","B350","B999","BL125","BL350","","BL999","B062","B050","B085") then I_NOTAREFF = "AU"; 

* Variable I_PUI_TRE ;
if I_PUI_TRE in ("F","Z","") then I_PUI_TRE = "I";
if I_PUI_TRE in ("H","A","B") then I_PUI_TRE = "Z";

* Variable Nombre de sinistres totale ;
if I_S_0_N = . then I_S_0_N = 0;
if I_S_1_N = . then I_S_1_N = 0;
if I_S_2_N = . then I_S_2_N = 0;
if I_S_3_N = . then I_S_3_N = 0;

if I_S_0_O = . then I_S_0_O = 0;
if I_S_1_O = . then I_S_1_O = 0;
if I_S_2_O = . then I_S_2_O = 0;
if I_S_3_O = . then I_S_3_O = 0;

o_s_n = sum(I_S_0_N, I_S_1_N, I_S_2_N, I_S_3_N); 
o_s_o = sum(I_S_0_O, I_S_1_O, I_S_2_O, I_S_3_O); 

* Variable I_CDSITFAM;
if I_CDSITFAM in ("S", "U", "V", "") then I_CDSITFAM = "Z";

* Variable I_REGION (+ remplacement par la modalité la plus fréquente);
if i_region in ("00","97","98","99","GU") then i_region ="AU";
if i_region = "" then i_region ="NE";

* Variable I_ETAT;
if I_ETAT in ("SU","TE") then I_ETAT = "AU";

* Variable I_MOTIFRSL;
if I_MOTIFRSL = . then I_MOTIFRSL = "a"; *Contrats actifs;

* Variable I_MTPAATTC ;
if I_MTPAATTC <= 1000 then I_MTPAATTC2 = "a";
if 1000 < I_MTPAATTC <= 3000 then I_MTPAATTC2 = "b";
if 3000 < I_MTPAATTC <= 5000 then I_MTPAATTC2 = "c";
if 5000 < I_MTPAATTC <= 10000 then I_MTPAATTC2 = "d";
if 10000 < I_MTPAATTC then I_MTPAATTC2 = "e";

* Variable Nombre de contrats résilités ;
if I_RESDI = . 		then I_RESDI = 0;
if I_RESIV = . 		then I_RESIV = 0;
if I_RESMH = . 		then I_RESMH = 0;
if I_RESSA = . 		then I_RESSA = 0;
if I_RESAU4R = .	then I_RESAU4R = 0;

o_nb_contrat_res = sum(I_RESDI,I_RESIV,I_RESMH,I_RESSA,I_RESAU4R);

* Variable I_MTPAAREF ;
if I_MTPAAREF <= 2031 then I_MTPAAREF2 = "a";
if 2031 < I_MTPAAREF <= 3796 then I_MTPAAREF2 = "b";
if 3796 < I_MTPAAREF <= 7615 then I_MTPAAREF2 = "c";
if 7615 < I_MTPAAREF then I_MTPAAREF2 = "d";

* Variable I_NBCTACT;
if I_NBCTACT = . then I_NBCTACT = 0;
if I_NBCTACT > 3 then I_NBCTACT = 3;

* Variable I_AU4R ;
if I_AU4R = . then I_AU4R = 0;
if I_AU4R > 2 then I_AU4R = 3;

* Variable I_MH;
if I_MH = . then I_MH = 0; 
if I_MH > 3 then I_MH = 4;

* Variable I_DI;
if I_DI = . then I_DI = 0;
if I_DI > 3 then I_DI = 4;

* Variable I_SA ;
if I_SA = . then I_SA = 0;
if I_SA > 3 then I_SA = 3;

* Variable I_IV;
if I_IV = . then I_IV = 0; 
if I_IV > 2 then I_IV = 3;

* Variable I_COEFCOMM ;
if I_COEFCOMM <= 93 then I_COEFCOMM2 = "a";
if 93 < I_COEFCOMM <= 99 then I_COEFCOMM2 = "b";
if 99 < I_COEFCOMM then I_COEFCOMM2 = "c";

* Variable I_CRM ;
if I_CRM < 64 then I_CRM2 = "a";
if 64 <= I_CRM < 77 then I_CRM2 = "b";
if 77 <= I_CRM < 93 then I_CRM2 = "c";
if 93 <= I_CRM then I_CRM2 = "d";

* Variable I_CDPRGES;
if I_CDPRGES ^= "4190A" then I_CDPRGES = "a";
if I_CDPRGES = "4190A" then I_CDPRGES = "b";

* Variable I_CDMCE;
if I_CDMCE = "P" then I_CDMCE = "a";
if I_CDMCE = "T" then I_CDMCE = "b";

*== Imputation par la modalité la plus fréquente : on a vérifié au préalable qu'il y'avait
un gab énorme entre la première et 2e modalités;

* Variable I_CD_SEX ;
if I_CD_SEX in ("", "M") then I_CD_SEX = "a";
if I_CD_SEX = "F" then I_CD_SEX = "b";

* Variable I_NBCTRES ;
if 2 < I_NBCTRES then I_NBCTRES = 3 ;

* Variable I_DEPT;
if I_DEPT = . then I_DEPT = 59;

* Variable I_CDMARVEH;
if I_CDMARVEH = "" then I_CDMARVEH = "RENAULT";

* Variable I_LBMDLVH;
if I_LBMDLVH = "" then I_LBMDLVH = "CLIO";

*== Variables que l'on ne garde pas ;
drop I_ANCCLI I_CD_CSP I_DT_NAI I_DTDBUCON I_DTPMRMCI I_DTOBTPDC I_DTEFTMVT I_CD_AGT I_AUTO4R I_NIVBM I_MOTIFRSL ;

*== Identification ;
o_id = _N_;

run;


* 5.4) Ajout de la fréquence d'apparition des numéros d'affiliés etc ;
%macro freq(in = , out = );
	proc freq data=lib.tab4 noprint; 
	table &in. / out = h1 (drop=percent); 
	run;

	data h1 ; 
	set h1; 
	rename count = &out.; 
	run;

	proc sort data=h1; 
	by &in.; 
	run;

	proc sort data=lib.tab4; 
	by &in.; 
	run; 

	data lib.tab4; 
	merge lib.tab4 h1; 
	by &in.; 
	run;

	data lib.tab4;
	set lib.tab4;
	drop &in.;
	run;

%mend;

%freq(in = I_NO_AFR, 	out = o_count_NO_AFR);
%freq(in = I_IDECON, 	out = o_count_IDECON);
%freq(in = I_NOCLIGES,	out = o_count_NOCLIGES);
%freq(in = I_NUMFOY, 	out = o_count_NUMFOY);

%CalculNmiss(lib.tab4, lib.Nmiss4);

*== 7) Recodage des modalités ;
* Certaines variables sont catégorielles mais lues comme numériques => besoin de recodification ;

*== 8) Outliers ;
proc univariate data=lib.tab2 robustscale plot;
var _NUMERIC_;
run;


%macro boxplot(table=, out=) ;

	proc contents data=&table. out=&out. noprint ; run ;
	data &out.; set &out. (where=(type=1)); keep name; run;

	data _null_;
	  set &out. ;
	  call symput('var'||compress(_n_),compress(name)) ;
	  call symput('obs',compress(_n_)) ;
	run;

	%do i=1 %to &obs. ;
		proc sgplot data=&table.;
		hbox &&var&i ;
		run;
	%end;

%mend;


ods pdf file="&data\Output\I.outliers.pdf" ;
	%boxplot(table = lib.tab2, out = content);
ods pdf close;



*== 7) Export de la table traitée ;
PROC EXPORT 
DATA=lib.tab4 
DBMS=TAB 
OUTFILE="&data\Data\table4.txt"
REPLACE;
run;

