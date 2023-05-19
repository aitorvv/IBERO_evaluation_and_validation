/*Primero se importan datos de csv*/
FILENAME REFFILE '/home/u47544240/TFM/datos_mortalidad_sdi/plots_IFN_iberopt_mortalidad_sdi.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=TFM.plots_IFN_iberopt_mortalidad_sdi;
	GETNAMES=YES;
RUN;

/*Establezco la ruta de la librería*/
libname TFM'/home/u47544240/TFM/';

/*Selecciono las variables a comparar*/
data datos;
set TFM.plots_IFN_iberopt_mortalidad_sdi;
y1=G_muerta_IFN2;
y2=G_muerta_IFN3;
run;

/*Comparo predichos y observados, y elimino los datos inexistentes*/
data datos;
set datos;
error=y2-y1;
/*if y2=0 or y1='.' or y2='.' then delete;*/
run;

/*Establezco la salida del pdf y activo los gráficos*/
options pagesize=max;
ods pdf file="/home/u47544240/TFM/outputs_mortalidad_sdi/G_muerta_mortalidad_sdi.pdf";
ods graphics on;

/*Calculo estadísticos básicos*/
proc means data=datos n mean std median q1 q3 min max stderr lclm uclm uss css;
var error;
run;

/*Hago una regresión*/
proc reg data=datos plots=all;
model y2=y1;
test y1=1;
run;

/*Hago una correlación*/
proc corr data=datos csscp outp=nada;
var y1 y2;
run;

/*Calculo el coeficiente de correlación de Lin*/
data concor;
set nada;
if _type_='CSSCP' and _name_='y1' then do;v1=y1;v3=y2;end;
if _type_='CSSCP' and _name_='y2' then v2=y2;
if _type_='MEAN' then do;v4=y1;v5=y2;end;
if _type_='N' then v6=y1;
output;
run;

data rc;
set concor;
syy=lag6(v1);sxx=lag5(v2);sxy=lag6(v3);y=lag4(v4);x=lag4(v5);n=lag2(v6);
rc=2*sxy/(sxx+syy+n*(x-y)**2);
if _type_='CORR' and _name_='y2' then output;
drop _type_--v6;
run;

proc print data=rc;
var rc;
run;

/*Cierro gráficos y pdf*/
ods graphics off;
ods pdf close;
quit;
