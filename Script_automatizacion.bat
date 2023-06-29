@echo off 
rmdir /s /q C:\Users\usuario\Documents\Codigo_compras\Modelos_analitica_compras
cd C:\Users\usuario\Documents\Codigo_compras\
git clone https://github.com/cgrcr/Modelos_analitica_compras
"C:\Program Files\R\R-4.3.1\bin\Rscript.exe" C:\Users\usuario\Documents\Codigo_compras\Modelos_analitica_compras\CODIGO_UNICO_MODELO_2022_V2_Jun.R
pause

