@echo off 
set "ruta=C:\Users\humberto.perera\Desktop\modelo_compras"

rem Asegurarse de que la ruta exista antes de eliminarla
if exist "%ruta%" (
    rmdir /s /q "%ruta%"
)

rem Clonar el repositorio de Git solo si no se encontraron problemas al eliminar la ruta
if not exist "%ruta%" (
    git clone https://github.com/cgrcr/Modelos_analitica_compras "%ruta%"
)

rem Asegurarse de que Rscript esté instalado y la ruta sea correcta
"C:\Program Files\R\R-4.3.1\bin\Rscript.exe" "%ruta%\CODIGO_UNICO_MODELO_2022_V4_Jul.R"
pause
