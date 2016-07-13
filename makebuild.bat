@echo off
@echo.@echo off >build.bat
@echo.rem a homemade batch file >>build.bat
for %%x in (\hamcalc64\prog\*.bas) do call addfile.bat %%x