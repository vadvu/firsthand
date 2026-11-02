@echo off
setlocal DisableDelayedExpansion
set "FH_RSCRIPT="
for %%R in (Rscript.exe) do set "FH_RSCRIPT=%%~$PATH:R"
if defined FH_RSCRIPT goto run
for %%K in ("HKCU\SOFTWARE\R-core\R" "HKLM\SOFTWARE\R-core\R" "HKCU\SOFTWARE\R-core\R64" "HKLM\SOFTWARE\R-core\R64") do (
  for /f "tokens=2,*" %%A in ('reg query %%K /v InstallPath 2^>nul') do (
    if /i "%%A"=="REG_SZ" if exist "%%B\bin\Rscript.exe" if not defined FH_RSCRIPT set "FH_RSCRIPT=%%B\bin\Rscript.exe"
  )
)
if defined FH_RSCRIPT goto run
echo Rscript not found. Install R from https://cran.r-project.org/ 1>&2
echo Or add the R bin directory to PATH. 1>&2
exit /b 1
:run
"%FH_RSCRIPT%" "%~dp0firsthand.R" %*
exit /b %errorlevel%
