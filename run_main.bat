@echo off
title URCs to DSS

set rexe=resources\R-4.0.3\bin\Rscript.exe

set rscript=main.R

%rexe% %rscript% 

echo Finished running %rscript%

pause