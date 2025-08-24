@echo off
echo ==================================================
echo 🚀 Launching FraNchEstYN batch test...
echo ==================================================

REM --- Set Rscript path if not in PATH ---
set R_SCRIPT="C:\Program Files\R\R-4.4.2\bin\Rscript.exe"

REM --- Navigate to tests folder (adjust if needed) ---
cd /d C:\GitHub\FraNchEstYN\tests

REM --- Run your test script with arguments (jobs range) ---
%R_SCRIPT% test_batch.R 1 3

echo ==================================================
echo ✅ Done! Check console output above.
echo ==================================================
pause
