@echo off
rem Double-click to PRACTICE on the simulated market (no internet data, no real money).
rem Practice data is kept separately in data-mock\ and never mixes with real-data trading.
cd /d "%~dp0"
title Solana Research Trader (practice market)
set MARKET_DATA=mock
if not exist node_modules (
  echo Installing dependencies, this takes a minute the first time...
  call npm.cmd install || goto :fail
)
echo Building the dashboard...
call npm.cmd run build || goto :fail
echo.
echo The practice dashboard opens at http://127.0.0.1:8787 in a few seconds.
echo Keep this window open. To stop, close this window.
start "" /b cmd /c "timeout /t 8 /nobreak >/dev/null && start http://127.0.0.1:8787"
call npm.cmd start
goto :eof
:fail
echo.
echo Something went wrong. Copy the messages above and ask for help.
pause
