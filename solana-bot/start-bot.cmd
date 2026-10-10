@echo off
rem Double-click to run the bot with REAL market data (paper mode unless you enable live).
rem Keep this window open while the bot runs. Closing it (or pressing Ctrl+C) stops the bot.
cd /d "%~dp0"
title Solana Research Trader
if not exist node_modules (
  echo Installing dependencies, this takes a minute the first time...
  call npm.cmd install || goto :fail
)
echo Building the dashboard...
call npm.cmd run build || goto :fail
echo.
echo The dashboard opens at http://127.0.0.1:8787 in a few seconds.
echo Keep this window open. To stop the bot, close this window.
start "" /b cmd /c "timeout /t 8 /nobreak >/dev/null && start http://127.0.0.1:8787"
call npm.cmd start
goto :eof
:fail
echo.
echo Something went wrong. Copy the messages above and ask for help.
pause
