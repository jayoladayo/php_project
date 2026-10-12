@echo off
cd /d "%~dp0"
.venv\Scripts\python -m forexbot run --mode demo
pause
