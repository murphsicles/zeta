@echo off
echo ========================================
echo    🏭 OPENCLAW MISSION CONTROL
echo ========================================
echo.
echo Starting Mission Control Dashboard...
echo.

REM Check if Node.js is installed
where node >nul 2>nul
if %errorlevel% neq 0 (
    echo ❌ ERROR: Node.js is not installed or not in PATH
    echo Please install Node.js from https://nodejs.org/
    pause
    exit /b 1
)

REM Check if in correct directory
if not exist "package.json" (
    echo ❌ ERROR: Not in mission-control directory
    echo Please navigate to: C:\Users\mummy\.openclaw\workspace\mission-control
    pause
    exit /b 1
)

REM Install dependencies if needed
if not exist "node_modules" (
    echo 📦 Installing dependencies...
    call npm install
    if %errorlevel% neq 0 (
        echo ❌ ERROR: Failed to install dependencies
        pause
        exit /b 1
    )
    echo ✅ Dependencies installed
)

REM Start the server
echo.
echo 🚀 Starting Mission Control server...
echo 📡 Dashboard: http://localhost:3000
echo 🔌 API: http://localhost:3000/api/status
echo 📊 WebSocket: ws://localhost:3000
echo.
echo Press Ctrl+C to stop the server
echo.

call npm start

if %errorlevel% neq 0 (
    echo.
    echo ❌ ERROR: Server failed to start
    echo Possible issues:
    echo 1. Port 3000 is already in use
    echo 2. Node.js modules corrupted
    echo 3. Missing dependencies
    echo.
    echo Try: netstat -ano | findstr :3000
    echo Then: taskkill /PID [PID] /F
    pause
)