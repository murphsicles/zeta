@echo off
echo ============================================
echo ZETA v0.5.0 GITHUB PUSH EXECUTION
echo ============================================
echo.
echo This script will push zetac-0.5.0 to GitHub
echo as the latest v0.5.0 release.
echo.

echo Step 1: Setting up GitHub remote...
echo Please enter your GitHub Personal Access Token:
set /p GITHUB_TOKEN=Token: 

if "%GITHUB_TOKEN%"=="" (
    echo ❌ No token provided. Exiting.
    pause
    exit /b 1
)

echo.
echo Step 2: Configuring git remote...
git remote remove origin 2>nul
git remote add origin https://%GITHUB_TOKEN%@github.com/murphsicles/zeta.git

if errorlevel 1 (
    echo ❌ Failed to configure git remote.
    pause
    exit /b 1
)

echo ✅ Git remote configured successfully.

echo.
echo Step 3: Pushing to main branch (as latest release)...
echo This will make v0.5.0 the default/latest release.
git push -f origin master:main

if errorlevel 1 (
    echo ❌ Failed to push to main branch.
    pause
    exit /b 1
)

echo ✅ Successfully pushed to main branch.

echo.
echo Step 4: Pushing v0.5.0 tag...
git push origin v0.5.0

if errorlevel 1 (
    echo ❌ Failed to push v0.5.0 tag.
    pause
    exit /b 1
)

echo ✅ Successfully pushed v0.5.0 tag.

echo.
echo ============================================
echo 🎉 ZETA v0.5.0 PUSHED SUCCESSFULLY!
echo ============================================
echo.
echo v0.5.0 (zetac-0.5.0) is now:
echo - On main branch (latest code)
echo - Tagged as v0.5.0
echo - Set as latest release on GitHub
echo.
echo Verify at: https://github.com/murphsicles/zeta/releases
echo.
echo The Dark Factory mission is complete.
pause