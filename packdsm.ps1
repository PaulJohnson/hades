# If gi-gtk libraries fail with "Failed to load shared library" messages
# then copy zlib1.dll from mingw64/bin to the GHC binaries folder.

$base = Get-Location
$signTool="C:\Program Files (x86)\Windows Kits\10\App Certification Kit\signtool.exe"
$certName="COMODO RSA Extended Validation Code Signing CA"

Write-host "Updating banana-ui-gtk..."
& git -C ..\banana-ui-gtk pull
if (-not $?) { exit }

Write-host "Updating reactive-banana..."
& git -C ..\reactive-banana pull
if (-not $?) { exit }

Write-host "Updating licensing..."
& git -C ..\licensing pull
if (-not $?) { exit }

Write-host "Updating blank-canvas..."
& git -C ..\blank-canvas pull
if (-not $?) { exit}

Write-host "Updating hades..."
Set-Location $base
& git pull
if (-not $?) { exit }

$version1 = & stack --silent query locals hades version
$version = $version1.Split([Environment]::NewLine) | Select -First 1

Write-host "Compiling DSM-$version ..."
& stack install dsm
if (-not $?) { exit }

Write-host "Signing executables..."
Copy-item "C:\Users\Paul Johnson\AppData\Roaming\local\bin\dsm.exe" -force -destination "Packages\Staging\dsm.exe"
Copy-item "C:\msys64\mingw64\bin\gspawn-win64-helper.exe" -force -destination "Packages\Staging\gspawn-win64-helper.exe"
Copy-item "C:\msys64\mingw64\bin\gspawn-win64-helper-console.exe" -force -destination "Packages\Staging\gspawn-win64-helper-console.exe"
& $signtool sign /v /t http://timestamp.comodoca.com/authenticode /i "$certName" /d "Diametric Safety Case Manager" /du "https://www.diametricsoftware.com/" Packages\Staging\dsm.exe Packages\Staging\gspawn-win64-helper.exe Packages\Staging\gspawn-win64-helper-console.exe
if (-not $?) { exit }

Write-host "Building MSI file..."
& candle -arch x64 "-dHadesVersion=$version" -o Packages\dsm.wixobj dsm\dsm.wxs
if (-not $?) { exit }
& light -cultures:en-US -ext WixUIExtension Packages\dsm.wixobj -out Packages\dsm-$version.msi
if (-not $?) { exit }

Write-host "Signing MSI..."
& $signtool sign /t http://timestamp.comodoca.com/authenticode /i "$certName" /d "Diametric Safety Case Manager" /du "https://www.diametricsoftware.com/" Packages\dsm-$version.msi
if (-not $?) { exit }

Write-host "Build completed successfully."
