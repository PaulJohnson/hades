# Installs and configures the software necessary to build the DSM on a Windows system.

# Before running this you will need to say "set-executionpolicy remotesigned" as Admin.

& mkdir Downloads

# Install Notepad++
Invoke-WebRequest -OutFile Downloads\npp.exe -Uri https://github.com/notepad-plus-plus/notepad-plus-plus/releases/download/v8.1.1/npp.8.1.1.Installer.exe
& .\Downloads\npp.exe

# Install Git.
Invoke-WebRequest -OutFile Downloads\git.exe -Uri https://github.com/git-for-windows/git/releases/download/v2.32.0.windows.1/Git-2.32.0-64-bit.exe
& .\Downloads\git.exe

# Install Windows SDK.
Invoke-WebRequest -OutFile Downloads\windows10-sdk.exe -Uri https://go.microsoft.com/fwlink/p/?linkid=2120843
& .\Downloads\windows10-sdk.exe /q

# Install SafeNet signing token handler.
Invoke-WebRequest -OutFile Downloads\safenet.zip -Uri https://www.ensured.com/files/Ensured-Safenet-Authentication-Tools-Windows-x64-10.8-R2.zip
Expand-Archive Downloads\safenet.zip
$MSIArguments = @(
    "/i"
    ('"{0}"' -f Downloads\safenet\safenet\*.msi)
    "/passive"
    "/norestart"
)
Start-Process "msiexec.exe" -ArgumentList $MSIArguments -Wait -NoNewWindow

# Install MinGW 
Invoke-WebRequest -OutFile Downloads\mingw-w64-install.exe -Uri https://github.com/msys2/msys2-installer/releases/download/2021-06-04/msys2-x86_64-20210604.exe
& .\Downloads\mingw-64-install.exe

& refreshenv

# Install Haskell stack
Invoke-webRequest -OutFile Downloads\stack.exe -Uri https://get.haskellstack.org/stable/windows-x86_64-installer.exe
& .\Downloads\stack.exe
& refreshenv

# Get SSH keys
& xcopy /E /H '\\TREETOP\Users\Paul Johnson\.ssh' 'C:\Users\Paul Johnson\.ssh'

# To Do:
# Add to User path: c:\msys64\bin;c:\msys64\usr\bin;c:\msys64\mingw64\bin

& stack exec -- pacman --noconfirm -S pkg-config mingw-w64-x86_64-gtk3 mingw-w64-x86_64-cairo mingw-w64-x86_64-gobject-introspection mingw-w64-x86_64-zlib glib2-devel
& pacman --noconfirm -S pkg-config mingw-w64-x86_64-gtk3 mingw-w64-x86_64-cairo mingw-w64-x86_64-gobject-introspection mingw-w64-x86_64-zlib glib2-devel

# Work around "Failed to load shared library" messages. Need to update with new versions of GHC.
copy "C:\Users\Paul Johnson\AppData\Local\Programs\stack\x86_64-windows\msys2-20210604\mingw64\bin\zlib1.dll" "C:\Users\Paul Johnson\AppData\Local\Programs\stack\x86_64-windows\ghc-8.10.4\bin"

# Clone repositories.
& cd ~/Documents
& mkdir Haskell
& cd Haskell
& git clone git@github.com:PaulJohnson/hades.git
& git clone git@github.com:PaulJohnson/banana-ui-gtk.git
& git clone git@github.com:PaulJohnson/licensing.git
& git clone git@github.com:PaulJohnson/blank-canvas.git
& git clone git@github.com:PaulJohnson/reactive-banana.git

# Temporary until RSVG bindings are added to haskell-gi
# Install stand-alone GHC.
Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
& choco install haskell-dev
& cabal update
# Following lines deliberately not run in haskell-gi folder.
& cabal install --lib haskell-gi
& git clone git@github.com:PaulJohnson/haskell-gi.git
& cd haskell-gi\bindings
& cabal run genBuildInfo .\RSVG\

