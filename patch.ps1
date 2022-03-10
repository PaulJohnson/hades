Copy-item "C:\Users\Paul Johnson\AppData\Roaming\local\bin\dsm.exe" -force -destination "C:\Program Files\Diametric\DSM\bin\dsm.exe"
Copy-item -path "C:\Users\Paul Johnson\Documents\Haskell\hades\share\hades\*" -force -recurse -destination "C:\Program Files\Diametric\DSM\share\hades"
Write-host "dsm.exe and share\hades copied."
