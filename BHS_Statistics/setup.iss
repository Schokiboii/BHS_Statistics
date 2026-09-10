[Setup]
AppName=Hermeskim Auswertung
AppVersion=1.0
DefaultDirName={autopf}\HermeskimAuswertung
DefaultGroupName=Hermeskim Auswertung
OutputDir=Installer
OutputBaseFilename=Hermeskim_Auswertung_Setup
Compression=lzma
SolidCompression=yes
; SetupIconFile=compiler:Setup.ico

[Files]
Source: "R-Portable\*"; DestDir: "{app}\R-Portable"; Flags: ignoreversion recursesubdirs
Source: "data\*"; DestDir: "{app}\data"; Flags: ignoreversion recursesubdirs
Source: "scripts\*"; DestDir: "{app}\scripts"; Flags: ignoreversion recursesubdirs
Source: "start_app.bat"; DestDir: "{app}"; Flags: ignoreversion

[Icons]
Name: "{autoprograms}\Hermeskim Auswertung"; Filename: "{app}\start_app.bat"
Name: "{autodesktop}\Hermeskim Auswertung"; Filename: "{app}\start_app.bat"

[Run]
Filename: "{app}\start_app.bat"; Description: "App jetzt starten"; Flags: postinstall nowait skipifsilent unchecked