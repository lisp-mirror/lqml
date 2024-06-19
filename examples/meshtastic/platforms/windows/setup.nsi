  Unicode True
  !include "MUI2.nsh"
  Name "Mesh SMS"
  OutFile "out\Setup-Mesh-SMS-0.9.4.exe"
  InstallDir "$PROGRAMFILES\Mesh SMS"
  Var STARTMENU_FOLDER
  !define MUI_ABORTWARNING
  !define MUI_FINISHPAGE_RUN $INSTDIR\app.exe
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_STARTMENU Application $STARTMENU_FOLDER
  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH
  !insertmacro MUI_LANGUAGE "English"
  !insertmacro MUI_RESERVEFILE_LANGDLL
Section ""
  SetShellVarContext all
  SetOutPath $INSTDIR
  File /r setup\*.*
  !insertmacro MUI_STARTMENU_WRITE_BEGIN Application
  CreateDirectory "$SMPROGRAMS\$STARTMENU_FOLDER"
  CreateShortCut "$DESKTOP\Mesh SMS.lnk" "$INSTDIR\app.exe"
  !insertmacro MUI_STARTMENU_WRITE_END
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Mesh SMS" "DisplayName" "Mesh SMS"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Mesh SMS" "UninstallString" '"$INSTDIR\Uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Mesh SMS" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Mesh SMS" "NoRepair" 1
  WriteUninstaller "$INSTDIR\Uninstall.exe"
SectionEnd
Section "Uninstall"
  SetShellVarContext all
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Mesh SMS"
  RMDir /r /REBOOTOK "$INSTDIR"
  RMDir /r "$SMPROGRAMS\Mesh SMS"
  Delete "$DESKTOP\Mesh SMS.lnk"
  Delete "$INSTDIR\Uninstall.exe"
SectionEnd
Function .onInit
  !insertmacro MUI_LANGDLL_DISPLAY
FunctionEnd

