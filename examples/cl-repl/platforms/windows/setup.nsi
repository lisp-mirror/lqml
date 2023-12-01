  Unicode True
  !include "MUI2.nsh"
  Name "cl-repl"
  OutFile "out\Setup-cl-repl-1.0.3.exe"
  InstallDir $PROGRAMFILES\cl-repl
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
  CreateShortCut "$DESKTOP\cl-repl.lnk" "$INSTDIR\app.exe"
  !insertmacro MUI_STARTMENU_WRITE_END
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\cl-repl" "DisplayName" "cl-repl"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\cl-repl" "UninstallString" '"$INSTDIR\Uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\cl-repl" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\cl-repl" "NoRepair" 1
  WriteUninstaller "$INSTDIR\Uninstall.exe"
SectionEnd
Section "Uninstall"
  SetShellVarContext all
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\cl-repl"
  RMDir /r /REBOOTOK "$INSTDIR"
  RMDir /r "$SMPROGRAMS\cl-repl"
  Delete "$DESKTOP\cl-repl.lnk"
  Delete "$INSTDIR\Uninstall.exe"
SectionEnd
Function .onInit
  !insertmacro MUI_LANGDLL_DISPLAY
FunctionEnd

