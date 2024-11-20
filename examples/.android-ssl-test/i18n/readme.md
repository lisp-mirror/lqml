
Translations
------------

Wrap all strings which need to be translated in either `(tr "")` (Lisp files)
or `qsTr("")` (QML files).

* compile app (either desktop or mobile, you may need `touch ../app.asd` to
  force recompilation of all files); this will generate a dummy file `tr.h`,
  containing all Lisp strings to translate

* run Qt command `lupdate` (here: Spanish, French) for creating the translation
  source files from both Lisp and QML strings:
```
  lupdate ../app.pro -ts es.ts fr.ts
```
* translate all `*.ts` files using **Qt Linguist**

* run Qt command `lrelease` to create compiled translation files:
```
  lrelease es.ts fr.ts
```
* run respective `qmake` again (destop/mobile) in order to include all `*.qm`
  files (compiled translations)

* next time you compile the app, the translation files will be included as
  resources in the executable

Now when you launch the app, the translation file matching your system locale
setting of your platform (see `QLocale`) will be loaded, see `QTranslator` in
[main.cpp](../../../src/cpp/main.cpp).
