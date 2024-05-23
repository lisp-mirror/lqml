#include "qt.h"
#include <QImage>
#include <QtDebug>

#ifdef PLUGIN
  #include <ecl_fun_plugin.h>
#else
  #include <ecl_fun.h>
#endif

QObject* ini() {
  static QObject* qt = nullptr;
  if (qt == nullptr) {
    qt = new QT;
#ifdef PLUGIN
    ini_lisp();
#endif
  }
  return qt;
}

QVariant QT::rotateImage(const QVariant& imagePath, const QVariant& angle) {
  // rotates image, replacing it; must be called from the UI thread, see QRUN*
  QString path(imagePath.toString());
  QImage img(path);
  QImage rotated = img.transformed(QTransform().rotate(angle.toReal()));
  rotated.save(path);
  return imagePath;
}

