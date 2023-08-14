#pragma once

#undef SLOT

#include <ecl/ecl.h>
#include <QQmlEngine>
#include <QGuiApplication>
#include <QInputMethodEvent>

QT_BEGIN_NAMESPACE

#define STRING(s) ecl_make_constant_base_string(s, -1)

#define DEFUN(name, c_name, num_args) \
  ecl_def_c_function(ecl_read_from_cstring(name), (cl_objectfn_fixed)c_name, num_args);

cl_object do_ini_app (); // for background ini

class Engine : public QQmlEngine {
  Q_OBJECT
public:
  Engine(QObject* parent = nullptr) : QQmlEngine(parent) {}

  Q_PROPERTY (QUrl baseUrl READ baseUrl WRITE setBaseUrl)

  Q_INVOKABLE void clearCache() { clearComponentCache(); }
};

class GuiApplication : public QGuiApplication {
  Q_OBJECT
public:
  GuiApplication(int& argc, char* argv[]) : QGuiApplication(argc, argv) {
#if (defined Q_OS_IOS) && (defined DISABLE_SMART_QUOTES)
    installEventFilter(this);
#endif
  }

#if (defined Q_OS_IOS) && (defined DISABLE_SMART_QUOTES)
  bool eventFilter(QObject* object, QEvent* event) override {
    if (event->type() == QEvent::InputMethod) {
      static bool skipAutoFullStop = false;
      QInputMethodEvent* input = static_cast<QInputMethodEvent*>(event);
      QString s = input->commitString();
      int size = s.size();
      if (size == 0) {
        skipAutoFullStop = true;
      } else {
        if (size == 1) {
          static QChar exChar;
          bool changed = true;
          const int code = s.at(0).unicode();
          switch (code) {
            // prevent auto full stop after double space
            case '.':
              if (skipAutoFullStop && (exChar == ' ')) {
                s = " ";
              } else {
                changed = false;
              }
              break;
            // capture Tab (iOS external keyboard), since QML Keys doesn't capture it
            case '\t':
              s = QString();
              keyPressed("Tab", object->objectName());
              break;
            // undo automatic double hyphen substitution
            case 8212:
              s = "--";
              break;
            // replace iOS smart quotation marks with standard ones
            // (English, French, German, ...)
            case 8216:
            case 8217:
            case 8218:
              s[0] = QChar('\'');
              break;
            case 171:
            case 187:
            case 8220:
            case 8221:
            case 8222:
              s[0] = QChar('"');
              break;
            // capture Alt+E, Alt+L (iOS external keyboard) for example 'cl-repl'
            case 8364:
              s = QString();
              keyPressed("Alt+E", object->objectName());
              break;
            case 172:
              s = QString();
              keyPressed("Alt+L", object->objectName());
              break;
            default:
              changed = false;
          }
          if (changed) {
            input->setCommitString(s);
          }
          exChar = s.at(0);
        } else if (size == 2) {
          bool changed = true;
          s = s.trimmed();
          const int code = s.at(0).unicode();
          switch (code) {
            // replace iOS smart quotation marks with standard ones
            // (Frensh, ...)
            case 171:
            case 187:
              s[0] = QChar('"');
              break;
            default:
              changed = false;
          }
          if (changed) {
            input->setCommitString(s);
          }
        }
        skipAutoFullStop = false;
      }
    }
    return QGuiApplication::eventFilter(object, event);
  }

Q_SIGNALS:
  void keyPressed(const QString&, const QString&);
#endif
};

QT_END_NAMESPACE
