#pragma once

#include <QQmlEngine>
#include <QGuiApplication>
#include <QInputMethodEvent>

QT_BEGIN_NAMESPACE

class Engine : public QQmlEngine {
  Q_OBJECT
public:
  Engine(QObject* parent = nullptr) : QQmlEngine(parent) {}

  Q_PROPERTY (QUrl baseUrl READ baseUrl WRITE setBaseUrl)

  Q_INVOKABLE void clearCache() { clearComponentCache(); }
};

class EventFilterApp : public QGuiApplication {
  Q_OBJECT
public:
  EventFilterApp(int& argc, char* argv[]) : QGuiApplication(argc, argv) {
#if (defined Q_OS_IOS) && (defined DISABLE_SMART_QUOTES)
    installEventFilter(this);
#endif
  }

#if (defined Q_OS_IOS) && (defined DISABLE_SMART_QUOTES)
  bool eventFilter(QObject* object, QEvent* event) override {
    if (event->type() == QEvent::InputMethod) {
      QInputMethodEvent* input = static_cast<QInputMethodEvent*>(event);
      QString s = input->commitString();
      if (s.size() == 1) {
        bool changed = true;
        const int code = s.at(0).unicode();
        switch (code) {
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
      } else if (s.size() == 2) {
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
    }
    return QGuiApplication::eventFilter(object, event);
  }

Q_SIGNALS:
  void keyPressed(const QString&, const QString&);
#endif
};

QT_END_NAMESPACE
