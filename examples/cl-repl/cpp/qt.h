#pragma once

#include <QtCore>
#include <QSyntaxHighlighter>

#ifdef Q_CC_MSVC
#define LIB_EXPORT __declspec(dllexport)
#else
#define LIB_EXPORT
#endif

#define VAR(type, var) \
  QVariant::fromValue<type>(var)

#define VAR_NEW(type) \
  QVariant::fromValue<type*>(new type())

#define VAR_NEW_1(type, arg) \
  QVariant::fromValue<type*>(new type(arg))

#define VAL(var, type) \
  var.value<type>()

#define DELETE_LATER(var) \
  QTimer::singleShot(0, var, &QObject::deleteLater)

QT_BEGIN_NAMESPACE

extern "C" { LIB_EXPORT QObject* ini(); }

class SyntaxHighlighter : public QSyntaxHighlighter {
  Q_OBJECT
  friend class QT;

public:
  SyntaxHighlighter(QTextDocument*);

protected:
  void highlightBlock(const QString& text) override;
};

// extend all non QObject classes with a QObject

class RegularExpression : public QObject, public QRegularExpression {
  Q_OBJECT
public:
  RegularExpression() {}
  RegularExpression(const QRegularExpression& other) : QRegularExpression(other) {}
};

class TextBlock : public QObject, public QTextBlock {
  Q_OBJECT
public:
  TextBlock(const QTextBlock& other) : QTextBlock(other) {}
};

class TextCursor : public QObject, public QTextCursor {
  Q_OBJECT
public:
  TextCursor(const QTextCursor& other) : QTextCursor(other) {}
};

class TextCharFormat : public QObject, public QTextCharFormat {
  Q_OBJECT
public:
  TextCharFormat() {}
  TextCharFormat(const QTextCharFormat& other) : QTextCharFormat(other) {}
};

// main class

class QT : public QObject {
  Q_OBJECT

public:
  // new
  Q_INVOKABLE QVariant makeObject(const QVariant&, const QVariant& = QVariant());

  // connect
  Q_INVOKABLE QVariant connectDocumentChanged(const QVariant&, const QVariant&);
#ifdef Q_OS_IOS
  Q_INVOKABLE QVariant connectKeyPressed();
#endif

  // methods
  Q_INVOKABLE QVariant block2                (const QVariant&);
  Q_INVOKABLE QVariant blockNumber           (const QVariant&);
  Q_INVOKABLE QVariant characterAt           (const QVariant&, const QVariant&);
  Q_INVOKABLE QVariant clearUndoRedoStacks   (const QVariant&);
  Q_INVOKABLE QVariant currentBlockState     (const QVariant&);
  Q_INVOKABLE QVariant exactMatch            (const QVariant&, const QVariant&);
  Q_INVOKABLE QVariant find2                 (const QVariant&, const QVariant&, const QVariant&);
  Q_INVOKABLE QVariant findBlockByLineNumber (const QVariant&, const QVariant&);
  Q_INVOKABLE QVariant lineCount             (const QVariant&);
  Q_INVOKABLE QVariant next                  (const QVariant&);
  Q_INVOKABLE QVariant position2             (const QVariant&);
  Q_INVOKABLE QVariant positionInBlock       (const QVariant&);
  Q_INVOKABLE QVariant previous              (const QVariant&);
  Q_INVOKABLE QVariant previousBlockState    (const QVariant&);
  Q_INVOKABLE QVariant setCurrentBlockState  (const QVariant&, const QVariant&);
  Q_INVOKABLE QVariant setFormat             (const QVariant&, const QVariant&);
  Q_INVOKABLE QVariant setPattern            (const QVariant&, const QVariant&);
  Q_INVOKABLE QVariant text                  (const QVariant&);
  Q_INVOKABLE QVariant textDocument          (const QVariant&);
};

QT_END_NAMESPACE
