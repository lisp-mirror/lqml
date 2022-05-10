#include "qt.h"
#include <QtDebug>
#include <QTextDocument>
#include <QQuickTextDocument>

#ifdef PLUGIN
  #include <ecl_fun_plugin.h>
#else
  #include <ecl_fun.h>
#endif

Q_DECLARE_METATYPE(RegularExpression*)
Q_DECLARE_METATYPE(TextBlock*)
Q_DECLARE_METATYPE(TextCharFormat*)
Q_DECLARE_METATYPE(TextCursor*)
Q_DECLARE_METATYPE(SyntaxHighlighter*)
Q_DECLARE_METATYPE(QTextDocument*)

QT_BEGIN_NAMESPACE

QObject* ini() {
  static QObject* qt = nullptr;
  if (!qt) {
    qt = new QT;
#ifdef PLUGIN
    ini_lisp();
#endif
  }
  return qt;
}

SyntaxHighlighter::SyntaxHighlighter(QTextDocument* doc)
  : QSyntaxHighlighter(doc) {}

void SyntaxHighlighter::highlightBlock(const QString& text) {
  ecl_fun("editor:highlight-block", VAR(SyntaxHighlighter*, this), text);
}

// new 

QVariant QT::makeObject(const QVariant& vName, const QVariant& vArg) {
  QString name(vName.toString());
  QVariant vObject;
  if (name == "QTextCharFormat") {
    vObject = VAR_NEW(TextCharFormat);
  } else if (name == "QSyntaxHighlighter") {
    QTextDocument* doc = VAL(vArg, QTextDocument*);
    if (doc != nullptr) {
      vObject = VAR_NEW_1(SyntaxHighlighter, doc);
    }
  } else if (name == "QRegularExpression") {
    vObject = VAR_NEW(RegularExpression);
  }
  return vObject;
}

// connect

QVariant QT::connectDocumentChanged(const QVariant& vDocument, const QVariant& vWhich) {
  QTextDocument* doc = VAL(vDocument, QTextDocument*);
  if (doc != nullptr) {
    QString which = vWhich.toString();
    if (which == "edit") {
      connect(doc, &QTextDocument::blockCountChanged,
              [](int count) {
                ecl_fun("editor:edit-line-count-changed", count);
              });
      connect(doc, &QTextDocument::contentsChanged,
              []() {
                ecl_fun("editor:edit-contents-changed");
              });
    } else if (which == "command") {
      connect(doc, &QTextDocument::blockCountChanged,
              [](int count) {
                ecl_fun("editor:command-line-count-changed", count);
              });
      connect(doc, &QTextDocument::contentsChanged,
              []() {
                ecl_fun("editor:command-contents-changed");
              });
    }
    connect(doc, &QTextDocument::cursorPositionChanged,
            [](const QTextCursor& cursor) {
              TextCursor cursor2(cursor);
              ecl_fun("editor:cursor-position-changed", VAR(TextCursor*, &cursor2));
            });
    return true;
  }
  return false;
}

// methods
//
// pointers stored in a QVariant are type checked (if QObject derived)

QVariant QT::block2(const QVariant& vCursor) {
  QTextCursor* cursor = VAL(vCursor, TextCursor*);
  if (cursor != nullptr) {
    TextBlock* tmp = new TextBlock(cursor->block());
    tmp->deleteLater();
    return VAR(TextBlock*, tmp);
  }
  return QVariant();
}

QVariant QT::blockNumber(const QVariant& vBlock) {
  QTextBlock* block = VAL(vBlock, TextBlock*);
  if (block != nullptr) {
    return block->blockNumber();
  }
  return QVariant();
}

QVariant QT::characterAt(const QVariant& vDocument, const QVariant& vPos) {
  QTextDocument* document = VAL(vDocument, QTextDocument*);
  if (document != nullptr) {
    return VAR(QChar, document->characterAt(vPos.toInt()));
  }
  return QVariant();
}

QVariant QT::clearUndoRedoStacks(const QVariant& vDocument) {
  QTextDocument* document = VAL(vDocument, QTextDocument*);
  if (document != nullptr) {
    document->clearUndoRedoStacks();
  }
  return QVariant();
}

QVariant QT::currentBlockState(const QVariant& vHighlighter) {
  SyntaxHighlighter* highlighter = VAL(vHighlighter, SyntaxHighlighter*);
  if (highlighter != nullptr) {
    return highlighter->currentBlockState();
  }
  return QVariant();
}

QVariant QT::exactMatch(const QVariant& vRegExp, const QVariant& vText) {
  QRegularExpression* regExp = VAL(vRegExp, RegularExpression*);
  if (regExp != nullptr) {
    QString text = vText.toString();
    return (regExp->match(text).capturedLength() == text.length());
  }
  return QVariant();
}

QVariant QT::find2(const QVariant& vDocument, const QVariant& vWhat, const QVariant& vPos) {
  QTextDocument* document = VAL(vDocument, QTextDocument*);
  if (document != nullptr) {
#if QT_VERSION < 0x060000
    const int type = vWhat.type();
#else
    const int type = vWhat.typeId();
#endif
    QTextCursor cursor;
    if (type == QMetaType::QString) {
      cursor = document->find(vWhat.toString(), vPos.toInt());
    } else {
      QRegularExpression* regexp = VAL(vWhat, RegularExpression*);
      if (regexp != nullptr) {
        cursor = document->find(*regexp, vPos.toInt());
      }
    }
    return QVariantList()
             << cursor.selectionStart()
             << cursor.selectionEnd()
             << cursor.selectedText();
  }
  return QVariant();
}

QVariant QT::findBlockByLineNumber(const QVariant& vDocument, const QVariant& vNumber) {
  QTextDocument* document = VAL(vDocument, QTextDocument*);
  if (document != nullptr) {
    TextBlock* tmp = new TextBlock(document->findBlockByLineNumber(vNumber.toInt()));
    tmp->deleteLater();
    return VAR(TextBlock*, tmp);
  }
  return QVariant();
}

QVariant QT::lineCount(const QVariant& vDocument) {
  QTextDocument* document = VAL(vDocument, QTextDocument*);
  if (document != nullptr) {
    return document->lineCount();
  }
  return QVariant();
}

QVariant QT::next(const QVariant& vBlock) {
  QTextBlock* block = VAL(vBlock, TextBlock*);
  if (block != nullptr) {
    TextBlock* tmp = new TextBlock(block->next());
    tmp->deleteLater();
    return VAR(TextBlock*, tmp);
  }
  return QVariant();
}

QVariant QT::position2(const QVariant& vCursor) {
  QTextCursor* cursor = VAL(vCursor, TextCursor*);
  if (cursor != nullptr) {
    return cursor->position();
  }
  return QVariant();
}

QVariant QT::positionInBlock(const QVariant& vCursor) {
  QTextCursor* cursor = VAL(vCursor, TextCursor*);
  if (cursor != nullptr) {
    return cursor->positionInBlock();
  }
  return QVariant();
}

QVariant QT::previous(const QVariant& vBlock) {
  QTextBlock* block = VAL(vBlock, TextBlock*);
  if (block != nullptr) {
    TextBlock* tmp = new TextBlock(block->previous());
    tmp->deleteLater();
    return VAR(TextBlock*, tmp);
  }
  return QVariant();
}

QVariant QT::previousBlockState(const QVariant& vHighlighter) {
  SyntaxHighlighter* highlighter = VAL(vHighlighter, SyntaxHighlighter*);
  if (highlighter != nullptr) {
    return highlighter->previousBlockState();
  }
  return QVariant();
}

QVariant QT::setCurrentBlockState(const QVariant& vHighlighter, const QVariant& vState) {
  SyntaxHighlighter* highlighter = VAL(vHighlighter, SyntaxHighlighter*);
  if (highlighter != nullptr) {
    highlighter->setCurrentBlockState(vState.toInt());
  }
  return QVariant();
}

QVariant QT::setFormat(const QVariant& vObject, const QVariant& vFormat) {
  QTextCharFormat* format = VAL(vObject, TextCharFormat*);
  if (format != nullptr) {
    QVariantMap map = VAL(vFormat, QVariantMap);
    format->setForeground(QColor(map.value("color").toString()));
    if (map.contains("bold")) {
      format->setFontWeight(QFont::Bold);
    }
    if (map.contains("italic")) {
      format->setFontItalic(true);
    }
    return true;
  }
  SyntaxHighlighter* highlighter = VAL(vObject, SyntaxHighlighter*);
  if (highlighter != nullptr) {
    QVariantList args = VAL(vFormat, QVariantList);
    int start = args.at(0).toInt();
    int count = args.at(1).toInt();
    QTextCharFormat* format = VAL(args.at(2), TextCharFormat*);
    if (format != nullptr) {
      highlighter->setFormat(start, count, *format);
    } else {
      highlighter->setFormat(start, count, QColor(args.at(2).toString()));
    }
    return true;
  }
  return false;
}

QVariant QT::setPattern(const QVariant& vObject, const QVariant& vArg) {
  QRegularExpression* regexp = VAL(vObject, RegularExpression*);
  if (regexp != nullptr) {
    regexp->setPattern(vArg.toString());
    regexp->optimize();
  }
  return QVariant();
}

QVariant QT::text(const QVariant& vBlock) {
  QTextBlock* block = VAL(vBlock, TextBlock*);
  if (block != nullptr) {
    return block->text();
  }
  return QVariant();
}

QVariant QT::textDocument(const QVariant& vDocument) {
  QQuickTextDocument* document = VAL(vDocument, QQuickTextDocument*);
  if (document != nullptr) {
    return VAR(QTextDocument*, document->textDocument());
  }
  return QVariant();
}

QT_END_NAMESPACE
