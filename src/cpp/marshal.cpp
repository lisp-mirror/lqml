#include "marshal.h"
#include <ecl/ecl.h>
#include <QVariant>
#include <QObject>

QT_BEGIN_NAMESPACE

// *** Lisp to Qt ***

template<typename T>
T toInt(cl_object l_num) {
  T i = 0;
  if (cl_integerp(l_num) == ECL_T) {
    i = fixint(l_num);
  }
  return i;
}

int toInt(cl_object l_num) {
  return toInt<int>(l_num);
}

template<typename T>
T toUInt(cl_object l_num) {
  T i = 0;
  if (cl_integerp(l_num) == ECL_T) {
    i = fixnnint(l_num);
  }
  return i;
}

uint toUInt(cl_object l_num) {
  return toUInt<uint>(l_num);
}

template<typename T>
T toFloat(cl_object l_num) {
  T f = 0;
  if (ECL_SINGLE_FLOAT_P(l_num)) {
    f = sf(l_num);
  }
  else if (ECL_DOUBLE_FLOAT_P(l_num)) {
    f = df(l_num);
  }
#ifdef ECL_LONG_FLOAT
  else if (ECL_LONG_FLOAT_P(l_num)) {
    f = ecl_long_float(l_num);
  }
#endif
  else if (cl_integerp(l_num) == ECL_T) {
    f = fixint(l_num);
  }
  else {
    cl_object l_f = cl_float(1, l_num);
    if (ECL_DOUBLE_FLOAT_P(l_f)) {
      f = df(l_f);
    }
    else if (ECL_SINGLE_FLOAT_P(l_f)) {
      f = sf(l_f);
    }
#ifdef ECL_LONG_FLOAT
    else if (ECL_LONG_FLOAT_P(l_f)) {
      f = ecl_long_float(l_f);
    }
#endif
  }
  return f;
}

float toFloat(cl_object l_num) {
  return toFloat<float>(l_num);
}

qreal toReal(cl_object l_num) {
  return toFloat<qreal>(l_num);
}

QByteArray toCString(cl_object l_str) {
  QByteArray ba;
  if (ECL_STRINGP(l_str)) {
    if (ECL_BASE_STRING_P(l_str)) {
      ba = QByteArray(reinterpret_cast<char*>(l_str->base_string.self),
                      l_str->base_string.fillp);
    }
    else {
      uint l = l_str->string.fillp;
      ba.resize(l);
      ecl_character* l_s = l_str->string.self;
      for (uint i = 0; i < l; i++) {
        ba[i] = l_s[i];
      }
    }
  }
  return ba;
}

QByteArray toQByteArray(cl_object l_vec) {
  QByteArray ba;
  if (ECL_VECTORP(l_vec)) {
    int len = LEN(l_vec);
    ba.resize(len);
    for (int i = 0; i < len; i++) {
      ba[i] = toInt(ecl_aref(l_vec, i));
    }
  }
  return ba;
}

QString toQString(cl_object l_str) {
  QString s;
  if (ECL_STRINGP(l_str)) {
    if (ECL_BASE_STRING_P(l_str)) {
      s = QString::fromLatin1(reinterpret_cast<char*>(l_str->base_string.self),
                              l_str->base_string.fillp);
    }
    else {
      uint l = l_str->string.fillp;
      s.resize(l);
      ecl_character* l_s = l_str->string.self;
      for (uint i = 0; i < l; i++) {
        s[i] = QChar(l_s[i]);
      }
    }
  }
  return s;
}

TO_QT_FLOAT_2 (QPointF)
TO_QT_FLOAT_2 (QSizeF)
TO_QT_FLOAT_4 (QRectF)

QVariant toQVariant(cl_object l_arg, int type) {
  QVariant var;
  switch (type) {
    case QMetaType::QByteArray: var = toQByteArray(l_arg); break;
    case QMetaType::QPointF:    var = toQPointF(l_arg);    break;
    case QMetaType::QRectF:     var = toQRectF(l_arg);     break;
    case QMetaType::QSizeF:     var = toQSizeF(l_arg);     break;
    default:
    if (cl_integerp(l_arg) == ECL_T) {                  // int
      var = QVariant(toInt(l_arg));
    }
    else if (cl_floatp(l_arg) == ECL_T) {               // double
      var = QVariant(toFloat<double>(l_arg));
    }
    else if (cl_stringp(l_arg) == ECL_T) {              // string
      var = QVariant(toQString(l_arg));
    }
    else if (l_arg == ECL_T) {                          // true
      var = QVariant(true);
    }
    else if (l_arg == ECL_NIL) {                        // false
      var = QVariant(false);
    }
    else if (cl_listp(l_arg) == ECL_T) {                // list
      var = QVariant::fromValue(toQVariantList(l_arg));
    }
    else if (cl_vectorp(l_arg) == ECL_T) {              // vector (of octets)
      var = QVariant(toQByteArray(l_arg));
    }
    else {                                              // default: undefined
      var = QVariant();
    }
    break;
  }
  return var;
}

QVariantList toQVariantList(cl_object l_list) {
  QVariantList l;
  if (ECL_LISTP(l_list)) {
    for (cl_object l_do_list = l_list; l_do_list != ECL_NIL; l_do_list = cl_cdr(l_do_list)) {
      cl_object l_el = cl_car(l_do_list);
      l << toQVariant(l_el);
    }
  }
  return l;
}

QObject* toQObjectPointer(cl_object l_obj) {
  STATIC_SYMBOL_PKG (s_qt_object_p,       "QT-OBJECT-P",       "QML") // see 'ini.lisp'
  STATIC_SYMBOL_PKG (s_qt_object_address, "QT-OBJECT-ADDRESS", "QML")
  if (cl_funcall(2, s_qt_object_p, l_obj) != ECL_NIL) {
    return reinterpret_cast<QObject*>(toUInt<quintptr>(cl_funcall(2, s_qt_object_address, l_obj)));
  }
  return nullptr;
}



// *** Qt to Lisp

cl_object from_cstring(const QByteArray& s) {
  cl_object l_s = ecl_alloc_simple_base_string(s.length());
  memcpy(l_s->base_string.self, s.constData(), s.length());
  return l_s;
}

static cl_object make_vector() {
  STATIC_SYMBOL_PKG (s_make_vector, "%MAKE-VECTOR", "QML") // see "ini.lisp"
  cl_object l_vector = cl_funcall(1, s_make_vector);
  return l_vector;
}

cl_object from_qbytearray(const QByteArray& ba) {
  cl_object l_vec = make_vector();
  for (int i = 0; i < ba.size(); i++) {
    cl_vector_push_extend(2, ecl_make_fixnum(ba.at(i)), l_vec);
  }
  return l_vec;
}

cl_object from_qstring(const QString& s) {
  cl_object l_s = ecl_alloc_simple_extended_string(s.length());
  ecl_character* l_p = l_s->string.self;
  for (int i = 0; i < s.length(); i++) {
    l_p[i] = s.at(i).unicode();
  }
  return l_s;
}

TO_CL_FLOAT_2 (QPointF, qpointf, x, y)
TO_CL_FLOAT_2 (QSizeF,  qsizef,  width, height)
TO_CL_FLOAT_4 (QRectF,  qrectf,  x, y, width, height)

cl_object from_qvariant(const QVariant& var) {
  cl_object l_obj = ECL_NIL;
#if QT_VERSION < 0x060000
  const int type = var.type();
#else
  const int type = var.typeId();
#endif
  switch (type) {
    case QMetaType::Bool:       l_obj = var.toBool() ? ECL_T : ECL_NIL;               break;
    case QMetaType::Double:     l_obj = ecl_make_doublefloat(var.toDouble());         break;
    case QMetaType::Int:        l_obj = ecl_make_integer(var.toInt());                break;
    case QMetaType::UInt:       l_obj = ecl_make_unsigned_integer(var.toUInt());      break;
    case QMetaType::ULongLong:  l_obj = ecl_make_unsigned_integer(var.toULongLong()); break;
    case QMetaType::QByteArray: l_obj = from_qbytearray(var.toByteArray());           break;
    case QMetaType::QPointF:    l_obj = from_qpointf(var.toPointF());                 break;
    case QMetaType::QRectF:     l_obj = from_qrectf(var.toRectF());                   break;
    case QMetaType::QSizeF:     l_obj = from_qsizef(var.toSizeF());                   break;
    case QMetaType::QString:    l_obj = from_qstring(var.toString());                 break;
    // special case (can be nested)
    case QMetaType::QVariantList:
    Q_FOREACH(QVariant v, var.value<QVariantList>()) {
      l_obj = CONS(from_qvariant(v), l_obj);
    }
    l_obj = cl_nreverse(l_obj);
    break;
  }
  return l_obj;
}

cl_object from_qobject_pointer(QObject* qobject) {
  STATIC_SYMBOL_PKG (s_qt_object, "QT-OBJECT", "QML") // see 'ini.lisp'
  return cl_funcall(2,
                    s_qt_object,
                    ecl_make_unsigned_integer(reinterpret_cast<quintptr>(qobject)));
}

QT_END_NAMESPACE
