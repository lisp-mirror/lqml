// all-in-one header to include in Qt plugin source files which want to call
// ECL functions through 'ecl_fun()'
//
// does not depend on LQML

#ifndef ECL_FUN_PLUGIN
#define ECL_FUN_PLUGIN

#include <QVariant>
#include <QObject>
#include <ecl/ecl.h>

QT_BEGIN_NAMESPACE

#define STRING(s) ecl_make_constant_base_string(s, -1)

#define STRING_COPY(s) (s ? ecl_make_simple_base_string(s, -1) : ECL_NIL)

#define STATIC_SYMBOL(var, name) \
  cl_object var = cl_intern(1, ecl_make_constant_base_string(name, -1));

#define STATIC_SYMBOL_PKG(var, name, pkg) \
  cl_object var = cl_intern(2, \
                            ecl_make_constant_base_string(name, -1), \
                            cl_find_package(ecl_make_constant_base_string(pkg, -1)));

#define LEN(x) fixint(cl_length(x))

#define  LIST1(a1) \
    CONS(a1,  ECL_NIL)
#define  LIST2(a1, a2) \
    CONS(a1, LIST1(a2))
#define  LIST3(a1, a2, a3) \
    CONS(a1, LIST2(a2, a3))
#define  LIST4(a1, a2, a3, a4) \
    CONS(a1, LIST3(a2, a3, a4))
#define  LIST5(a1, a2, a3, a4, a5) \
    CONS(a1, LIST4(a2, a3, a4, a5))
#define  LIST6(a1, a2, a3, a4, a5, a6) \
    CONS(a1, LIST5(a2, a3, a4, a5, a6))
#define  LIST7(a1, a2, a3, a4, a5, a6, a7) \
    CONS(a1, LIST6(a2, a3, a4, a5, a6, a7))
#define  LIST8(a1, a2, a3, a4, a5, a6, a7, a8) \
    CONS(a1, LIST7(a2, a3, a4, a5, a6, a7, a8))
#define  LIST9(a1, a2, a3, a4, a5, a6, a7, a8, a9) \
    CONS(a1, LIST8(a2, a3, a4, a5, a6, a7, a8, a9))
#define LIST10(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) \
    CONS(a1, LIST9(a2, a3, a4, a5, a6, a7, a8, a9, a10))

#define TO_CL_FLOAT_2(cap_name, name, x1, x2) \
cl_object from_##name(const cap_name& q) { \
  cl_object l_ret = LIST2(ecl_make_doublefloat(q.x1()), ecl_make_doublefloat(q.x2())); \
  return l_ret; \
}

#define TO_CL_FLOAT_4(cap_name, name, x1, x2, x3, x4) \
cl_object from_##name(const cap_name& q) { \
  cl_object l_ret = LIST4(ecl_make_doublefloat(q.x1()), ecl_make_doublefloat(q.x2()), ecl_make_doublefloat(q.x3()), ecl_make_doublefloat(q.x4())); \
  return l_ret; \
}

#define TO_QT_FLOAT_2(name) \
name to##name(cl_object x) { \
  if (LISTP(x)) { \
    return name(toReal(cl_first(x)), toReal(cl_second(x))); \
  } \
  return name(); \
}

#define TO_QT_FLOAT_4(name) \
name to##name(cl_object x) { \
  if (LISTP(x)) { \
    return name(toReal(cl_first(x)), toReal(cl_second(x)), toReal(cl_third(x)), toReal(cl_fourth(x))); \
  } \
  return name(); \
}

void error_msg(const char* fun, cl_object l_args) {
  STATIC_SYMBOL (s_error_output, "*ERROR-OUTPUT*")
  cl_format(4,
            cl_symbol_value(s_error_output),
            STRING("~%[ECL:error] ~A ~{~S~^ ~}~%"),
            STRING(fun),
            l_args);
}

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

QVariant     toQVariant(cl_object, int = -1);
QVariant     toQVariantMap(cl_object);
QVariantList toQVariantList(cl_object);

QVariant toQVariant(cl_object l_arg, int type) {
  QVariant var;
  switch (type) {
    case QMetaType::QPointF: var = toQPointF(l_arg); break;
    case QMetaType::QRectF:  var = toQRectF(l_arg);  break;
    case QMetaType::QSizeF:  var = toQSizeF(l_arg);  break;
    default:
    if (cl_integerp(l_arg) == ECL_T) {              // int
      var = QVariant(toInt(l_arg));
    }
    else if (cl_floatp(l_arg) == ECL_T) {           // double
      var = QVariant(toFloat<double>(l_arg));
    }
    else if (cl_stringp(l_arg) == ECL_T) {          // string
      var = QVariant(toQString(l_arg));
    }
    else if (l_arg == ECL_T) {                      // true
      var = QVariant(true);
    }
    else if (l_arg == ECL_NIL) {                    // false
      var = QVariant(false);
    }
    else if (cl_listp(l_arg) == ECL_T) {            // list
      var = (cl_keywordp(cl_first(l_arg)) == ECL_T)
            ? toQVariantMap(l_arg)
            : toQVariantList(l_arg);
    }
    else {                                          // default: undefined
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

QString toCamelCase(const QString& name) {
  // convert Lisp name to Qt name
  QString qtName(name);
  int j = 0;
  for (int i = 0; i < name.length(); i++, j++) {
    qtName[j] = (name.at(i) == QChar('-'))
                ? name.at(++i).toUpper() : name.at(i);
  }
  qtName.truncate(j);
  return qtName;
}

QVariant toQVariantMap(cl_object l_list) {
  // special case for Lisp keyword arguments
  QVariantMap map;
  if (cl_keywordp(cl_first(l_list)) == ECL_T) {
    cl_object l_do_args = l_list;
    while (l_do_args != ECL_NIL) {
      map.insert(toCamelCase(toQString(cl_symbol_name(cl_first(l_do_args))).toLower()),
                 toQVariant(cl_second(l_do_args)));
      l_do_args = cl_cddr(l_do_args);
    }
  }
  return map;
}

// *** Qt to Lisp ***

cl_object from_cstring(const QByteArray& s) {
  cl_object l_s = ecl_alloc_simple_base_string(s.length());
  memcpy(l_s->base_string.self, s.constData(), s.length());
  return l_s;
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
    case QMetaType::QPointF:    l_obj = from_qpointf(var.toPointF());                 break;
    case QMetaType::QRectF:     l_obj = from_qrectf(var.toRectF());                   break;
    case QMetaType::QSizeF:     l_obj = from_qsizef(var.toSizeF());                   break;
    case QMetaType::QString:    l_obj = from_qstring(var.toString());                 break;
    // special case (can be nested)
    case QMetaType::QVariantList:
      QVariantList list(var.value<QVariantList>());
      for (QVariant v : qAsConst(list)) {
        l_obj = CONS(from_qvariant(v), l_obj);
      }
      l_obj = cl_nreverse(l_obj);
    break;
  }
  return l_obj;
}

QHash<QByteArray, void*> lisp_functions;

cl_object lisp_apply(cl_object l_fun, cl_object l_args) {
  cl_object l_ret = Cnil;
  const cl_env_ptr l_env = ecl_process_env();
  CL_CATCH_ALL_BEGIN(l_env) {
    CL_UNWIND_PROTECT_BEGIN(l_env) {
      l_ret = cl_apply(2, l_fun, l_args);
    }
    CL_UNWIND_PROTECT_EXIT {}
    CL_UNWIND_PROTECT_END;
  }
  CL_CATCH_ALL_END;
  return l_ret;
}

QVariant ecl_fun(
  const QByteArray&,
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant());

#define PUSH_ARG(x) l_args = CONS(from_qvariant(x), l_args)

QVariant ecl_fun(const QByteArray& pkgFun,
                 const QVariant& a1,
                 const QVariant& a2,
                 const QVariant& a3,
                 const QVariant& a4,
                 const QVariant& a5,
                 const QVariant& a6,
                 const QVariant& a7,
                 const QVariant& a8,
                 const QVariant& a9,
                 const QVariant& a10,
                 const QVariant& a11,
                 const QVariant& a12,
                 const QVariant& a13,
                 const QVariant& a14,
                 const QVariant& a15,
                 const QVariant& a16) {
  void* symbol = lisp_functions.value(pkgFun);
  if (!symbol) {
    int p = pkgFun.indexOf(':');
    QByteArray pkg = (p == -1) ? "qml-user" : pkgFun.left(p);
    QByteArray fun = pkgFun.mid(pkgFun.lastIndexOf(':') + 1);
    cl_object l_sym = cl_find_symbol(2,
                                     make_constant_base_string(fun.toUpper().constData()),
                                     cl_find_package(make_constant_base_string(pkg.toUpper().constData())));
    if (l_sym != Cnil) {
      symbol = l_sym;
      lisp_functions[pkgFun] = symbol;
    }
  }
  cl_object l_args = Cnil;
  if (!a1.isNull()) { PUSH_ARG(a1);
    if (!a2.isNull()) { PUSH_ARG(a2);
      if (!a3.isNull()) { PUSH_ARG(a3);
        if (!a4.isNull()) { PUSH_ARG(a4);
          if (!a5.isNull()) { PUSH_ARG(a5);
            if (!a6.isNull()) { PUSH_ARG(a6);
              if (!a7.isNull()) { PUSH_ARG(a7);
                if (!a8.isNull()) { PUSH_ARG(a8);
                  if (!a9.isNull()) { PUSH_ARG(a9);
                    if (!a10.isNull()) { PUSH_ARG(a10);
                      if (!a11.isNull()) { PUSH_ARG(a11);
                        if (!a12.isNull()) { PUSH_ARG(a12);
                          if (!a13.isNull()) { PUSH_ARG(a13);
                            if (!a14.isNull()) { PUSH_ARG(a14);
                              if (!a15.isNull()) { PUSH_ARG(a15);
                                if (!a16.isNull()) { PUSH_ARG(a16); }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  l_args = cl_nreverse(l_args);
  if (symbol) {
    cl_object l_ret = lisp_apply((cl_object)symbol, l_args);
    return toQVariant(l_ret);
  }
  error_msg(QString("ecl_fun(): %1").arg(QString(pkgFun)).toLatin1().constData(), l_args);
  return QVariant();
}

QT_END_NAMESPACE

#endif
