#ifndef MARSHAL_H
#define MARSHAL_H

#undef SLOT

#include <ecl/ecl.h>
#include <QRectF>
#include <QVariant>

QT_BEGIN_NAMESPACE

#define STRING(s) ecl_make_constant_base_string(s, -1)

#define STRING_COPY(s) (s ? ecl_make_simple_base_string(s, -1) : ECL_NIL)

#define STATIC_SYMBOL(var, name) \
  static cl_object var = cl_intern(1, ecl_make_constant_base_string(name, -1));

#define STATIC_SYMBOL_PKG(var, name, pkg) \
  static cl_object var = cl_intern(2, \
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
static cl_object from_##name(const cap_name& q) { \
  cl_object l_ret = LIST2(ecl_make_doublefloat(q.x1()), ecl_make_doublefloat(q.x2())); \
  return l_ret; \
}

#define TO_CL_FLOAT_4(cap_name, name, x1, x2, x3, x4) \
static cl_object from_##name(const cap_name& q) { \
  cl_object l_ret = LIST4(ecl_make_doublefloat(q.x1()), ecl_make_doublefloat(q.x2()), ecl_make_doublefloat(q.x3()), ecl_make_doublefloat(q.x4())); \
  return l_ret; \
}

#define TO_QT_FLOAT_2(name) \
static name to##name(cl_object x) { \
  if (LISTP(x)) { \
    return name(toReal(cl_first(x)), toReal(cl_second(x))); \
  } \
  return name(); \
}

#define TO_QT_FLOAT_4(name) \
static name to##name(cl_object x) { \
  if (LISTP(x)) { \
    return name(toReal(cl_first(x)), toReal(cl_second(x)), toReal(cl_third(x)), toReal(cl_fourth(x))); \
  } \
  return name(); \
}

template<typename T> T toInt(cl_object);
template<typename T> T toUInt(cl_object);
template<typename T> T toFloat(cl_object);

int          toInt(cl_object);
uint         toUInt(cl_object);
float        toFloat(cl_object);
qreal        toReal(cl_object);
QByteArray   toCString(cl_object);
QByteArray   toQByteArray(cl_object);
QString      toQString(cl_object);
QVariant     toQVariant(cl_object, int = -1);
QVariantList toQVariantList(cl_object);
QObject*     toQObjectPointer(cl_object);

cl_object from_cstring(const QByteArray&);
cl_object from_qbytearray(const QByteArray&);
cl_object from_qstring(const QString&);
cl_object from_qvariant(const QVariant&);
cl_object from_qobject_pointer(QObject*);

QString toCamelCase(const QString&);

QT_END_NAMESPACE

#endif
