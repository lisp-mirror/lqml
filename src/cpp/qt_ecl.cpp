#undef SLOT

#include "qt_ecl.h"
#include "marshal.h"
#include "ecl_ext.h"
#include <ecl/ecl.h>
#include <QVariant>

QT_BEGIN_NAMESPACE

static QHash<QByteArray, void*> lisp_functions;

static cl_object lisp_apply(cl_object l_fun, cl_object l_args) {
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
                 const QVariant& a10) {
  void* symbol = lisp_functions.value(pkgFun);
  if(!symbol) {
    int p = pkgFun.indexOf(':');
    QByteArray pkg = (p == -1) ? "qml-user" : pkgFun.left(p);
    QByteArray fun = pkgFun.mid(pkgFun.lastIndexOf(':') + 1);
    cl_object l_sym = cl_find_symbol(2,
                                     make_constant_base_string(fun.toUpper().constData()),
                                     cl_find_package(make_constant_base_string(pkg.toUpper().constData())));
    if(l_sym != Cnil) {
      symbol = l_sym;
      lisp_functions[pkgFun] = symbol;
    }
  }
  cl_object l_args = Cnil;
  if(!a1.isNull()) { PUSH_ARG(a1);
    if(!a2.isNull()) { PUSH_ARG(a2);
      if(!a3.isNull()) { PUSH_ARG(a3);
        if(!a4.isNull()) { PUSH_ARG(a4);
          if(!a5.isNull()) { PUSH_ARG(a5);
            if(!a6.isNull()) { PUSH_ARG(a6);
              if(!a7.isNull()) { PUSH_ARG(a7);
                if(!a8.isNull()) { PUSH_ARG(a8);
                  if(!a9.isNull()) { PUSH_ARG(a9);
                    if(!a10.isNull()) { PUSH_ARG(a10); }
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
  if(symbol) {
    cl_object l_ret = lisp_apply((cl_object)symbol, l_args);
    return toQVariant(l_ret);
  }
  error_msg(QString("ecl_fun(): %1").arg(QString(pkgFun)).toLatin1().constData(), l_args);
  return QVariant();
}

QT_END_NAMESPACE

