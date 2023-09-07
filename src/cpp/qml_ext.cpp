#include "qml_ext.h"
#include "lqml.h"
#include "ecl_fun.h"

QT_BEGIN_NAMESPACE

static QVariant qmlApply(QObject* caller,
                         const QString& function,
                         const QVariantList& arguments) {
  return ecl_fun("qml:qml-apply",
                 QVariant(reinterpret_cast<quintptr>(caller)),
                 QVariant(function),
                 QVariant(arguments));
}

static QVariant toVariant(const QJSValue& value) {
  QVariant var = value.toVariant();
#if QT_VERSION < 0x060000
  const int type = var.type();
#else
  const int type = var.typeId();
#endif
  if (type == QMetaType::Double) {
    // workaround for uint32, see '>>>0' in QML
    quint32 uint = var.toUInt();
    if (uint == var.toDouble()) {
      return QVariant(uint);
    }
  }
  return var;
}

QVariant Lisp::call(const QJSValue& caller_or_function,
                    const QJSValue& function_or_arg0,
                    const QJSValue& arg1,
                    const QJSValue& arg2,
                    const QJSValue& arg3,
                    const QJSValue& arg4,
                    const QJSValue& arg5,
                    const QJSValue& arg6,
                    const QJSValue& arg7,
                    const QJSValue& arg8,
                    const QJSValue& arg9,
                    const QJSValue& arg10,
                    const QJSValue& arg11,
                    const QJSValue& arg12,
                    const QJSValue& arg13,
                    const QJSValue& arg14,
                    const QJSValue& arg15,
                    const QJSValue& arg16) {
  QObject* caller = nullptr;
  QString function;
  QVariantList arguments;
  if (caller_or_function.isQObject()) {
    caller = caller_or_function.toQObject();
    function = function_or_arg0.toString();
  } else if (caller_or_function.isString()) {
    function = caller_or_function.toString();
    if (!function_or_arg0.isUndefined()) {
      arguments << toVariant(function_or_arg0);
    }
  }
  if (!arg1.isUndefined()) {
    arguments << toVariant(arg1);
    if (!arg2.isUndefined()) {
      arguments << toVariant(arg2);
      if (!arg3.isUndefined()) {
        arguments << toVariant(arg3);
        if (!arg4.isUndefined()) {
          arguments << toVariant(arg4);
          if (!arg5.isUndefined()) {
            arguments << toVariant(arg5);
            if (!arg6.isUndefined()) {
              arguments << toVariant(arg6);
              if (!arg7.isUndefined()) {
                arguments << toVariant(arg7);
                if (!arg8.isUndefined()) {
                  arguments << toVariant(arg8);
                  if (!arg9.isUndefined()) {
                    arguments << toVariant(arg9);
                    if (!arg10.isUndefined()) {
                      arguments << toVariant(arg10);
                      if (!arg11.isUndefined()) {
                        arguments << toVariant(arg11);
                        if (!arg12.isUndefined()) {
                          arguments << toVariant(arg12);
                          if (!arg13.isUndefined()) {
                            arguments << toVariant(arg13);
                            if (!arg14.isUndefined()) {
                              arguments << toVariant(arg14);
                              if (!arg15.isUndefined()) {
                                arguments << toVariant(arg15);
                                if (!arg16.isUndefined()) {
                                  arguments << toVariant(arg16);
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
  }
  return qmlApply(caller, function, arguments);
}

QVariant Lisp::apply(const QJSValue& caller_or_function,
                     const QJSValue& function_or_arguments,
                     const QJSValue& arguments_or_undefined) {
  QObject* caller = nullptr;
  QString function;
  QVariantList arguments;
  if (caller_or_function.isQObject()) {
    caller = caller_or_function.toQObject();
    function = function_or_arguments.toString();
    arguments = arguments_or_undefined.toVariant().value<QVariantList>();
  }
  else if (caller_or_function.isString()) {
    function = caller_or_function.toString();
    arguments = function_or_arguments.toVariant().value<QVariantList>();
  }
  return qmlApply(caller, function, arguments);
}

QT_END_NAMESPACE
