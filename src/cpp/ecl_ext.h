#pragma once

#undef SLOT

#include <ecl/ecl.h>
#include <QList>
#include <QVariant>

QT_BEGIN_NAMESPACE

#define DEFUN(name, c_name, num_args) \
  ecl_def_c_function(ecl_read_from_cstring(name), (cl_objectfn_fixed)c_name, num_args);

#define STRING(s) ecl_make_constant_base_string(s, -1)

#define STRING_COPY(s) (s ? ecl_make_simple_base_string(s, -1) : ECL_NIL)

#define PRINT(x) cl_print(1, x)

#define TERPRI() cl_terpri(0)

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

cl_object ensure_permissions2 (cl_object);
cl_object js2                 (cl_object, cl_object);
cl_object pixel_ratio         ();
cl_object qapropos2           (cl_object, cl_object, cl_object);
cl_object qchildren           (cl_object);
cl_object qcopy_file          (cl_object, cl_object);
cl_object qdirectory          (cl_object);
cl_object qescape             (cl_object);
cl_object qexec2              (cl_object);
cl_object qexit               ();
cl_object qfind_child         (cl_object, cl_object);
cl_object qfind_children2     (cl_object, cl_object, cl_object);
cl_object qfrom_utf8          (cl_object);
cl_object qinvoke_method2     (cl_object, cl_object, cl_object);
cl_object qload_cpp           (cl_object, cl_object);
cl_object qlog2               (cl_object);
cl_object qnull               (cl_object);
cl_object qml_get2            (cl_object, cl_object);
cl_object qml_set2            (cl_object, cl_object, cl_object);
cl_object qobject_name        (cl_object);
cl_object qprocess_events     ();
cl_object qquit2              (cl_object);
cl_object qrun_on_ui_thread2  (cl_object, cl_object);
cl_object qget2               (cl_object, cl_object);
cl_object qset2               (cl_object, cl_object);
cl_object qsingle_shot2       (cl_object, cl_object);
cl_object qtranslate          (cl_object, cl_object, cl_object);
cl_object qversion            ();
cl_object qt_object_info      (cl_object);
cl_object reload2             ();
cl_object root_item           ();
cl_object set_shutdown_p      (cl_object);

void iniCLFunctions();
void error_msg(const char*, cl_object);

QT_END_NAMESPACE
