#include "single_shot.h"

QT_BEGIN_NAMESPACE

SingleShot::SingleShot(int msec, void* fun)
  : function(fun) {
  id = startTimer(msec);
}

void SingleShot::timerEvent(QTimerEvent*) {
  killTimer(id);
  const cl_env_ptr l_env = ecl_process_env();
  CL_CATCH_ALL_BEGIN(l_env) {
    CL_UNWIND_PROTECT_BEGIN(l_env) {
      cl_funcall(1, (cl_object)function);
    }
    CL_UNWIND_PROTECT_EXIT {}
    CL_UNWIND_PROTECT_END;
  }
  CL_CATCH_ALL_END;
  deleteLater();
}

QT_END_NAMESPACE

