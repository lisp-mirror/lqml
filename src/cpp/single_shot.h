#pragma once

#undef SLOT

#include <ecl/ecl.h>
#include <QObject>

class SingleShot : public QObject {
  Q_OBJECT

public:
  int id;
  void* function;

  SingleShot(int, void*);

protected:
  void timerEvent(QTimerEvent*) override;
};

