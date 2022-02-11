#ifndef SINGLE_SHOT_H
#define SINGLE_SHOT_H

#undef SLOT

#include <ecl/ecl.h>
#include <QObject>

QT_BEGIN_NAMESPACE

class SingleShot : public QObject {
  Q_OBJECT

public:
  int id;
  void* function;

  SingleShot(int, void*);

protected:
  void timerEvent(QTimerEvent*) override;
};

QT_END_NAMESPACE

#endif
