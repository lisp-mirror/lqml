#include "qt.h"

#import <UIKit/UIKit.h>

QT_BEGIN_NAMESPACE

QVariant QT::keepScreenOn(const QVariant& on) {
  [UIApplication sharedApplication].idleTimerDisabled = on.toBool() ? YES : NO;
  return on;
}

QT_END_NAMESPACE
