#include "qt.h"

#import <UIKit/UIKit.h>

QVariant QT::keepScreenOn(const QVariant& on) {
  [UIApplication sharedApplication].idleTimerDisabled = on.toBool() ? YES : NO;
  return on;
}

