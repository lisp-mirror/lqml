#include "usb_meshtastic.h"
#include <QSerialPortInfo>
#include <QTimer>
#include <QGuiApplication>
#include <QtDebug>

USB_ME::USB_ME() {
  connect(this, &QSerialPort::readyRead, this, &USB_ME::read2);
  connect(this, &QSerialPort::errorOccurred,
          [](QSerialPort::SerialPortError error) {
            if (error != QSerialPort::NoError) {
              qDebug() << "USB error:" << error;
            }
          });
}

void USB_ME::connectToRadio() {
  if (isOpen()) {
    Q_EMIT setReady(portName());
    qDebug() << "USB already open;" << portName();
    return;
  }

  const auto infos = QSerialPortInfo::availablePorts();
  const QStringList supported = { "RAK" }; // TODO: currently RAK only
  for (const QSerialPortInfo& info : infos) {
    QString name(info.description() + " " + info.manufacturer());
    QString port(info.portName());
    if (port.startsWith("tty") &&
        (port.contains("ACM", Qt::CaseInsensitive) ||  // Linux
         port.contains("USB", Qt::CaseInsensitive))) { // macOS
      for (auto s : supported) {
        if (name.contains(s, Qt::CaseInsensitive)) {
          setPortName(info.portName());
          qDebug() << "USB:" << port << name;
          goto done;
        }
      }
    } 
  }
  done:

  if (!open(QIODevice::ReadWrite)) {
    qDebug() << "USB: unable to open port" << portName();
  } else {
    ready = true;
    Q_EMIT setReady(portName());
    qDebug() << "USB open";
  }
}

void USB_ME::disconnect() {
  close();
  qDebug() << "USB closed";
}

void USB_ME::write2(const QByteArray& data) {
  if (ready) {
    write(data.constData(), data.size());
  } else {
    qDebug() << "USB not ready: write()";
  }
}

void USB_ME::read2() {
  const char header[] = { '\x94', '\xc3' };
  QByteArray data(readAll());
  qGuiApp->processEvents(); // macOS needs this for some reason

  if (data.startsWith(header)) { // skip evtl. debug log
    // split on every new header
    const int LEN = 4;
    int end = 0;
    int start = LEN;
    while ((end = data.indexOf(header, start)) != -1) {
      Q_EMIT receivedFromRadio(data.mid(start, end - start));
      start = end + LEN;
    }
    Q_EMIT receivedFromRadio(data.mid(start));

    static QTimer* timer = nullptr;
    if (!timer) {
      timer = new QTimer;
      timer->setSingleShot(true);
      connect(timer, &QTimer::timeout, this, &USB_ME::receivingDone);
    }
    timer->start(1000); // assume receiving done after pause of 1 sec
  }
}

