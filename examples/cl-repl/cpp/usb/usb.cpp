#include "usb.h"
#include <QSerialPortInfo>
#include <QTimer>
#include <QtDebug>

USB::USB() {
  connect(this, &QSerialPort::readyRead, this, &USB::read2);
  connect(this, &QSerialPort::errorOccurred,
          [](QSerialPort::SerialPortError error) {
            if (error != QSerialPort::NoError) {
              qDebug() << "USB error:" << error;
            }
          });
  setBaudRate(Baud9600);

  timer.setSingleShot(true);
  connect(&timer, &QTimer::timeout, this, &USB::done);
}

bool USB::connect2() {
  if (isOpen()) {
    setReady(portName());
    qDebug() << "USB already open:" << portName();
    return true;
  }

  const auto infos = QSerialPortInfo::availablePorts();
  const QStringList supported = { "Arduino" }; // set here micro controller name
  for (auto info : infos) {
    QString name(info.manufacturer() + " | " + info.description());
    QString port(info.portName());
    if (port.startsWith("tty") &&
        (port.contains("ACM", Qt::CaseInsensitive) ||  // Linux
         port.contains("USB", Qt::CaseInsensitive))) { // macOS
      for (auto s : supported) {
        if (name.contains(s, Qt::CaseInsensitive)) {
          setPortName(info.portName());
          qDebug() << "USB:" << port
                   << "VID" << info.vendorIdentifier()
                   << "PID" << info.productIdentifier()
                   << "name" << name;
          goto done;
        }
      }
    } 
  }

done:
  if (!open(QIODevice::ReadWrite)) {
    qDebug() << "USB: unable to open port" << portName();
    return false;
  } else {
    ready = true;
    setReady(portName());
    qDebug() << "USB open";
  }
  return true;
}

void USB::disconnect() {
  close();
  qDebug() << "USB closed";
}

void USB::write2(const QByteArray& data) {
  if (ready) {
    write(data);
  } else {
    qDebug() << "USB not ready: write()";
  }
}

void USB::read2() {
  packets << readAll();
  timer.start(250); // assume receiving done after pause
}

void USB::done() {
  Q_EMIT receivingDone(packets.join());
  packets.clear();
}

