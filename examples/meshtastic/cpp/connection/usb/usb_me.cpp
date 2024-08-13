#include "usb_me.h"
#include "../connection.h"
#include <QSerialPortInfo>
#include <QTimer>
#include <QtDebug>

USB_ME::USB_ME(Connection* _con) : con(_con) {
  connect(this, &QSerialPort::readyRead, this, &USB_ME::read2);
  connect(this, &QSerialPort::errorOccurred,
          [&](QSerialPort::SerialPortError error) {
            if (error != QSerialPort::NoError) {
              qDebug() << "USB error:" << error;
              disconnect();
            }
          });
  setBaudRate(Baud115200);

  connect(this, &QIODevice::bytesWritten, [&](qint64) { con->sendingDone(); });

  timer.setSingleShot(true);
  connect(&timer, &QTimer::timeout, this, &USB_ME::done);
}

void USB_ME::connectToRadio() {
  if (isOpen()) {
    if (!con->backgroundMode) {
      con->setReady(QVariant(QVariantList() << true));
    }
    qDebug() << "USB already open:" << portName();
    return;
  }

  const auto infos = QSerialPortInfo::availablePorts();
  // tested with Heltec v3, LilyGO T-Beam/T-Deck, RAK 4631
  const QStringList supported = { "RAK", "T-Deck", "UART", "USB" };
  for (auto info : infos) {
    QString name(info.manufacturer() + " | " + info.description());
    QString port(info.portName());
#ifdef Q_OS_UNIX
    if (port.startsWith("tty") &&
        (port.contains("ACM", Qt::CaseInsensitive) ||  // Linux
         port.contains("USB", Qt::CaseInsensitive))) { // macOS
#else
    if (port.startsWith("COM", Qt::CaseInsensitive)) { // Windows
#endif
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
  if (open(QIODevice::ReadWrite)) {
    ready = true;
    setDataTerminalReady(true);
    setRequestToSend(true);
    if (!con->backgroundMode) {
      con->setReady(QVariant(QVariantList() << true));
    }
    qDebug() << "USB open";
  } else {
    qDebug() << "USB: unable to open port" << portName();
  }
}

void USB_ME::disconnect() {
  close();
  qDebug() << "USB closed";
}

void USB_ME::write2(const QByteArray& data) {
  if (ready) {
    write(data);
  } else {
    qDebug() << "USB not ready: write()";
  }
}

void USB_ME::read2() {
  packets << readAll();
  timer.start(1000); // assume receiving done after pause of 1 sec
}

void USB_ME::done() {
  con->done(packets);
}

