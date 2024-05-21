#include "usb_me.h"
#include "../connection.h"
#include <QSerialPortInfo>
#include <QTimer>
#include <QtDebug>

USB_ME::USB_ME(Connection* _con) : con(_con) {
  connect(this, &QSerialPort::readyRead, this, &USB_ME::read2);
  connect(this, &QSerialPort::errorOccurred,
          [](QSerialPort::SerialPortError error) {
            if (error != QSerialPort::NoError) {
              qDebug() << "USB error:" << error;
            }
          });
  setBaudRate(Baud115200);

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
  // tested with HELTEC v3, LILYGO T-Beam, RAK 4631
  const QStringList supported = { "RAK", "UART", "USB" };
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
  if (!open(QIODevice::ReadWrite)) {
    qDebug() << "USB: unable to open port" << portName();
  } else {
    ready = true;
    setDataTerminalReady(true);
    setRequestToSend(true);
    if (!con->backgroundMode) {
      con->setReady(QVariant(QVariantList() << true));
    }
    qDebug() << "USB open";
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

void USB_ME::received(const QByteArray& data) {
  if (!data.isEmpty()) {
    if (con->backgroundMode) {
      con->saveBytes(data);
    } else {
      con->receivedFromRadio(QVariant(QVariantList() << data));
    }
  }
}

void USB_ME::done() {
  if (!con->backgroundMode) {
    static bool startup = true;
    if (startup) {
      con->sendSavedBytes(); // for eventual, saved but not sent packets
    } else {
      startup = false;
    }
  }

  const QByteArray HEADER = QByteArray::fromHex("94c3");
  const int LEN = 4;
  QByteArray data(packets.join());
  packets.clear();
  int start = 0;
  while ((start = data.indexOf(HEADER, start)) != -1) {
    int i_len = start + 2;
    int len = (data.at(i_len) << 8) + data.at(i_len + 1);
    received(data.mid(start + LEN, len));
    start += LEN + len;
  }

  con->receivingDone();
}

