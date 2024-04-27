#include "usb_me.h"
#include "../connection.h"
#include <QSerialPortInfo>
#include <QTimer>
#include <QGuiApplication>
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
}

void USB_ME::connectToRadio() {
  if (isOpen()) {
    if (!con->backgroundMode) {
      con->setReady(QVariant(QVariantList() << portName()));
    }
    qDebug() << "USB already open;" << portName();
    return;
  }

  const auto infos = QSerialPortInfo::availablePorts();
  // tested with RAK 4631, LILYGO T-Beam
  const QStringList supported = { "RAK", "UART", "USB" };
  for (auto info : infos) {
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
    if (!con->backgroundMode) {
      con->setReady(QVariant(QVariantList() << portName()));
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
    write(data.constData(), data.size());
  } else {
    qDebug() << "USB not ready: write()";
  }
}

void USB_ME::read2() {
  const QByteArray HEADER = QByteArray::fromHex("94c3");
  QByteArray data(readAll());

  if (data.startsWith(HEADER)) { // skip evtl. debug log
    // split on every new header
    const int LEN = 4;
    int end = 0;
    int start = LEN;
    while ((end = data.indexOf(HEADER, start)) != -1) {
      received(data.mid(start, end - start));
      start = end + LEN;
    }
    received(data.mid(start));

    static QTimer* timer = nullptr;
    if (timer == nullptr) {
      timer = new QTimer;
      timer->setSingleShot(true);
      connect(timer, &QTimer::timeout, this, &USB_ME::done);
    }
    timer->start(1000); // assume receiving done after pause of 1 sec
  }
}

void USB_ME::received(const QByteArray& data) {
  if (con->backgroundMode) {
    con->saveBytes(data);
  } else {
    con->receivedFromRadio(QVariant(QVariantList() << data));
  }
}

void USB_ME::done() {
  if (!con->backgroundMode) {
    con->receivingDone();
    static bool startup = true;
    if (startup) {
      con->sendSavedBytes(); // for eventual, saved but not sent packets
    } else {
      startup = false;
    }
  }
}

