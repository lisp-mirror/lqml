#include "usb_meshtastic.h"
#include <QSerialPortInfo>
#include <QTimer>
#include <QtDebug>

USB_ME::USB_ME() {
  const auto infos = QSerialPortInfo::availablePorts();
  const QStringList boards = { "RAK", "UART" }; // TODO
  for (const QSerialPortInfo& info : infos) {
    QString name(info.description() + " " + info.manufacturer());
    QString port(info.portName());
    qDebug() << "USB:" << port << name;
    if (port.startsWith("tty") &&
        (port.contains("ACM", Qt::CaseInsensitive) ||  // Linux
         port.contains("USB", Qt::CaseInsensitive))) { // macOS
      for (auto board : boards) {
        if (name.contains(board, Qt::CaseInsensitive)) {
          setPortName(info.portName());
          goto done;
        }
      }
    } 
  }
done:
  connect(this, &QSerialPort::readyRead, this, &USB_ME::read2);
  connect(this, &QSerialPort::errorOccurred,
          [](QSerialPort::SerialPortError error) {
            if (error != QSerialPort::NoError) {
              qDebug() << "USB error:" << error;
            }
          });

  if (!open(QIODevice::ReadWrite)) {
    qDebug() << "USB: unable to open port" << portName();
  } else {
    qDebug() << "USB open";
    state = Open;
    QTimer::singleShot(0, this, &USB_ME::wantConfigId);
  }
}

void USB_ME::wantConfigId() {
  if (state != Closed) {
    // 4 bytes header + protobuf 'wantConfigId'
    const char want_config_id[] = { '\x94', '\xc3', '\x00', '\x02',
                                    '\x18', '\x01' };
    write(want_config_id, sizeof(want_config_id));
    qDebug() << "USB wantConfigId()";
  } else {
    qDebug() << "USB not ready: wantConfigId()";
  }
}

void USB_ME::write2(const QByteArray& data) {
  if (state == Ready) {
    write(data.constData(), data.size());
  } else {
    qDebug() << "USB not ready: write()";
  }
}

void USB_ME::read2() {
  static QTimer* timer = nullptr;
  const char header[] = { '\x94', '\xc3' };
  const int MAX = 0xffff;
  QByteArray data(read(MAX));
  if (data.startsWith(header)) { // skip evtl. debug log
    if (state == Open) {
      Q_EMIT setReady(portName());
      qDebug() << "USB ready";
    }
    state = Ready;

    // split on every new header
    const int LEN = 4;
    int end = 0;
    int start = LEN;
    while ((end = data.indexOf(header, start)) != -1) {
      Q_EMIT receivedFromRadio(data.mid(start, end - start));
      start = end + LEN;
    }
    Q_EMIT receivedFromRadio(data.mid(start));

    if (!timer) {
      timer = new QTimer;
      timer->setSingleShot(true);
      connect(timer, &QTimer::timeout, this, &USB_ME::receivingDone);
    }
    timer->start(1000); // assume receiving done after pause of 1 sec
  }
}

