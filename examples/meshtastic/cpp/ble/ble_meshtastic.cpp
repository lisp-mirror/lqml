#include "ble_meshtastic.h"
#include <QMetaEnum>
#include <QStandardPaths>
#include <QFile>
#include <QDataStream>

#ifdef Q_OS_ANDROID
  #include "../android_service/qtandroidservice_ro.h"
  #include <QAndroidService>
#endif

// service
const UID BLE_ME::uuid_service   = UID(STR("{6ba1b218-15a8-461f-9fa8-5dcae273eafd}"));

// characteristics
const UID BLE_ME::uuid_toRadio   = UID(STR("{f75c76d2-129e-4dad-a1dd-7866124401e7}"));
const UID BLE_ME::uuid_fromRadio = UID(STR("{2c55e69e-4993-11ed-b878-0242ac120002}"));
const UID BLE_ME::uuid_fromNum   = UID(STR("{ed9da18c-a800-4f66-a670-aa7547e34453}"));

#ifdef Q_OS_ANDROID
BLE_ME::BLE_ME(QtAndroidService* service) : BLE(uuid_service), emitter(service) {
  service->ble = this;
  // forward signals defined in class BLE
  connect(this, &BLE::deviceDiscovered, emitter, &QtAndroidService::deviceDiscovered);
  connect(this, &BLE::bleError,         emitter, &QtAndroidService::bleError);
#else
BLE_ME::BLE_ME() : BLE(uuid_service), emitter(this) {
#endif
  connect(this, &BLE::mainServiceReady, this, &BLE_ME::ini);
  connect(this, &BLE::deviceDisconnecting, this, &BLE_ME::disconnecting);
}

bool BLE_ME::deviceFilter(const QBluetoothDeviceInfo& info) {
  return info.name().contains(filter, Qt::CaseInsensitive) &&
         (info.coreConfigurations() & QBluetoothDeviceInfo::LowEnergyCoreConfiguration);
}

void BLE_ME::ini() {
  connect(mainService, &QLowEnergyService::stateChanged,
          this, &BLE_ME::serviceStateChanged);
  connect(mainService, &QLowEnergyService::characteristicChanged,
          this, &BLE_ME::characteristicChanged);
  connect(mainService, &QLowEnergyService::characteristicRead,
          this, &BLE_ME::characteristicRead);
  connect(mainService, &QLowEnergyService::characteristicWritten,
          this, &BLE_ME::characteristicWritten);
  connect(mainService, QOverload<QLowEnergyService::ServiceError>::of(&QLowEnergyService::error),
          this, &BLE_ME::serviceError);

  connect(mainService, &QLowEnergyService::descriptorWritten,
          [](const QLowEnergyDescriptor&, const QByteArray& value) {
            qDebug() << "notifications changed:" << value;
          });

  if (mainService->state() == QLowEnergyService::DiscoveryRequired) {
    qDebug() << "discovering details...";
    mainService->discoverDetails();
  } else {
    searchCharacteristics();
  }
}

void BLE_ME::serviceStateChanged(QLowEnergyService::ServiceState state) {
  qDebug() << "service state changed:" << state;
  if (state == QLowEnergyService::ServiceDiscovered) {
    searchCharacteristics();
  }
}

void BLE_ME::searchCharacteristics() {
  qDebug() << "searching characteristics...";
  const auto characteristics = mainService->characteristics();
  for (auto ch : characteristics) {
    if (ch.isValid()) {
      if ((ch.properties() & QLowEnergyCharacteristic::Write) &&
          (ch.uuid() == uuid_toRadio)) {                         // toRadio
        toRadio = ch;
        qDebug() << "...found 'toRadio'";
      }
      if (ch.properties() & QLowEnergyCharacteristic::Read) {
        if (ch.uuid() == uuid_fromRadio) {                       // fromRadio
          fromRadio = ch;
          qDebug() << "...found 'fromRadio'";
        } else if (ch.uuid() == uuid_fromNum) {
          fromNum = ch;
          qDebug() << "...found 'fromNum'";                      // fromNum

          // enable notifications
          notifications = ch.descriptor(QBluetoothUuid::ClientCharacteristicConfiguration);
          if (notifications.isValid()) {
            mainService->writeDescriptor(notifications, QByteArray::fromHex("0100"));
            qDebug() << "notifications enabled";
          }
        }
      }
    }
  }

  if (toRadio.isValid() && fromRadio.isValid() && fromNum.isValid()) {
    QStringList names;
    for (auto device : qAsConst(devices)) {
      names << device.name().right(4);
    }
    if (!backgroundMode) {
      emitter->setReady(true, currentDevice.name().right(4), names);
    }
  }
}

void BLE_ME::characteristicChanged(const QLowEnergyCharacteristic&,
                                   const QByteArray& data) {
  if (!data.isEmpty()) {
    if (backgroundMode) {
      read();
    } else {
      emitter->receivedFromRadio(data, "notified");
    }
  }
}

void BLE_ME::characteristicRead(const QLowEnergyCharacteristic&,
                                const QByteArray& data) {
  if (data.isEmpty()) {
    if (!backgroundMode) {
      emitter->receivingDone();
    }
  } else {
    if (backgroundMode) {
      saveBytes(data);
    } else {
      emitter->receivedFromRadio(data, QString());
    }
    read();
  }
}

void BLE_ME::characteristicWritten(const QLowEnergyCharacteristic&,
                                   const QByteArray&) {
  read();
}

void BLE_ME::serviceError(QLowEnergyService::ServiceError error) {
  static QMetaEnum qme = mainService->metaObject()->enumerator(
                         mainService->metaObject()->indexOfEnumerator("ServiceError"));
  qDebug() << "service error:" << QLatin1String(qme.valueToKey(error));
}

// read/write

void BLE_ME::read() {
  if ((mainService != nullptr) && fromRadio.isValid()) {
    // will call 'characteristicRead()' on success
    mainService->readCharacteristic(fromRadio);
  } else {
    qDebug() << "not ready to read";
  }
}

void BLE_ME::write(const QByteArray& data) {
  if ((mainService != nullptr) && toRadio.isValid()) {
    // will call 'characteristicWritten()' on success
    mainService->writeCharacteristic(toRadio, data);
  } else {
    qDebug() << "not ready to write";
  }
}

// on device disconnect

void BLE_ME::disconnecting() {
  if ((mainService != nullptr) && notifications.isValid()) {
    // disable notifications
    mainService->writeDescriptor(notifications, QByteArray::fromHex("0000"));
  }
  if (!backgroundMode) {
    emitter->setReady(false, QString(), QStringList());
  }
  delete mainService; mainService = nullptr;
}

// background mode

void BLE_ME::setBackgroundMode(bool background) {
#if (defined Q_OS_ANDROID) || (defined Q_OS_IOS)
  backgroundMode = background;
  qDebug() << "background mode:" << backgroundMode;
  if (!backgroundMode) {
    sendSavedBytes();
  }
#endif
}

static QString packetsFile() {
  return QStandardPaths::writableLocation(QStandardPaths::DocumentsLocation) + "/meshtastic-packets.bin";
}

void BLE_ME::saveBytes(const QByteArray& packet) {
  QFile file(packetsFile());
  if (file.open(QIODevice::WriteOnly | QIODevice::Append)) {
    QDataStream ds(&file);
    ds << packet;
    file.close();
  }
}

void BLE_ME::sendSavedBytes() {
  QVariantList packets;
  QString fileName(packetsFile());
  QFile file(fileName);
  if (file.open(QIODevice::ReadOnly)) {
    QDataStream ds(&file);
    while (!ds.atEnd()) {
      QByteArray packet;
      ds >> packet;
      packets.append(packet);
    }
    file.close();
    if (!packets.isEmpty()) {
      emitter->sendSavedPackets(QVariant(packets));
      QFile::remove(fileName);
    }
  }
}
