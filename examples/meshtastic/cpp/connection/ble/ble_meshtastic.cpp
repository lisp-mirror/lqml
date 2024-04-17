#include "ble_meshtastic.h"
#include "../connection.h"
#include <QMetaEnum>

#ifdef Q_OS_ANDROID
  #include "../../android_service/qtandroidservice_ro.h"
#endif

// service
const UID BLE_ME::uuid_service   = UID(STR("{6ba1b218-15a8-461f-9fa8-5dcae273eafd}"));

// characteristics
const UID BLE_ME::uuid_toRadio   = UID(STR("{f75c76d2-129e-4dad-a1dd-7866124401e7}"));
const UID BLE_ME::uuid_fromRadio = UID(STR("{2c55e69e-4993-11ed-b878-0242ac120002}"));
const UID BLE_ME::uuid_fromNum   = UID(STR("{ed9da18c-a800-4f66-a670-aa7547e34453}"));

#ifdef Q_OS_ANDROID
BLE_ME::BLE_ME(QtAndroidService* service, Connection* _con) : BLE(uuid_service), emitter(service), con(_con) {
  // forward signals defined in class BLE
  connect(this, &BLE::deviceDiscovered, service, &QtAndroidService::deviceDiscovered);
  connect(this, &BLE::bleError,         service, &QtAndroidService::bleError);
#else
BLE_ME::BLE_ME(Connection* _con) : BLE(uuid_service), emitter(_con), con(_con) {
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
#if QT_VERSION < 0x060000
  connect(mainService, QOverload<QLowEnergyService::ServiceError>::of(&QLowEnergyService::error),
          this, &BLE_ME::serviceError);
#else
  connect(mainService, &QLowEnergyService::errorOccurred, this, &BLE_ME::serviceError);
#endif

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
#if QT_VERSION < 0x060000
          notifications = ch.descriptor(QBluetoothUuid::ClientCharacteristicConfiguration);
#else
          notifications = ch.descriptor(QBluetoothUuid::DescriptorType::ClientCharacteristicConfiguration);
#endif
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
    if (!con->backgroundMode) {
      QVariantList vNames;
      for (auto name : qAsConst(names)) { vNames << name; }
      emitter->setReady(QVariant(QVariantList() << true << currentDevice.name().right(4) << vNames));
    }
  }
}

void BLE_ME::characteristicChanged(const QLowEnergyCharacteristic&,
                                   const QByteArray& data) {
  if (!data.isEmpty()) {
    if (con->backgroundMode) {
      read();
    } else {
      emitter->receivedFromRadio(QVariant(QVariantList() << data << QString("notified")));
    }
  }
}

void BLE_ME::characteristicRead(const QLowEnergyCharacteristic&,
                                const QByteArray& data) {
  if (data.isEmpty()) {
    if (!con->backgroundMode) {
      emitter->receivingDone();
      static bool startup = true;
      if (startup) {
        con->sendSavedBytes(); // for eventual, saved but not sent packets
      } else {
        startup = false;
      }
    }
  } else {
    if (con->backgroundMode) {
      con->saveBytes(data);
    } else {
      emitter->receivedFromRadio(QVariant(QVariantList() << data));
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
  if (!con->backgroundMode) {
    emitter->setReady(QVariant(QVariantList()));
  }
  delete mainService; mainService = nullptr;
}

