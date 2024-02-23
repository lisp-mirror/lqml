#ifndef REP_QTANDROIDSERVICE_SOURCE_H
#define REP_QTANDROIDSERVICE_SOURCE_H

// This is an autogenerated file.
// Do not edit this file, any changes made will be lost the next time it is generated.

#include <QtCore/qobject.h>
#include <QtCore/qdatastream.h>
#include <QtCore/qvariant.h>
#include <QtCore/qmetatype.h>

#include <QtRemoteObjects/qremoteobjectnode.h>
#include <QtRemoteObjects/qremoteobjectsource.h>


class QtAndroidServiceSource : public QObject
{
    Q_OBJECT
    Q_CLASSINFO(QCLASSINFO_REMOTEOBJECT_TYPE, "QtAndroidService")
    Q_CLASSINFO(QCLASSINFO_REMOTEOBJECT_SIGNATURE, "5239a8ec9c5c20228d0b3b90f08230011762e23c")

public:
    explicit QtAndroidServiceSource(QObject *parent = nullptr) : QObject(parent)
    {
    }

public:
    ~QtAndroidServiceSource() override = default;


Q_SIGNALS:
    void deviceDiscovered(const QString & __repc_variable_1);
    void bleError();
    void setReady(bool __repc_variable_1, const QString & __repc_variable_2, const QStringList & __repc_variable_3);
    void receivedFromRadio(const QByteArray & __repc_variable_1, const QString & __repc_variable_2);
    void receivingDone();

public Q_SLOTS:
    virtual void startDeviceDiscovery(const QString & __repc_variable_1) = 0;
    virtual void setDeviceFilter(const QString & __repc_variable_1) = 0;
    virtual void read() = 0;
    virtual void write(const QByteArray & __repc_variable_1) = 0;

private:
    friend class QT_PREPEND_NAMESPACE(QRemoteObjectNode);
};


class QtAndroidServiceSimpleSource : public QtAndroidServiceSource
{
    Q_OBJECT

public:
    explicit QtAndroidServiceSimpleSource(QObject *parent = nullptr) : QtAndroidServiceSource(parent)
    {
    }

public:
    ~QtAndroidServiceSimpleSource() override = default;


private:
};


template <class ObjectType>
struct QtAndroidServiceSourceAPI : public SourceApiMap
{
    QtAndroidServiceSourceAPI(ObjectType *object, const QString &name = QLatin1String("QtAndroidService"))
        : SourceApiMap(), m_name(name)
    {
        Q_UNUSED(object);
        m_enums[0] = 0;
        m_properties[0] = 0;
        m_signals[0] = 5;
        m_signals[1] = QtPrivate::qtro_signal_index<ObjectType>(&ObjectType::deviceDiscovered, static_cast<void (QObject::*)(QString)>(0),m_signalArgCount+0,&m_signalArgTypes[0]);
        m_signals[2] = QtPrivate::qtro_signal_index<ObjectType>(&ObjectType::bleError, static_cast<void (QObject::*)()>(0),m_signalArgCount+1,&m_signalArgTypes[1]);
        m_signals[3] = QtPrivate::qtro_signal_index<ObjectType>(&ObjectType::setReady, static_cast<void (QObject::*)(bool,QString,QStringList)>(0),m_signalArgCount+2,&m_signalArgTypes[2]);
        m_signals[4] = QtPrivate::qtro_signal_index<ObjectType>(&ObjectType::receivedFromRadio, static_cast<void (QObject::*)(QByteArray,QString)>(0),m_signalArgCount+3,&m_signalArgTypes[3]);
        m_signals[5] = QtPrivate::qtro_signal_index<ObjectType>(&ObjectType::receivingDone, static_cast<void (QObject::*)()>(0),m_signalArgCount+4,&m_signalArgTypes[4]);
        m_methods[0] = 4;
        m_methods[1] = QtPrivate::qtro_method_index<ObjectType>(&ObjectType::startDeviceDiscovery, static_cast<void (QObject::*)(QString)>(0),"startDeviceDiscovery(QString)",m_methodArgCount+0,&m_methodArgTypes[0]);
        m_methods[2] = QtPrivate::qtro_method_index<ObjectType>(&ObjectType::setDeviceFilter, static_cast<void (QObject::*)(QString)>(0),"setDeviceFilter(QString)",m_methodArgCount+1,&m_methodArgTypes[1]);
        m_methods[3] = QtPrivate::qtro_method_index<ObjectType>(&ObjectType::read, static_cast<void (QObject::*)()>(0),"read()",m_methodArgCount+2,&m_methodArgTypes[2]);
        m_methods[4] = QtPrivate::qtro_method_index<ObjectType>(&ObjectType::write, static_cast<void (QObject::*)(QByteArray)>(0),"write(QByteArray)",m_methodArgCount+3,&m_methodArgTypes[3]);
    }

    QString name() const override { return m_name; }
    QString typeName() const override { return QStringLiteral("QtAndroidService"); }
    int enumCount() const override { return m_enums[0]; }
    int propertyCount() const override { return m_properties[0]; }
    int signalCount() const override { return m_signals[0]; }
    int methodCount() const override { return m_methods[0]; }
    int sourceEnumIndex(int index) const override
    {
        if (index < 0 || index >= m_enums[0])
            return -1;
        return m_enums[index+1];
    }
    int sourcePropertyIndex(int index) const override
    {
        if (index < 0 || index >= m_properties[0])
            return -1;
        return m_properties[index+1];
    }
    int sourceSignalIndex(int index) const override
    {
        if (index < 0 || index >= m_signals[0])
            return -1;
        return m_signals[index+1];
    }
    int sourceMethodIndex(int index) const override
    {
        if (index < 0 || index >= m_methods[0])
            return -1;
        return m_methods[index+1];
    }
    int signalParameterCount(int index) const override
    {
        if (index < 0 || index >= m_signals[0])
            return -1;
        return m_signalArgCount[index];
    }
    int signalParameterType(int sigIndex, int paramIndex) const override
    {
        if (sigIndex < 0 || sigIndex >= m_signals[0] || paramIndex < 0 || paramIndex >= m_signalArgCount[sigIndex])
            return -1;
        return m_signalArgTypes[sigIndex][paramIndex];
    }
    int methodParameterCount(int index) const override
    {
        if (index < 0 || index >= m_methods[0])
            return -1;
        return m_methodArgCount[index];
    }
    int methodParameterType(int methodIndex, int paramIndex) const override
    {
        if (methodIndex < 0 || methodIndex >= m_methods[0] || paramIndex < 0 || paramIndex >= m_methodArgCount[methodIndex])
            return -1;
        return m_methodArgTypes[methodIndex][paramIndex];
    }
    int propertyIndexFromSignal(int index) const override
    {
        Q_UNUSED(index);
        return -1;
    }
    int propertyRawIndexFromSignal(int index) const override
    {
        Q_UNUSED(index);
        return -1;
    }
    const QByteArray signalSignature(int index) const override
    {
        switch (index) {
        case 0: return QByteArrayLiteral("deviceDiscovered(QString)");
        case 1: return QByteArrayLiteral("bleError()");
        case 2: return QByteArrayLiteral("setReady(bool,QString,QStringList)");
        case 3: return QByteArrayLiteral("receivedFromRadio(QByteArray,QString)");
        case 4: return QByteArrayLiteral("receivingDone()");
        }
        return QByteArrayLiteral("");
    }
    QList<QByteArray> signalParameterNames(int index) const override
    {
        if (index < 0 || index >= m_signals[0])
            return QList<QByteArray>();
        return ObjectType::staticMetaObject.method(m_signals[index + 1]).parameterNames();
    }
    const QByteArray methodSignature(int index) const override
    {
        switch (index) {
        case 0: return QByteArrayLiteral("startDeviceDiscovery(QString)");
        case 1: return QByteArrayLiteral("setDeviceFilter(QString)");
        case 2: return QByteArrayLiteral("read()");
        case 3: return QByteArrayLiteral("write(QByteArray)");
        }
        return QByteArrayLiteral("");
    }
    QMetaMethod::MethodType methodType(int) const override
    {
        return QMetaMethod::Slot;
    }
    QList<QByteArray> methodParameterNames(int index) const override
    {
        if (index < 0 || index >= m_methods[0])
            return QList<QByteArray>();
        return ObjectType::staticMetaObject.method(m_methods[index + 1]).parameterNames();
    }
    const QByteArray typeName(int index) const override
    {
        switch (index) {
        case 0: return QByteArrayLiteral("void");
        case 1: return QByteArrayLiteral("void");
        case 2: return QByteArrayLiteral("void");
        case 3: return QByteArrayLiteral("void");
        }
        return QByteArrayLiteral("");
    }
    QByteArray objectSignature() const override { return QByteArray{"5239a8ec9c5c20228d0b3b90f08230011762e23c"}; }

    int m_enums[1];
    int m_properties[1];
    int m_signals[6];
    int m_methods[5];
    const QString m_name;
    int m_signalArgCount[5];
    const int* m_signalArgTypes[5];
    int m_methodArgCount[4];
    const int* m_methodArgTypes[4];
};

QT_BEGIN_NAMESPACE
QT_END_NAMESPACE


#endif // REP_QTANDROIDSERVICE_SOURCE_H
