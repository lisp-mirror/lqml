#pragma once

#include <QQmlEngine>

class Engine : public QQmlEngine {
    Q_OBJECT
public:
    Engine(QObject* parent = nullptr) : QQmlEngine(parent) {}

    Q_INVOKABLE void clearCache() { clearComponentCache(); }
};
