package org.cl.meshtastic;

import android.content.Context;
import android.app.NotificationManager;
import android.app.NotificationChannel;
import android.app.Notification;
import android.hardware.usb.UsbDeviceConnection;
import android.hardware.usb.UsbManager;
import android.os.Build;
import android.util.Log;

import com.hoho.android.usbserial.driver.UsbSerialDriver;
import com.hoho.android.usbserial.driver.UsbSerialPort;
import com.hoho.android.usbserial.driver.UsbSerialProber;
import com.hoho.android.usbserial.util.SerialInputOutputManager;

import java.io.IOException;
import java.util.List;

import org.qtproject.qt5.android.bindings.QtService;

public class MeAndroidService extends QtService
{
    // USB

    public SerialSocket usbSocket;
    public SerialInputOutputManager ioManager;

    public static native void qtSetReady(boolean ready);
    public static native void qtOnNewData(byte[] data);

    // Service

    @Override
    public void onCreate() {
        super.onCreate();

        Notification.Builder builder = null;

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            NotificationChannel channel =
                new NotificationChannel("channel01",
                                        "Mesh SMS",
                                        NotificationManager.IMPORTANCE_LOW);
            channel.setShowBadge(false);
            NotificationManager manager =
                (NotificationManager)getSystemService(Context.NOTIFICATION_SERVICE);
            manager.createNotificationChannel(channel);
            builder = new Notification.Builder(this, "channel01");
        } else {
            builder = new Notification.Builder(this).setPriority(Notification.PRIORITY_LOW);
        }

        startForeground(888, builder.build());
    }

    @Override
    public void onDestroy() {
        super.onDestroy();

        stopForeground(true);
    }

    // USB

    private class SerialSocket implements SerialInputOutputManager.Listener
    {
        private UsbSerialPort serialPort;

        SerialSocket(UsbSerialPort serialPort) {
            this.serialPort = serialPort;
        }

        void write(byte[] data) {
            if (serialPort != null) {
                try {
                    serialPort.write(data, 1000);
                } catch (Exception e) {
                    Log.d("[USB]", Log.getStackTraceString(e));
                }
            }
        }

        @Override
        public void onNewData(byte[] data) {
            qtOnNewData(data);
        }

        @Override
        public void onRunError(Exception e) {
            Log.d("[USB]", Log.getStackTraceString(e));
        }
    }

    public void iniUsb() {
        UsbManager manager = (UsbManager) getSystemService(Context.USB_SERVICE);
        List<UsbSerialDriver> availableDrivers = UsbSerialProber.getDefaultProber().findAllDrivers(manager);
        if (availableDrivers.isEmpty()) {
            qtSetReady(false);
            return;
        }

        UsbSerialDriver driver = availableDrivers.get(0); // CdcAcmSerialDriver
        UsbDeviceConnection connection = manager.openDevice(driver.getDevice());
        if (connection == null) {
            qtSetReady(false);
            return;
        }

        UsbSerialPort port = driver.getPorts().get(0);
        try {
            port.open(connection);
            port.setParameters(115200, 8, UsbSerialPort.STOPBITS_1, UsbSerialPort.PARITY_NONE);
        } catch (Exception e) {}

        usbSocket = new SerialSocket(port);
        try {
            port.setDTR(true);
            port.setRTS(true);
        } catch (IOException e) {}

        ioManager = new SerialInputOutputManager(port, usbSocket);
        ioManager.setReadBufferSize(1024);
        ioManager.start();

        qtSetReady(true);
    }

    public void writeUsb(byte[] data) {
        if (usbSocket != null) {
            usbSocket.write(data);
        }
    }
}
