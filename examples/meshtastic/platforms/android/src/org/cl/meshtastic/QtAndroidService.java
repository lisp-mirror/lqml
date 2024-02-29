package org.cl.meshtastic;

import android.content.Context;
import android.app.NotificationManager;
import android.app.NotificationChannel;
import android.app.Notification;
import android.os.Build;
import android.util.Log;

import org.qtproject.qt5.android.bindings.QtService;

public class QtAndroidService extends QtService {

    @Override
    public void onCreate()
    {
        super.onCreate();

        Notification.Builder builder = null;
        NotificationManager manager = (NotificationManager)getSystemService(Context.NOTIFICATION_SERVICE);

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            NotificationChannel channel =
                new NotificationChannel("channel01",
                                        "Mesh SMS",
                                        NotificationManager.IMPORTANCE_LOW);
            channel.setShowBadge(false);
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
}
