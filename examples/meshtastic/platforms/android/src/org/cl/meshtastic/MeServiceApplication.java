package org.cl.meshtastic;

import android.content.Intent;
import android.os.Build;
import android.util.Log;

import org.qtproject.qt5.android.bindings.QtApplication;

public class MeServiceApplication extends QtApplication {

    @Override
    public void onCreate() {
        super.onCreate();

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            startForegroundService(new Intent(this, MeAndroidService.class));
        } else {
            startService(new Intent(this, MeAndroidService.class));
        }
    }
}
