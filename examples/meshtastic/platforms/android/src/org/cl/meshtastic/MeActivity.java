package org.cl.meshtastic;

import android.content.Context;
import android.content.Intent;
import android.location.LocationListener;
import android.location.LocationManager;
import android.location.LocationRequest;
import android.util.Log;

import org.qtproject.qt5.android.bindings.QtActivity;

public class MeActivity extends QtActivity
{
    public static native void qtUsbDeviceAttached();

    // GPS

    // location hack: needed to not lose initial position with non moving
    // devices (on android Qt seems not to capture inital location values)

    public double position_lat  = 0.0; 
    public double position_lon  = 0.0; 
    public double position_alt  = 0.0; 
    public long   position_time = 0;

    public void iniLocation() {
        LocationListener mLocationListenerGPS = new LocationListener() {
            @Override
            public void onLocationChanged(android.location.Location location) {
                position_lat  = location.getLatitude();
                position_lon  = location.getLongitude();
                position_alt  = location.getAltitude();
                position_time = location.getTime();
            }
        };

        try {
            LocationManager mLocationManager = (LocationManager)getSystemService(Context.LOCATION_SERVICE);
            mLocationManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 2000, 0, mLocationListenerGPS);
        }
        catch (Exception e) {
            Log.e("[GPS]", Log.getStackTraceString(e));
        }
    }

    // USB

    @Override
    protected void onNewIntent(Intent intent) {
        if ("android.hardware.usb.action.USB_DEVICE_ATTACHED".equals(intent.getAction())) {
            qtUsbDeviceAttached();
        }
    }
}
