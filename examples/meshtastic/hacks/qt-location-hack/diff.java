64a65,70
> // for hack
> import android.location.LocationManager;
> import android.location.LocationListener;
> import android.location.LocationRequest;
> import android.util.Log;
> 
66a73,123
>     // hack
>     public double _position_lat_      = 0.0; 
>     public double _position_lon_      = 0.0; 
>     public double _position_alt_      = 0.0; 
>     public long   _position_time_     = 0;
>     private static final String LQML  = "[LQML]";
> 
>     // hack
>     public void iniLocation()
>     {
>         LocationListener mLocationListenerGPS = new LocationListener() {
>             @Override
>             public void onLocationChanged(android.location.Location location) {
>                 _position_lat_  = location.getLatitude();
>                 _position_lon_  = location.getLongitude();
>                 _position_alt_  = location.getAltitude();
>                 _position_time_ = location.getTime();
>                 //String msg = "lat: " + latitude + " lon: " + longitude;
>                 //Log.d(LQML, msg);
>             }
> 
>             @Override
>             public void onStatusChanged(String provider, int status, Bundle extras) {
>                 //Log.d(LQML, "GPS: onStatusChanged");
>             }
> 
>             @Override
>             public void onProviderEnabled(String provider) {
>                 //Log.d(LQML, "GPS: onProviderEnabled");
>             }
> 
>             @Override
>             public void onProviderDisabled(String provider) {
>                 //Log.d(LQML, "GPS: onProviderDisabled");
>             }
>         };
> 
>         try {
>             //Log.d(LQML, "ini GPS location...");
>             LocationManager mLocationManager = (LocationManager)getSystemService(Context.LOCATION_SERVICE);
>             mLocationManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 2000, 100, mLocationListenerGPS);
>             //Log.d(LQML, "ini GPS location OK");
>         }
>         catch (Exception e) {
>             Log.e(LQML, Log.getStackTraceString(e));
>         }
>     }
> 
