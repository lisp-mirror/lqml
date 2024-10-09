Name: harbour-sokoban
Summary: Sokoban game for Sailfish OS

Version: 0.1
Release: 1

Group: Amusements/Games
URL: https://gitlab.com/eql/lqml/-/tree/master/examples/sokoban
License: "It's complicated."

%description
Sokoban LQML Example for Sailfish OS.

# These are all installed in /opt/qt5 and not detected on RPM install.
%global __requires_exclude ^libQt5.*?\\.so.*$

%install
mkdir -p %{buildroot}%{_bindir}
mkdir -p %{buildroot}%{_libdir}
mkdir -p %{buildroot}%{_datadir}/applications
mkdir -p %{buildroot}%{_datadir}/icons/hicolor/128x128/apps
install -m 755 app %{buildroot}%{_bindir}/harbour-sokoban
install -m 644 ../../../../ecl/build/libecl.so.23.9.9 %{buildroot}%{_libdir}/libecl.so.23.9.9
install -m 644 harbour-sokoban.desktop %{buildroot}%{_datadir}/applications/harbour-sokoban.desktop
install -m 644 harbour-sokoban.png %{buildroot}%{_datadir}/icons/hicolor/128x128/apps/harbour-sokoban.png
strip %{buildroot}%{_bindir}/harbour-sokoban
strip %{buildroot}%{_libdir}/libecl.so.23.9.9
cd %{buildroot}%{_libdir}
ln -s libecl.so.23.9.9 libecl.so.23.9
ln -s libecl.so.23.9.9 libecl.so.23

%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig

%files
# XXX no idea how and why this is created
%exclude %{_libdir}/documentation.list
%{_bindir}/harbour-sokoban
%{_libdir}/libecl.so.23
%{_libdir}/libecl.so.23.9
%{_libdir}/libecl.so.23.9.9
%{_datadir}/applications/harbour-sokoban.desktop
%{_datadir}/icons/hicolor/128x128/apps/harbour-sokoban.png
