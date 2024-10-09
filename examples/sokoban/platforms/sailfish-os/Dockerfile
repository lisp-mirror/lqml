# Dockerfile

FROM sfosbid:latest

WORKDIR /home/nemo

RUN git clone https://gitlab.com/embeddable-common-lisp/ecl.git ecl
RUN git clone https://gitlab.com/eql/lqml.git lqml

RUN sudo chown -R nemo:sailfish-system /home/nemo

# Build and install ECL in build environment.
WORKDIR /home/nemo/ecl
RUN git checkout 23.9.9
RUN sb2 -t SailfishOS-latest-aarch64 -m sdk-install -R zypper install --no-confirm gcc make
RUN sb2 -t SailfishOS-latest-aarch64 -m sdk-install -R ./configure
RUN sb2 -t SailfishOS-latest-aarch64 -m sdk-install -R make
RUN sb2 -t SailfishOS-latest-aarch64 -m sdk-install -R make install

# LQML
WORKDIR /home/nemo/lqml/src/build
RUN sb2 -t SailfishOS-latest-aarch64 -m sdk-install -R zypper addrepo --refresh https://repo.sailfishos.org/obs/sailfishos:/chum/4.6_aarch64/sailfishos:chum.repo
RUN sb2 -t SailfishOS-latest-aarch64 -m sdk-install -R sh -c 'echo "repo_gpgcheck = off" >> /etc/zypp/zypp.conf'
RUN sb2 -t SailfishOS-latest-aarch64 -m sdk-install -R zypper install --no-confirm opt-qt5-qtdeclarative-devel opt-qt5-qtconnectivity-devel opt-qt5-qtlocation-devel opt-qt5-qtmultimedia-devel opt-qt5-qtquickcontrols2-devel opt-qt5-qtsensors-devel
RUN sb2 -t SailfishOS-latest-aarch64 -m sdk-install -R /opt/qt5/bin/qmake ../lqml-lib.pro
RUN sb2 -t SailfishOS-latest-aarch64 -m sdk-install -R make

# Prepare Sokoban directory.
WORKDIR /home/nemo/lqml/examples
RUN sb2 -t SailfishOS-latest-aarch64 -m sdk-install -R ./copy.sh sokoban

# Build Sokoban.
WORKDIR /home/nemo/lqml/examples/sokoban/build
RUN rm -rf * .qmake.stash
RUN sb2 -t SailfishOS-latest-aarch64 -m sdk-install -R /opt/qt5/bin/qmake ..
RUN sb2 -t SailfishOS-latest-aarch64 -m sdk-install -R make

# Build RPM.
COPY platforms/img/logo.png ./harbour-sokoban.png
COPY platforms/sailfish-os/harbour-sokoban.desktop .
RUN mkdir -p rpm
COPY platforms/sailfish-os/harbour-sokoban.spec ./rpm/
RUN sudo chown -R nemo:sailfish-system .
RUN mb2 -t SailfishOS-latest-aarch64 build

CMD echo -e "In another terminal execute:\n    - 'docker cp ${HOSTNAME}:$(pwd)/RPMS/$(basename RPMS/*.rpm) .'\nto copy the RPM out of the container." && read -p "Then press <Enter> to terminate this container."
