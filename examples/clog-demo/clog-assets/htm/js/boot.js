/*static version*/
var clog={};

if (typeof clog_debug == 'undefined') {
    clog_debug = false;
}

const ws = {
    send: function(message) {
        // hack: notify QML (see 'onTitleChanged()')
        document.title = message;
        document.title = "-"; // reset (not empty!)
    }
};

$( document ).ready(function() {
    var s = document.location.search;
    var tokens;
    var r = /[?&]?([^=]+)=([^&]*)/g;

    clog['body']=document.body;
    clog['head']=document.head;
    clog['documentElement']=document.documentElement;
    clog['window']=window;
    clog['navigator']=navigator;
    clog['document']=window.document;
    clog['location']=window.location;
});
