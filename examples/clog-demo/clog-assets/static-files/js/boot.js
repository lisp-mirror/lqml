var _ws=null;
var adr; var adrc;
var clog={};
var pingerid;
var retryid;
var s = document.location.search;
var tokens;
var r = /[?&]?([^=]+)=([^&]*)/g;
var ios = false;

clog['body']=document.body;
clog['head']=document.head;
clog['documentElement']=document.documentElement;
clog['window']=window;
clog['navigator']=navigator;
clog['document']=window.document;
clog['location']=window.location;

if (typeof clog_debug == 'undefined') {
    clog_debug = false;
}

const ws = {
    // replace 'ws.send()'
    send: function(message) {
        if (ios) {
            _ws.send(message);
        } else {
            // hack, see QML 'onTitleChanged()'
            document.title = message;
            document.title = "-"; // non empty
        }
    }
};

function Ping_ws() {
    if (_ws.readyState == 1) {
        _ws.send ('0');
    }
}

function Shutdown_ws(event) {
    if (_ws != null) {
	_ws.onerror = null;
	_ws.onclose = null;
	_ws.close ();
	_ws = null;
    }
    clearInterval (pingerid);
    if (clog['html_on_close'] != '') {
        $(document.body).html(clog['html_on_close']);
    }
}

function Setup_ws() {
    _ws.onmessage = function (event) {
        try {
            if (clog_debug == true) {
		console.log ('eval data = ' + event.data);
            }
            eval (event.data);
        } catch (e) {
            console.error (e.message);
        }
    }

    var rc = function (event) {
	console.log (event);
	clearInterval (retryid);
	_ws = null;
	_ws = new WebSocket (adr  + '?r=' + clog['connection_id']);
        _ws.onopen = function (event) {
            console.log ('reconnect successful');
            Setup_ws();
        }
        _ws.onclose = function (event) {
            console.log ('reconnect failure');
	    console.log (Date.now());
	    retryid = setInterval(function () {rc("Failed reconnect - trying again")}, 500);
        }
    }

    _ws.onerror = function (event) {
        console.log ('onerror: reconnect');
	rc("onerror - trying reconnect")
    }

    _ws.onclose = function (event) {
        if (event.code && event.code === 1000) {
            console.log("WebSocket connection got normal close from server. Don't reconnect.");
            Shutdown_ws(event);
        } else {
	    rc("onclose - trying reconnnect");
        }
    }
}

function Open_ws() {
    /*
    if (location.protocol == 'https:') {
	adr = 'wss://' + location.hostname;
    } else {
	adr = 'ws://' + location.hostname;
    }

    if (location.port != '') { adr = adr + ':' + location.port; }
    adr = adr + '/clog';
    */

    if (ios) {
        adr = 'ws://127.0.0.1:8080';

        if (clog['connection_id']) {
          adrc = adr  + '?r=' + clog['connection_id'];
        } else { adrc = adr }

        try {
            console.log ('connecting to ' + adrc);
            _ws = new WebSocket (adrc);
        } catch (e) {
            console.log ('trying again, connecting to ' + adrc);
            _ws = new WebSocket (adrc);
        }

        if (_ws != null) {
            _ws.onopen = function (event) {
                console.log ('connection successful');
                Setup_ws();
            }
            pingerid = setInterval (function () {Ping_ws ();}, 10000);
        } else {
            document.writeln ('If you are seeing this your browser or your connection to the internet is blocking websockets.');
        }
    }
}

$( document ).ready(function() {
    if (_ws == null) { Open_ws(); }
});
