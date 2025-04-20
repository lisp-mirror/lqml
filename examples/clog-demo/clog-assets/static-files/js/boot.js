var ws=null;
var adr; var adrc;
var clog={};
var pingerid;
var retryid;
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

if (typeof clog_debug == 'undefined') {
    clog_debug = false;
}

function Ping_ws() {
    if (ws.readyState == 1) {
        ws.send ('0');
    }
}

function Shutdown_ws(event) {
    if (ws != null) {
	ws.onerror = null;
	ws.onclose = null;
	ws.close ();
	ws = null;
    }
    clearInterval (pingerid);
    if (clog['html_on_close'] != '') {
        $(document.body).html(clog['html_on_close']);
    }
}

function Setup_ws() {
    ws.onmessage = function (event) {
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
	ws = null;
	ws = new WebSocket (adr  + '?r=' + clog['connection_id']);
        ws.onopen = function (event) {
            console.log ('reconnect successful');
            Setup_ws();
        }
        ws.onclose = function (event) {
            console.log ('reconnect failure');
	    console.log (Date.now());
	    retryid = setInterval(function () {rc("Failed reconnect - trying again")}, 500);
        }
    }

    ws.onerror = function (event) {
        console.log ('onerror: reconnect');
	rc("onerror - trying reconnect")
    }

    ws.onclose = function (event) {
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

    adr = 'ws://127.0.0.1:8080';

    if (clog['connection_id']) {
      adrc = adr  + '?r=' + clog['connection_id'];
    } else { adrc = adr }

    try {
        console.log ('connecting to ' + adrc);
        ws = new WebSocket (adrc);
    } catch (e) {
        console.log ('trying again, connecting to ' + adrc);
        ws = new WebSocket (adrc);
    }

    if (ws != null) {
        ws.onopen = function (event) {
            console.log ('connection successful');
            Setup_ws();
        }
        pingerid = setInterval (function () {Ping_ws ();}, 10000);
    } else {
            document.writeln ('If you are seeing this your browser or your connection to the internet is blocking websockets.');
    }
}

$( document ).ready(function() {
    if (ws == null) { Open_ws(); }
});
