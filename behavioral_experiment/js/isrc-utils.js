/**
 * isrcUtils
 */
var isrcUtils = {


    /**
     * URL of server where to post the data at the end of the experiment
     */
    serverURI: "../MultiArmedBandit_Explore/wbsrv-comm/write_data_Bandit.php",


    /**
     * Properties
     */
    buttons: [],
    preloadedImages: [],
    initialTimeStamp: null,
    data: {
        'uid': 0,
        'duration': 0,
        'date': null,
    },


    /**
     * Cordova: initialize()
     */
    initialize: function () {
        //document.addEventListener('ready', this.onDeviceReady.bind(this), false);
        window.addEventListener('load', this.onDeviceReady.bind(this), false);
    },


    /**
     * Cordova: onDeviceReady()
     */
    onDeviceReady: function () {
        this.receivedEvent('deviceready');
    },


    /**
     * Cordova: receivedEvent()
     */
    receivedEvent: function (id) {
        switch (id) {
            case 'deviceready':
                this.Setup();
                isrcUtils.SetHandlers();
                break;
        }
    },


    /**
     * Setup()
     */
    Setup: function () {

        // Routes setup
        var routes = document.querySelectorAll('div.route');
        for (var i = 0; i < routes.length; ++i) {
            routes[i].style.display = 'none';
        }
        var route = document.getElementById('rt-start');
        if (route) route.style.display = 'block';

        // Routes buttons setup
        var buttons = document.querySelectorAll('button[data-href]'),
            i;
        for (i = 0; i < buttons.length; ++i) {
            buttons[i].addEventListener('click', isrcUtils.ButtonGoto);
        }

        // skip login if testing session data is in URL
        if (isrcUtils.GetUriParam('uid') !== null) {
            isrcUtils.data.uid = isrcUtils.GetUriParam('uid');
            console.log('[debug] uid:', isrcUtils.data.uid)
            isrcUtils.initialTimeStamp = new Date();
            document.getElementById('rt-start').style.display = 'none';

            // Show exp registration page if age is missing from URL
            if (isrcUtils.GetUriParam('age') == null) {
                document.getElementById('page1').style.display = 'block';
            }
        }

    },


    /**
     * SetHandlers()
     */
    SetHandlers: function () {
        isrcUtils.buttons[0] = document.getElementById('btn-initial-form');
        if (isrcUtils.buttons[0])
            isrcUtils.buttons[0].addEventListener('click', isrcUtils.InitialFormSubmit);

        isrcUtils.buttons[1] = document.getElementById('btn-app-restart');
        if (isrcUtils.buttons[1])
            isrcUtils.buttons[1].addEventListener('click', isrcUtils.Restart);

        isrcUtils.buttons[2] = document.getElementById('stats-records');
        if (isrcUtils.buttons[2])
            isrcUtils.buttons[2].addEventListener('click', isrcUtils.SaveDataToFileDialog);
    },


    /**
     * InitialFormSubmit()
     */
    InitialFormSubmit: function () {
        var input = document.getElementById('uid-input');
        if (input.value == null || input.value == "") return;

        isrcUtils.data.uid = input.value;
        isrcUtils.initialTimeStamp = new Date();

        var topbarUid = document.getElementById('user-id');
        if (topbarUid) topbarUid.innerHTML = 'User ID: ' + isrcUtils.data.uid;

        isrcUtils.Goto('page1');
    },


    /**
     * ButtonGoto()
     */
    ButtonGoto: function (evt) {
        var href = evt.target.getAttribute('data-href');
        isrcUtils.Goto(href);
        return true;
    },


    /**
     * Goto()
     */
    Goto: function (href) {
        var route = document.getElementById(href);
        if (route != null) {
            var routes = document.querySelectorAll('div.route'),
                i;
            for (i = 0; i < routes.length; ++i) {
                routes[i].style.display = 'none';
            }
            route.style.display = 'block';
        }
    },



    /**
     * ArrayShuffle()
     */
    ArrayShuffle: function (array) {
        var currentIndex = array.length,
            temporaryValue, randomIndex;

        // While there remain elements to shuffle...
        while (0 !== currentIndex) {

            // Pick a remaining element...
            randomIndex = Math.floor(Math.random() * currentIndex);
            currentIndex -= 1;

            // And swap it with the current element.
            temporaryValue = array[currentIndex];
            array[currentIndex] = array[randomIndex];
            array[randomIndex] = temporaryValue;
        }

        return array;
    },


    /**
     * ImagesPreload()
     */
    PreloadImages: function (images) {
        var baseIndex = isrcUtils.preloadedImages.length - 1;
        for (var i = 0; i < images.length; i++) {
            isrcUtils.preloadedImages[baseIndex + i] = new Image();
            isrcUtils.preloadedImages[baseIndex + i].src = images[i];
        }
    },

    /**
     * SaveAndEnd()
     */
    SaveAndEnd: function (stimuliData) {
        // merge data from stimuli
        isrcUtils.data = isrcUtils.ExtendObject(isrcUtils.data, stimuliData);
        isrcUtils.Goto('rt-end');
        isrcUtils.SaveData();
    },


    /**
     * Restart()
     */
    Restart: function () {
        location.reload();
    },


    /**
     * SaveData()
     */
    SaveData: function() {
		
		
		isrcUtils.Post(
			isrcUtils.serverURI,
			{"data": JSON.stringify(isrcUtils.data)},
			function(msg) {console.log("data saved ", msg)},
			function(msg) {console.log("data save error", msg)}
		)
		return;
		
		
	// i dont need any of this down here.

		var xhr = new XMLHttpRequest();
			xhr.open('POST', './wbsrv-comm/write_data_Bandit.php'); // change 'write_data.php' to point to php script.

		xhr.setRequestHeader('Content-Type', 'application/json');
  
		xhr.onload = function() {
    
			if(xhr.status == 200){
				try{
					console.log(xhr.responseText)
					var response = JSON.parse(xhr.responseText);
				}
				catch(e){
					var div = document.createElement("div");
					div.setAttribute("style","color:red;");
					var textnode = document.createTextNode("Error occured while saving to the remote database! <br />Make sure to download this file otherwise all the data is lost!");
					div.appendChild(textnode);                              // Append the text to <li>
					var body = document.getElementsByTagName("body")[0];
					body.appendChild(div);
    		
					console.log("Error occured:" + e.toString());
				//	jsPsych.data.get().localSave('json', 'Gamble.json'); // here i need to look into before i really continue
				}
			}
		};
		var json = {"data":JSON.stringify(isrcUtils.data)}
		
		xhr.send({data: JSON.stringify(isrcUtils.data)});
	},

//JSON.stringify(isrcUtils.data)}
    /**
     * GetUriParam()
     */
    GetUriParam(prop) {
        var params = {};
        var search = decodeURIComponent( window.location.href.slice( window.location.href.indexOf( '?' ) + 1 ) );
        var definitions = search.split( '&' );
    
        definitions.forEach( function( val, key ) {
            var parts = val.split( '=', 2 );
            params[ parts[ 0 ] ] = parts[ 1 ];
        } );
    
        return ( prop && prop in params ) ? params[ prop ] : null;
    },


    /**
     * Post()
     */
    Post(url, data, success, fail) {
        var params = typeof data == 'string' ? data : Object.keys(data).map(
                function(k){ return encodeURIComponent(k) + '=' + encodeURIComponent(data[k]) }
            ).join('&');
    
        var xhr = window.XMLHttpRequest ? new XMLHttpRequest() : new ActiveXObject("Microsoft.XMLHTTP");
        xhr.open('POST', url);
        xhr.onreadystatechange = function() {
            if (xhr.readyState>3 && xhr.status==200) { success(xhr.responseText); }
            else { fail(xhr.responseText); }
        };
        xhr.setRequestHeader('X-Requested-With', 'XMLHttpRequest');
        xhr.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
        xhr.send(params);
        return xhr;
    },


    /**
     * ExtendObject()
     */
    ExtendObject: function (obj, src) {
        for (var key in src) {
            if (src.hasOwnProperty(key)) obj[key] = src[key];
        }
        return obj;
    },


    /**
     * RequestConfirmation()
     */
    RequestConfirmation: function (handlerFunction, evt) {
        var event = evt;
        var handler = handlerFunction;
        navigator.notification.confirm(
            'Bist du sicher?',
            function (buttonIndex) {
                if (buttonIndex === 2) return;
                return handler(evt);
            },
            'Bestätigen',
            ['ja', 'nein'] 
        );
    },


}
isrcUtils.initialize();