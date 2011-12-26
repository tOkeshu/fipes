(function() {

    App.Models.Fipe = Backbone.Model.extend({
        urlRoot: '/fipes',

        open: function(callback) {
            // FIXME: use the line below instead of the ugly
            // one. Before that, you'll need to find how to use
            // websockets with Nginx as a reverse proxy.
            //
            // var uri = "ws://" + location.host + this.url();
            var uri = 'ws://localhost:3473' + this.url();

            if ("MozWebSocket" in window) {
                WebSocket = MozWebSocket;
            }

            if ("WebSocket" in window) {
                this.ws = new WebSocket(uri);
            } else {
                throw new Error('Your browser does not support websockets');
            }

            // Code taken from cowboy_examples which was taken from
            // misultin.
            var that = this;
            this.ws.onopen = function() {
                // websocket is connected
                console.log("websocket connected!");
                // send hello data to server.
                that.ws.send("hello server!");
                console.log("sent message to server: 'hello server'!");
            };
            this.ws.onmessage = function (evt) {
                var event = tnetstrings.parse(evt.data).value;

                if (callback) callback(event.uid);

                var receivedMsg = evt.data;
                console.log("server sent the following: '" + receivedMsg + "'");
            };
            this.ws.onclose = function() {
                // websocket was closed
                console.log("websocket was closed");
            };
        }
    });

})();
