(function() {

    App.Models.Fipe = Backbone.Model.extend({
        urlRoot: '/fipes',

        open: function(callback) {
            // FIXME: Remove this ugly uri. Before that, you'll need
            // to find how to use websockets with Nginx as a reverse
            // proxy.
            var uri = "wss://" + location.host + this.url();

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
            };
            this.ws.onmessage = function (evt) {
                var event = tnetstrings.parse(evt.data).value;

                switch (event.type) {
                case "uid":
                    if (callback) callback(event.uid);
                    break;
                case "stream":
                    var file = App.Files.get(event.file);
                    file.sendChunk(that.ws, event);
                    break;
                case "file.new":
                    // Someone offers a new file.
                    if (event.file.owner != App.UID) {
                        App.Files.add(event.file);
                    }
                    break;
                case "file.remove":
                    // Someone removed a file.
                    if (event.file.owner != App.UID) {
                        var file = App.Files.get(event.file.id);
                        App.Files.remove(file);
                    }
                    break;
                }

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
