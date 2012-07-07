(function() {

    App.Models.Files = Backbone.Collection.extend({
        model: App.Models.File,

        url: function() {
            // TODO: throw a comprehensive error when the fipe was not
            // attached.
            return this.fipe.url() + '/files';
        },

        save: function() {
            // Save files, only when there is an attached fipe.
            if (this.fipe) {
                this.each(function(file) {
                    if (file.isNew()) file.save();
                });
            }
        }
    });

    App.Models.File = Backbone.Model.extend({

        fullUrl: function() {
            return document.location.protocol + '//' + document.domain + this.url();
        },

        toJSON: function() {
            return {
                owner : App.UID,
                name  : this.get('name'),
                type  : this.get('type'),
                size  : this.get('size')
            };
        },

        sendChunk: function(ws, e) {
            var that   = this;
            var file   = this.get('obj');
            var reader = new FileReader;
            var seek   = e.seek;
            var slice  = 1024 * 512; // 512 KB

            // Make a portable slice method.
            if (file.slice == undefined) {
                if (file.webkitSlice) var method = file.webkitSlice;
                if (file.mozSlice) var method = file.mozSlice;
                file.slice = _.bind(method, file);
            }

            reader.onload = function(evt) {
                var data = btoa(evt.target.result);
                var event = tnetstrings.dump({
                    type       : "chunk",
                    payload    : data,
                    downloader : e.downloader
                });
                ws.send(event);

            }

            // Stream the file
            if (seek < file.size) {
                var blob = file.slice(seek, seek + slice);
                reader.readAsBinaryString(blob);
            // Stop the stream
            } else {
                var eos = tnetstrings.dump({
                    type: "eos",
                    downloader: e.downloader
                });
                ws.send(eos);
            }
        }

    });

})();
