(function() {

    App.Models.File = Backbone.Model.extend({

        initialize: function() {
            this.uploads = new App.Models.Uploads;
        },

        fullUrl: function() {
            var l = document.location;
            return l.protocol + '//' + l.host + this.url();
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

                var upload = that.uploads.get(e.downloader);
                if (upload) {
                    upload.set({seek: seek});
                } else {
                    that.uploads.add({
                        id: e.downloader,
                        seek: seek,
                        size: file.size
                    });
                }
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
                var upload = that.uploads.get(e.downloader);
                upload.set({seek: file.size});
            }
        }

    });

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

})();
