window.factories = {
    file: function(name, content) {
        var blob = new Blob([content], {type: 'plain/text'})
        blob.name = name;
        blob.content = content;
        return blob;
    },

    fileEvent: function(files) {
        return { target: { files: files } };
    },

    offer: function(files) {
        if (!_.isArray(files))
            files = [files]

        var event = factories.fileEvent(files);
        App.FipeView.selectFiles(event);
    },

    offerAsSomeoneElse: function(files) {
        if (!_.isArray(files))
            files = [files]

        var uri = "ws://" + location.host + ':3473' + App.Fipe.url();
        var ws  = new WebSocket(uri);
        var xhr = new XMLHttpRequest;

        ws.onmessage = function(evt) {
            var event = tnetstrings.parse(evt.data).value;

            switch (event.type) {
            case "uid":
                _.each(files, function(file) {
                    factories.registerFile(event.uid, file, xhr);
                });
            }
        }

        return factories.remoteOffer(xhr, ws);
    },

    registerFile: function(uid, file, xhr) {
        var payload = tnetstrings.dump({
            id:    'fake',
            name:  file.name,
            type:  file.type,
            size:  file.size,
            owner: uid
        });

        xhr.open('POST', App.Fipe.url() + '/files', false);
        xhr.send(payload);
    },

    remoteOffer: function(xhr, ws) {
        return {
            xhr: xhr,
            ws: ws,
            stop: function() {
                ws.close();
            }
        };
    },

    download: function(url) {
        var xhr = new XMLHttpRequest();
        xhr.open('GET', url, false);
        xhr.send();
        return xhr;
    },

    contentDisposition: function(name) {
        return 'attachment; filename="' + name + '"';
    }
};

