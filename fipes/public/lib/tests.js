// FIXME: non atomic tests, uh ?
QUnit.config.reorder = false;

var factories = {
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
}

test('Offer a file then download it', function() {
    stop();

    var file = factories.file('Tiny file', 'Tiny file is tiny');
    factories.offer(file);

    setTimeout(function() {
        var url = $('.files > ul > li').find('input[type=text]').val();
        var xhr = factories.download(url);
        equal(xhr.response, file.content, 'The offered and downloaded files have the same size');
        equal(xhr.getResponseHeader('Content-Disposition'), factories.contentDisposition(file.name), 'The offered and downloaded file have the same name');
        equal(xhr.getResponseHeader('Transfer-Encoding'), 'chunked', 'The response was chunked');
        // FIXME: should fail. Checkout fipes/src/fipes_files.erl to fix that.
        equal(xhr.getResponseHeader('Content-Type'), file.type, 'The response was the of same type than the offered file');

        start();
    }, 1000);
});

test('Offer multiple files', function() {
    stop();

    var files = [factories.file('1st', 'First file'),
                 factories.file('2nd', 'Second file')];
    factories.offer(files);

    setTimeout(function() {
        equal($('.files > ul > li').length, 3, 'There is 3 offered files');

        start();
    }, 1000);
});

test('Someone else offers a few files then leaves', function() {
    stop();

    var files = [factories.file('Another', 'Another file'),
                 factories.file('And again', 'And another again')];
    var offer = factories.offerAsSomeoneElse(files);

    setTimeout(function() {
        equal($('.files > ul > li').length, 5, 'There is 5 offered files');
        offer.stop();

        setTimeout(function() {
            equal($('.files > ul > li').length, 3, 'There is now 3 offered files');
            start();
        }, 1000);
    }, 1000);
});

