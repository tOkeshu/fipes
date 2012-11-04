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

