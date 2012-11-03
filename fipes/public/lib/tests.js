var factories = {
    file: function(name, content) {
        var blob = new Blob([content], {type: 'plain/text'})
        blob.name = name;
        blob.content = content;
        return blob;
    },

    fileEvent: function() {
        var files = Array.prototype.slice.call(arguments);
        return { target: { files: files } };
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
    var event = factories.fileEvent(file);

    App.FipeView.selectFiles(event);

    setTimeout(function() {
        var url = $('.files > ul > li').find('input[type=text]').val();
        var xhr = factories.download(url);
        equal(xhr.response, file.content, 'The uploaded and downloaded files have the same size');
        equal(xhr.getResponseHeader('Content-Disposition'), factories.contentDisposition(file.name), 'The uploaded and downloaded file have the same name');
        equal(xhr.getResponseHeader('Transfer-Encoding'), 'chunked', 'The response was chunked');
        equal(xhr.getResponseHeader('Content-Type'), file.type, 'The response was the of same type than the uploaded file');

        start();
    }, 1000);
});
