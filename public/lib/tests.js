// FIXME: non atomic tests, uh ?
QUnit.config.reorder = false;

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

