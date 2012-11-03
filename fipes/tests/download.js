var fs    = require("fs");
var other = require('casper').create();

function fileTestCase(data) {
    casper.test.assertEqual(data.length, 365254, 'The downloaded file and the original have the same size');
}

casper.start('http://fipes.lo', function() {
    this.fill('.widget', {
        'files': 'Pipes_to_infinity.by_Ranjith_Siji.jpg'
    }, true);
});

casper.waitForSelector('.files > ul > li', function() {
    var workerPath = fs.absolute('./fipes/priv/worker.js');
    var worker = new Worker(workerPath);

    var url = this.evaluate(function() {
        return $('input[type=text]').val();
    });

    casper.test.comment('Downloading ' + url);
    worker.onmessage = function(event) { fileTestCase(event.data) };
    worker.postMessage(url);
});

casper.wait(1500, function() {
    // Waiting for the download to be completed;
    this.test.done();
});

casper.run(function() {});

